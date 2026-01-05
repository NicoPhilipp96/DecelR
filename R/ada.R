#' Acceleration-to-Deceleration Assessment (ADA)
#'
#' Analyze time–speed (and optional position/force) data for acceleration/deceleration metrics.
#' Acceleration is derived from filtered speed-time (dv/dt), not from a provided acceleration column.
#'
#' @param data A data.frame with columns: time (s), speed (m/s); optional force (N), optional position (m)
#' @param cutoff_hz Numeric. Butterworth low-pass cutoff frequency in Hz. Default = 3.
#' @param filter_order Integer. Butterworth filter order. Default = 2.
#' @param start_method Either "acc_threshold" (start when derived acceleration ≤ acc_threshold) or "peak_speed".
#' @param acc_threshold Numeric. Start-of-deceleration threshold (m/s²). Default = -1.5.
#' @param stop_speed Numeric. Speed threshold for end of deceleration (m/s). Default = 0.2.
#' @param trim_speed Numeric. Speed threshold to trim standing phase (m/s). Default = 0.5.
#'
#' @return A list with:
#' \itemize{
#'   \item data: tibble with filtered columns (`speed_f`, derived `acc_f`, optional `force_f`, `position_f`)
#'   \item indices: list with `i_peak`, `i_start`, `i_stop`
#'   \item metrics: tibble of ADA metrics
#'   \item plot_time: ggplot object of speed–time with thresholds shown
#'   \item plot_pos: ggplot object of speed–position with thresholds shown (if position exists or is derived)
#' }
#' @export
ada_analyze <- function(
    data,
    cutoff_hz = 3,
    filter_order = 2,
    start_method = c("acc_threshold", "peak_speed"),
    acc_threshold = -1.5,
    stop_speed = 0.2,
    trim_speed = 0.5
) {
  requireNamespace("dplyr"); requireNamespace("signal"); requireNamespace("ggplot2")

  start_method <- match.arg(start_method)

  # --- inputs & basic cleaning
  required <- c("time", "speed")
  miss <- setdiff(required, names(data))
  if (length(miss)) stop("Missing required columns: ", paste(miss, collapse = ", "))

  df <- dplyr::as_tibble(data) |>
    dplyr::mutate(
      time = as.numeric(.data$time),
      speed = abs(as.numeric(.data$speed)),
      force = if ("force" %in% names(data)) as.numeric(.data$force) else NA_real_,
      position = if ("position" %in% names(data)) as.numeric(.data$position) else NA_real_
    ) |>
    dplyr::arrange(.data$time)

  # Drop rows with missing essential values
  df <- df |>
    dplyr::filter(is.finite(.data$time), is.finite(.data$speed))

  if (nrow(df) < 5) stop("Not enough valid rows after cleaning.")

  # --- auto-trim standing phase and zero time
  if (any(df$speed > trim_speed, na.rm = TRUE)) {
    start_i <- which(df$speed > trim_speed)[1]
    df <- df[start_i:nrow(df), , drop = FALSE]
  }
  df$time <- df$time - df$time[1]

  # --- sampling & filter
  dt <- median(diff(df$time), na.rm = TRUE)
  if (!is.finite(dt) || dt <= 0) stop("Cannot infer sampling rate from `time`.")
  fs <- 1 / dt
  wn <- cutoff_hz / (fs / 2)
  if (!is.finite(wn) || wn <= 0 || wn >= 1) stop("cutoff_hz too high/low for sampling rate (Nyquist limit).")

  bf <- signal::butter(n = filter_order, W = wn, type = "low")

  df <- dplyr::mutate(
    df,
    speed_f = as.numeric(signal::filtfilt(bf, speed)),
    force_f = if (!all(is.na(force))) as.numeric(signal::filtfilt(bf, force)) else NA_real_
  )

  # --- derive acceleration from filtered speed-time (dv/dt)
  # Use a robust finite-difference that works for slightly irregular time
  t <- df$time
  v <- df$speed_f

  # central diff where possible; forward/backward at edges
  acc <- rep(NA_real_, length(v))
  if (length(v) >= 3) {
    acc[2:(length(v)-1)] <- (v[3:length(v)] - v[1:(length(v)-2)]) / (t[3:length(t)] - t[1:(length(t)-2)])
    acc[1] <- (v[2] - v[1]) / (t[2] - t[1])
    acc[length(v)] <- (v[length(v)] - v[length(v)-1]) / (t[length(t)] - t[length(t)-1])
  } else {
    acc <- c(NA_real_, diff(v) / diff(t))
  }

  # Optional: filter the derived acceleration lightly using same filter
  # (helps with noisy dv/dt)
  acc_f <- as.numeric(signal::filtfilt(bf, acc))
  df$acc_f <- acc_f

  # --- position (use provided if available; else integrate filtered speed)
  if (!all(is.na(df$position))) {
    df$position <- df$position - df$position[1]
    inc <- c(0, diff(df$time) * (head(df$speed_f, -1) + tail(df$speed_f, -1)) / 2)
    df$position_f <- cumsum(inc)
  } else {
    inc <- c(0, diff(df$time) * (head(df$speed_f, -1) + tail(df$speed_f, -1)) / 2)
    df$position_f <- cumsum(inc)
  }

  # --- peak, start, stop
  i_peak <- which.max(df$speed_f)

  if (start_method == "peak_speed") {
    i_start <- i_peak
  } else {
    after_peak <- seq.int(i_peak, nrow(df))
    # derived acceleration threshold (e.g. <= -1.5 m/s^2)
    rel <- which(df$acc_f[after_peak] <= acc_threshold)
    i_start <- if (length(rel)) after_peak[rel[1]] else i_peak
  }

  after_start <- seq.int(i_start, nrow(df))
  rel_stop <- which(df$speed_f[after_start] < stop_speed)
  i_stop <- if (length(rel_stop)) after_start[rel_stop[1]] else nrow(df)

  acc_idx <- seq_len(max(i_start - 1, 1))
  dec_idx <- seq.int(i_start, i_stop)
  acc_dat <- df[acc_idx, , drop = FALSE]
  dec_dat <- df[dec_idx, , drop = FALSE]

  # --- decel distance (prefer provided position; else integrated position_f)
  if (!all(is.na(df$position))) {
    dec_dist <- max(dec_dat$position, na.rm = TRUE) - min(dec_dat$position, na.rm = TRUE)
  } else {
    dec_dist <- max(dec_dat$position_f, na.rm = TRUE) - min(dec_dat$position_f, na.rm = TRUE)
  }

  # --- metrics
  metrics <- dplyr::tibble(
    `Sampling Rate (Hz)`             = fs,
    `Filter Cutoff (Hz)`             = cutoff_hz,
    `Start Method`                   = start_method,
    `Max Approach Speed (m/s)`       = max(acc_dat$speed_f, na.rm = TRUE),
    `Max Acceleration (m/s²)`        = max(acc_dat$acc_f, na.rm = TRUE),
    `Average Acceleration (m/s²)`    = mean(acc_dat$acc_f, na.rm = TRUE),
    `Max Deceleration (m/s²)`        = min(dec_dat$acc_f, na.rm = TRUE),
    `Average Deceleration (m/s²)`    = mean(dec_dat$acc_f, na.rm = TRUE),
    `Decel Duration (s)`             = df$time[i_stop] - df$time[i_start],
    `Decel Distance (m)`             = dec_dist
  ) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))

  subtitle_text <- paste0(
    "Dashed = Peak velocity | ",
    "Dotted = Deceleration start (",
    if (start_method == "acc_threshold") paste0("derived acc ≤ ", acc_threshold, " m/s²") else "at peak velocity",
    ") | Long-dashed = Deceleration end"
  )

  # --- plots: Speed vs Time
  plt_time <- ggplot2::ggplot(df, ggplot2::aes(x = time)) +
    ggplot2::geom_line(ggplot2::aes(y = speed), alpha = 0.35) +
    ggplot2::geom_line(ggplot2::aes(y = speed_f), linewidth = 0.9) +
    ggplot2::geom_vline(xintercept = df$time[i_peak],  linetype = "dashed",   linewidth = 0.6) +
    ggplot2::geom_vline(xintercept = df$time[i_start], linetype = "dotted",   linewidth = 0.6) +
    ggplot2::geom_vline(xintercept = df$time[i_stop],  linetype = "longdash", linewidth = 0.6) +
    ggplot2::labs(
      title = "Speed vs Time (Raw & Filtered)",
      subtitle = subtitle_text,
      x = "Time (s)",
      y = "Speed (m/s)"
    ) +
    ggplot2::theme_classic()

  # --- plots: Speed vs Position (always available because we derive position_f)
  pos_peak  <- df$position_f[i_peak]
  pos_start <- df$position_f[i_start]
  pos_stop  <- df$position_f[i_stop]

  plt_pos <- ggplot2::ggplot(df, ggplot2::aes(x = position_f)) +
    ggplot2::geom_line(ggplot2::aes(y = speed), alpha = 0.35) +
    ggplot2::geom_line(ggplot2::aes(y = speed_f), linewidth = 0.9) +
    ggplot2::geom_vline(xintercept = pos_peak,  linetype = "dashed",   linewidth = 0.6) +
    ggplot2::geom_vline(xintercept = pos_start, linetype = "dotted",   linewidth = 0.6) +
    ggplot2::geom_vline(xintercept = pos_stop,  linetype = "longdash", linewidth = 0.6) +
    ggplot2::labs(
      title = "Speed vs Position (Raw & Filtered)",
      subtitle = subtitle_text,
      x = "Position (m)",
      y = "Speed (m/s)"
    ) +
    ggplot2::theme_classic()

  list(
    data = df,
    indices = list(i_peak = i_peak, i_start = i_start, i_stop = i_stop),
    metrics = metrics,
    plot_time = plt_time,
    plot_pos  = plt_pos
  )
}
