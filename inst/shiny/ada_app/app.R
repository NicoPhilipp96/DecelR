#' Launch DecelR Shiny app
#' @export
run_decelr <- function() {
  app_dir <- system.file("app", package = "DecelR")
  if (app_dir == "") stop("Could not find app directory. Try re-installing `DecelR`.", call. = FALSE)
  shiny::runApp(app_dir, display.mode = "normal")
}

# inst/app/app.R

library(shiny)
library(ggplot2)
library(DT)
library(readr)
library(DecelR)

ui <- fluidPage(
  titlePanel("DecelR – Acceleration to Deceleration Assessment"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV (time, speed; position/force optional)", accept = ".csv"),
      tags$hr(),
      numericInput("cutoff", "Butterworth cutoff (Hz)", value = 3, min = 0.1, step = 0.1),
      numericInput("order", "Butterworth order", value = 2, min = 1, step = 1),
      selectInput("start_method", "Start of decel threshold",
                  choices = c("Derived acceleration ≤ threshold" = "acc_threshold",
                              "Peak speed" = "peak_speed")),
      numericInput("acc_thr", "Derived acceleration threshold (m/s²)", value = -1.5, step = 0.1),
      numericInput("stop_speed", "End-of-decel speed (m/s)", value = 0.2, min = 0, step = 0.05),
      numericInput("trim_speed", "Trim standing speed (m/s)", value = 0.5, min = 0, step = 0.05),
      selectInput("xaxis", "Plot X-axis", choices = c("Time", "Position", "Both"), selected = "Time"),
      actionButton("analyze", "Analyze", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 uiOutput("plot_ui")
        ),
        tabPanel(
          "Metrics",
          downloadButton("download_metrics", "Download metrics (CSV)"),
          br(), br(),
          DTOutput("metrics")
        ),
        tabPanel("Preview Data", DTOutput("preview"))
      )
    )
  )
)

server <- function(input, output, session) {

  raw_df <- reactive({
    req(input$file)
    readr::read_csv(input$file$datapath, show_col_types = FALSE)
  })

  output$preview <- renderDT({
    req(raw_df())
    datatable(head(raw_df(), 50), options = list(pageLength = 10, scrollX = TRUE))
  })

  results <- eventReactive(input$analyze, {
    validate(need(!is.null(raw_df()), "Please upload a CSV first."))

    df <- raw_df()
    names(df) <- tolower(names(df))

    variants <- list(
      time = c("time", "time_s", "t"),
      speed = c("speed", "vel", "velocity"),
      force = c("force", "fz"),
      position = c("position", "pos", "x")
    )

    map_first_match <- function(alts, names_df) {
      cand <- intersect(alts, names_df)
      if (length(cand) == 0) NA_character_ else cand[1]
    }
    cols <- vapply(variants, map_first_match, names_df = names(df), FUN.VALUE = character(1))

    # Require only time + speed now
    if (any(is.na(cols[c("time","speed")]))) {
      stop("CSV must contain columns for time and speed (position/force optional).")
    }

    df2 <- data.frame(
      time = df[[cols["time"]]],
      speed = df[[cols["speed"]]],
      force = if (!is.na(cols["force"])) df[[cols["force"]]] else NA_real_,
      position = if (!is.na(cols["position"])) df[[cols["position"]]] else NA_real_
    )

    DecelR::ada_analyze(
      data = df2,
      cutoff_hz = input$cutoff,
      filter_order = input$order,
      start_method = input$start_method,
      acc_threshold = input$acc_thr,
      stop_speed = input$stop_speed,
      trim_speed = input$trim_speed
    )
  })

  output$plot_ui <- renderUI({
    req(results())
    if (input$xaxis == "Both") {
      tagList(
        plotOutput("plot_time", height = 380),
        plotOutput("plot_pos",  height = 380)
      )
    } else if (input$xaxis == "Time") {
      plotOutput("plot_time", height = 420)
    } else {
      plotOutput("plot_pos",  height = 420)
    }
  })

  output$plot_time <- renderPlot({
    req(results())
    results()$plot_time
  })

  output$plot_pos <- renderPlot({
    req(results())
    results()$plot_pos
  })

  output$metrics <- renderDT({
    res <- results()
    validate(need(!is.null(res), "Upload data and click Analyze."))
    metrics <- res$metrics
    tbl <- data.frame(Metric = names(metrics), Value = t(metrics), row.names = NULL)
    datatable(tbl, options = list(dom = "t", scrollY = TRUE, pageLength = nrow(tbl)))
  })

  output$download_metrics <- downloadHandler(
    filename = function() {
      paste0("DecelR_ADA_metrics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      res <- results()
      req(res)
      readr::write_csv(res$metrics, file)
    }
  )
}

shinyApp(ui, server)
