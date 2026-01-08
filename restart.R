# 1) Restart R (Ctrl+Shift+F10 in RStudio), then run:
devtools::document()
devtools::load_all()     # or devtools::install() if you prefer full install
packageVersion("DecelR") # sanity check it's your dev build

# 2) Run the app from SOURCE during development:
shiny::runApp("inst/app")

# (When you’re ready to test the installed app wrapper)
# library(DecelR); run_decelr()
usethis::use_mit_license("Nico Philipp")

################################################################################

library(readr)
library(DecelR)

df <- readr::read_csv(file.choose())

# If your time column is time_s, rename it once:
# df <- dplyr::rename(df, time = time_s)

res <- ada_analyze(
  data = df,
  cutoff_hz = 2,
  filter_order = 4,
  start_method = "acc_threshold",  # or "peak_speed"
  acc_threshold = -1.5,
  stop_speed = 0.2,
  trim_speed = 0.5
)

res$metrics
res$plot_time
res$plot_pos

################################################################################
################################################################################
devtools::install()

file.edit("inst/shiny/ada_app/app.R")
any(grepl("SOURCE_APP_FILE_CONFIRMED",
          readLines("inst/shiny/ada_app/app.R")))

library(DecelR)
run_ada_app()
