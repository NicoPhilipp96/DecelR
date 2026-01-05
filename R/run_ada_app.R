#' Launch the DecelR ADA Shiny app
#'
#' @return Runs the ADA Shiny app.
#' @export
run_ada_app <- function() {
  app_dir <- system.file("shiny/ada_app", package = "DecelR")
  if (app_dir == "") {
    stop(
      "Could not find the Shiny app directory in the installed package. ",
      "Expected: inst/shiny/ada_app/app.R",
      call. = FALSE
    )
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
