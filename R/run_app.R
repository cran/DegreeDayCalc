#' @title Run the DegreeDayCalc Shiny application (internal helper)
#' @description Internal helper that launches the bundled Shiny app.
#' @param ... Passed to shiny::runApp().
#' @keywords internal
run_app <- function(...) {
  app_dir <- system.file("app", package = "DegreeDayCalc")
  if (app_dir == "" || !dir.exists(app_dir)) {
    stop(
      "Cannot find the Shiny app directory. ",
      "Please reinstall the package or check that 'inst/app' is included.",
      call. = FALSE
    )
  }
  
  shiny::runApp(appDir = app_dir, ...)
}

#' @title DegreeDayCalc
#' @description Launch the DegreeDayCalc Shiny application for degree-day phenology
#' calculations and visualization of cumulative thermal requirements across life stages.
#'
#' @return Runs a Shiny application in the default browser (or viewer).
#' @export
#'
#' @examples
#' if (interactive()) {
#'   DegreeDayCalc()
#' }
DegreeDayCalc <- function() {
  run_app()
}
