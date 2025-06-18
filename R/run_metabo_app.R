#' Run the Metabolomics Shiny Application
#'
#' Launches the Shiny app for metabolite quantitative/qualitative combined analysis included in this package.
#'
#' @return This function does not return a value. It starts the Shiny application for interactive use.
#' @details The Shiny app UI and server logic are contained in the \code{inst/app/} directory of the package. This function finds the app directory and calls \code{shiny::runApp()} to launch it. Ensure you have the required input files ready (sample info and measurement/identification files for both modes) before running the app.
#' @seealso \code{\link[shiny]{runApp}}
#' @examples
#' \dontrun{
#' # After installing and loading the package:
#' run_metabo_app()
#' }
#' @export
#'
run_metabo_app <- function() {
  options(shiny.maxRequestSize = 200 * 1024^2)
  appDir <- system.file("app", package = "QImerge")
  if (appDir == "") {
    stop("Could not find Shiny app directory. Try re-installing the package.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
