#' Check and Install Required Packages
#'
#' Ensures that a list of R packages are installed. If a package is not installed, it will be installed from CRAN.
#'
#' @param packages Character vector of package names to check and install.
#' @return This function is called for its side effects. It invisibly returns `NULL`.
#' @details This utility function will attempt to load each package using `require()`. If a package is not found, it calls `install.packages()` to install it, then loads it. This is mainly used to ensure all required libraries for the Shiny app are available.
#' @examples
#' \dontrun{
#' check_and_install_packages(c("dplyr", "shiny"))
#' }
#' @export
check_and_install_packages <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package)
    }
  }
}

