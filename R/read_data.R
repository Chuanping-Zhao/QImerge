#' Read Data File (CSV, XLSX, or TXT)
#'
#' Reads an input data file (CSV, Excel, or tab-delimited text) and returns a data frame.
#'
#' @param file_path String. Path to the data file to be read.
#' @return A data frame containing the contents of the file.
#' @details The file type is automatically detected by file extension:
#' - `.csv`: read using \code{read.csv()}.
#' - `.xlsx`: read using \code{readxl::read_excel()}.
#' - `.txt`: read as a delimited text using \code{data.table::fread()} (treating empty strings as NA).
#' If the file extension is not one of these, the function will throw an error.
#' @importFrom readxl read_excel
#' @importFrom data.table fread
#' @examples
#' \dontrun{
#' df <- read_data("data/sample.csv")
#' df2 <- read_data("data/sample.xlsx")
#' }
#' @export
read_data <- function(file_path) {
  # Ensure required packages are installed (readxl, data.table, shiny are used)
  check_and_install_packages(c("readxl", "data.table", "shiny"))

  file_extension <- tools::file_ext(file_path)
  switch(tolower(file_extension),
         "csv"  = read.csv(file_path, stringsAsFactors = FALSE),
         "xlsx" = readxl::read_excel(file_path),
         "txt"  = data.table::fread(file_path, na.strings = ""),
         stop("Unsupported file type: ", file_extension))
}
