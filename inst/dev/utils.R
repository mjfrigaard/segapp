#' Import plain text or excel data
#'
#' @param path path to file
#' @param sheet sheet number or name (if .xlsx)
#'
#' @return imported data
#' @export import_data
#'
#' @importFrom segtools import_flat_file
import_data <- function(path, sheet = NULL) {
  ext <- tools::file_ext(path)
  if (ext == "xlsx") {
    raw_data <- readxl::read_excel(
        path = path,
        sheet = sheet
      )
    uploaded <- tibble::as_tibble(raw_data)
  } else {
    # call the import function
    uploaded <- segtools::import_flat_file(path = path)
  }
  return(uploaded)
}
