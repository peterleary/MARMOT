#' @title Get Metadata
#' @description Extract the template MARMOT metadata file to a specific folder for easy editing in Excel.
#' @return The MARMOT metadata file will be saved to the specified folder.
#' @author Peter Leary
#' @export
#' @import Rcpp
#' @examples
#' \dontrun{
#' getMetadata(folder = "~/Desktop/Flow_Data/")
#' }
#' getMetadata <- function(folder = NULL) {
#'  openxlsx::write.xlsx(x = system.file("pipeline/", "MARMOT_Metadata.xlsx", package = "MARMOT"), file = file.path(folder, "MARMOT_Metadata.xlsx"))
#' }

getMetadata <- function() {
  # Locate the metadata file in the installed package
  pkg_file <- system.file("pipeline/", "MARMOT_Metadata.xlsx", package = "MARMOT")
  
  # Check if the file exists and is found
  if (!nzchar(pkg_file) || !file.exists(pkg_file)) {
    stop("Metadata file not found in the package installation. Please ensure 'inst/pipeline/MARMOT_Metadata.xlsx' is included in your package.")
  }
  
  # Read and return the metadata from the Excel file
  metadata <- readxl::read_excel(pkg_file)
  return(metadata)
}
