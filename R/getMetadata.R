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
#' openxlsx::write.xlsx(x = system.file("pipeline/", "MARMOT_Metadata.xlsx", package = "MARMOT"), file = file.path(folder, "MARMOT_Metadata.xlsx"))
#' }

getMetadata <- function(destfile) {
  # Define the path to the metadata file inside the package
  metadata_path <- system.file("pipeline", "MARMOT_Metadata.xlsx", package = "MARMOT")
  
  # Check if the file exists
  if (metadata_path == "") {
    stop("Metadata file not found. Ensure the package is installed correctly.")
  }
  
  # Copy the file to the user-defined destination
  file.copy(metadata_path, destfile)
  
  message("Metadata file has been saved to: ", destfile)
}
