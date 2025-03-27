#' @title getMetadata
#' @description This function copies the metadata file from the package to a user-defined location.
#' @return A message indicating the location of the metadata file.
#' @author Peter Leary
#' @export
#' @import Rcpp
#' @examples
#' \dontrun{
#' marmot(metadata = "~/Desktop/Flow_Data/MARMOT_metadata.xlsx", name = "Study Name", render = FALSE)
#' }
getMetadata <- function(localfilepath = ".") {
  # Define the path to the metadata file inside the package
  metadata_path <- system.file("pipeline", "MARMOT_Metadata.xlsx", package = "MARMOT")
  
  # Check if the file exists
  if (metadata_path == "") {
    stop("Metadata file not found. Ensure the package is installed correctly.")
  }
  
  # Copy the file to the user-defined destination
  file.copy(metadata_path, localfilepath)
  
  message("Metadata file has been saved to: ", localfilepath)
}
