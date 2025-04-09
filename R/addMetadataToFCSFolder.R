#' @title addMetadataToFCSFolder
#' @description This function copies the metadata file from the package to a user-defined location. Ideally the folder where the gated FCS files live. 
#' @return A message indicating the location of the metadata file.
#' @author Peter Leary
#' @export
#' @import Rcpp
#' @examples
#' \dontrun{
#' addMetadataToFCSFolder("Files/MARMOT_Metadata.xlsx")
#' }
addMetadataToFCSFolder <- function(FCS_folder = ".") {
  # Define the path to the metadata file inside the package
  metadata_file <- system.file("pipeline", "MARMOT_Metadata.xlsx", package = "MARMOT", mustWork = TRUE)
  
  # Copy the file to the user-defined destination
  file.copy(metadata_file, FCS_folder)
  
  message("Metadata file has been saved to: ", FCS_folder)
}
