#' @title addMetadataToFCSFolder
#' @description This function copies the metadata file from the package to a user-defined location. Ideally the folder where the gated FCS files live. 
#' @return A message indicating the location of the metadata file.
#' @author Peter Leary
#' @export
#' @import Rcpp
#' @importFrom Rcpp evalCpp
#' @examples
#' \dontrun{
#' addMetadataToFCSFolder("Files/MARMOT_Metadata.xlsx")
#' }
addMetadataToFCSFolder <- function(FCS_folder = ".", name = NULL) {
  # Define the path to the metadata file inside the package
  metadata_file <- system.file("pipeline", "MARMOT_Metadata.xlsx", package = "MARMOT", mustWork = TRUE)
  
  if (is.null(name)) {
    name = basename(metadata_file)
  } else {
    name = paste0("MARMOT_Metadata_", name, ".xlsx")
  }
  
  # Copy the file to the user-defined destination
  require(readr)
  write_file(x = metadata_file, file = file.path(FCS_folder, name))
  
  message("Metadata file has been saved to: ", FCS_folder)
}
