#' @title addMetadataToFCSFolder
#' @description This function copies the metadata file from the package to a user-defined location. Ideally the folder where the gated FCS files live.
#' @param FCS_folder Path to the folder containing FCS files (default ".")
#' @param name Name suffix for the metadata file (default NULL)
#' @param overwrite Logical; overwrite existing file? (default FALSE)
#' @return A message indicating the location of the metadata file.
#' @author Peter Leary
#' @export
#' @examples
#' \dontrun{
#' addMetadataToFCSFolder("Files/MARMOT_Metadata.xlsx")
#' }
addMetadataToFCSFolder <- function(FCS_folder = ".", name = NULL, overwrite = FALSE) {
  metadata_file <- system.file("pipeline", "MARMOT_Metadata.xlsx", package = "MARMOT", mustWork = TRUE)
  
  if (is.null(name)) {
    name <- basename(metadata_file)
  } else {
    name <- paste0("MARMOT_Metadata_", name, ".xlsx")
  }
  
  success <- file.copy(from = metadata_file, to = file.path(FCS_folder, name), overwrite = overwrite)
  
  if (!success) {
    stop("Failed to copy metadata file.")
  }

  message("Metadata file has been saved to: ", file.path(FCS_folder, name))
}
