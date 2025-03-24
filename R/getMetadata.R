#' Download the MARMOT metadata file
#'
#' This function copies the metadata Excel file (`MARMOT_Metadata.xlsx`) from 
#' the package directory to a user-specified location.
#'
#' @param localfilepath A character string specifying the full path where the 
#' metadata file should be saved. If `NULL` (default), the file will be saved 
#' in the current working directory as `"MARMOT_Metadata.xlsx"`.
#' 
#' @details 
#' The metadata file is stored within the package's `inst/pipeline/` directory. 
#' This function retrieves the file path dynamically and allows users to copy it 
#' to a desired location.
#'
#' @examples
#' \dontrun{
#' # Download to the current working directory
#' getMetadata()
#' 
#' # Download to a specific location
#' getMetadata("C:/Users/YourName/Documents/MARMOT_Metadata.xlsx")
#' }
#' 
getMetadata <- function(localfilepath) {
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
