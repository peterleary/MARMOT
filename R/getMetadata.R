#' @title Get Metadata
#' @description Extract the template MARMOT metadata file to a specific folder for easy editing in Excel.
#' @return The MARMOT metadata file will be saved to the specified folder.
#' @author Peter Leary
#' @export
#' @import Rcpp
#' @examples
#' \dontrun{
 getMetadata(folder = "~/Desktop/Flow_Data/")
 }
 getMetadata <- function(folder = NULL) {
 openxlsx::write.xlsx(x = system.file("pipeline/", "MARMOT_Metadata.xlsx", package = "MARMOT"), file = file.path(folder, "MARMOT_Metadata.xlsx"))
 }
