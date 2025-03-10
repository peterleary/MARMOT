#' Extract the metadata file to a specific folder
#' @export
#' @import Rcpp
#' @examples
#' \dontrun{
#' getMetadata(folder = "~/Desktop/MARMOT/")
#' }
getMetadata <- function(folder = NULL) {
  openxlsx::write.xlsx(x = system.file("pipeline/", "MARMOT_Metadata.xlsx", package = "MARMOT"), file = file.path(folder, "MARMOT_Metadata.xlsx"))
}