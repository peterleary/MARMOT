#' @title shinyMarmot
#' @description Start the shinyMarmot application locally using results generated from the main MARMOT pipeline.
#' @return An interactive shiny app session in console.
#' @author Peter Leary
#' @export
#' @import Rcpp
#' @examples
#' \dontrun{
#' shniyMarmot(marmot_output = "~/Desktop/Flow_Data/Results_2025-03-10_11_01_01/R_files")
#' }
shinyMarmot <- function(marmot_output = NA, demo = NA) {
  
  folder <- system.file("app", package = "MARMOT")
  if (!is.na(demo) & is.na(marmot_output)) {
    marmot_output <- system.file("examples/R_files/", package = "MARMOT")
  }
  if (is.na(demo) & is.na(marmot_output)) {
    marmot_output <- system.file("examples/R_files/", package = "MARMOT")
  }
  marmot_output <<- tools::file_path_as_absolute(marmot_output)
  shiny::runApp(appDir = folder)
}
