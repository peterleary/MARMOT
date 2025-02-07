#' start shinyMarmot application
#' @export
#' @examples
#' \dontrun{
#' marmotOut <- system.file("examples/R_files/", package = "shinyMarmot")
#' shniyMarmot(marmot_output)
#' }
shinyMarmot <- function(marmot_output = NA, demo = NA) {
  folder <- system.file("app", package = "shinyMarmot")
  if (!is.na(demo) & is.na(marmot_output)) {
    marmot_output <- system.file("examples/R_files/", package = "shinyMarmot")
  }
  if (is.na(demo) & is.na(marmot_output)) {
    marmot_output <- system.file("examples/R_files/", package = "shinyMarmot")
  }
  marmot_output <<- tools::file_path_as_absolute(marmot_output)
  shiny::runApp(appDir = folder)
}
