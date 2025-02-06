#' start shinyMarmot application
#' @export
#' @examples
#' \dontrun{
#' marmotOut <- system.file("examples/R_files/", package = "shinyMarmot")
#' shniyMarmot(marmotOut)
#' }
shinyMarmot <- function(fileSE = NA, demo = NA) {
  folder <- system.file("app", package = "shinyMarmot")
  if (!is.na(demo) & is.na(fileSE)) {
    fileSE <- system.file("examples/R_files/", package = "shinyMarmot")
  }
  if (is.na(demo) & is.na(fileSE)) {
    fileSE <- system.file("examples/R_files/", package = "shinyMarmot")
  }
  fileSE <<- tools::file_path_as_absolute(fileSE)
  shiny::runApp(appDir = folder)
}
