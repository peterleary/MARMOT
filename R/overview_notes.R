# The Overview's warnings and notes (memory cap, downsampling, QC skipped,
# names lined up) are worked out by chunks that a reload does not run. The
# first run saves them beside its results, so a reload can show them again.
# They can name samples, files and markers, so they are kept out of the run
# log.

#' Save the Overview's warnings for a later reload
#'
#' @param dir The results' `R_files` folder.
#' @param warnings The Overview's warnings, as Markdown callouts.
#' @return The path of the file, invisibly.
#' @export
save_overview_warnings <- function(dir, warnings) {
  path <- file.path(dir, "overview_warnings.json")
  jsonlite::write_json(list(format = 1L, warnings = as.list(as.character(warnings))),
                       path, auto_unbox = TRUE, pretty = TRUE)
  invisible(path)
}

#' Read the Overview's warnings saved by the first run
#'
#' @param dir The results' `R_files` folder.
#' @return `NULL` when the results have none saved (made before this was
#'   added), otherwise a character vector (possibly empty).
#' @export
read_overview_warnings <- function(dir) {
  path <- file.path(dir, "overview_warnings.json")
  if (!file.exists(path)) return(NULL)
  as.character(unlist(jsonlite::read_json(path)$warnings))
}
