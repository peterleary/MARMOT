# Results format stamp
#
# Every results folder says which format it is in, so that MARMOT can tell
# straight away whether it is able to read it. The stamp is written twice:
# as `marmot_format.json` beside the h5ad, and inside the h5ad itself (in
# `uns/marmot_manifest`), so a lone h5ad file can still be identified.

.results_format_name <- "marmot-results"
.results_format_file <- "marmot_format.json"

# Raise this only for a change that an older MARMOT of the same major version
# could not read. Additions to the format keep the number.
.results_format_version <- 2L

# The stamp for results written by this MARMOT.
.results_format_stamp <- function() {
  list(
    format         = .results_format_name,
    format_version = .results_format_version,
    marmot_version = as.character(utils::packageVersion("MARMOT")),
    timestamp      = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )
}

#' Stamp a results folder with its format
#'
#' Writes `marmot_format.json` into the folder that holds
#' `marmot_results.h5ad`.
#'
#' @param output_dir The `R_files` folder.
#' @param stamp The stamp to write. Defaults to the format of this MARMOT.
#' @return The path of the stamp file, invisibly.
#' @export
write_results_format <- function(output_dir, stamp = .results_format_stamp()) {
  stamp_path <- file.path(output_dir, .results_format_file)
  writeLines(jsonlite::toJSON(stamp, auto_unbox = TRUE, pretty = TRUE), stamp_path)
  invisible(stamp_path)
}

#' Read the format of a results file
#'
#' Looks for `marmot_format.json` beside the h5ad first, then for the stamp
#' inside the h5ad. Results made by MARMOT 1.x carry format version 1, or no
#' stamp at all.
#'
#' @param h5ad_path Path to `marmot_results.h5ad`.
#' @return A list with `format_version` (an integer, `NA` if there is no
#'   stamp) and `marmot_version` (a string, `NA` if unknown).
#' @export
read_results_format <- function(h5ad_path) {
  found <- list(format_version = NA_integer_, marmot_version = NA_character_)

  stamp_path <- file.path(dirname(h5ad_path), .results_format_file)
  if (file.exists(stamp_path)) {
    stamp <- tryCatch(jsonlite::fromJSON(stamp_path), error = function(e) NULL)
    if (!is.null(stamp$format_version)) {
      found$format_version <- as.integer(stamp$format_version)
      if (!is.null(stamp$marmot_version)) found$marmot_version <- as.character(stamp$marmot_version)
      return(found)
    }
  }

  # No usable JSON: look inside the h5ad. 1.x wrote `schema_version = 1` and
  # `pipeline_version` here, and format 2 keeps both names.
  read_one <- function(name) {
    tryCatch(
      as.vector(rhdf5::h5read(h5ad_path, paste0("uns/marmot_manifest/", name)))[1],
      error = function(e) NULL
    )
  }
  version <- read_one("schema_version")
  if (!is.null(version)) found$format_version <- as.integer(version)
  made_by <- read_one("pipeline_version")
  if (!is.null(made_by)) found$marmot_version <- as.character(made_by)
  rhdf5::h5closeAll()
  found
}

# Stop with an error of class `marmot_results_format`, so that the Explorer
# can show the message on its own instead of inside a generic error box.
.stop_results_format <- function(...) {
  stop(structure(
    class = c("marmot_results_format", "error", "condition"),
    list(message = paste0(...), call = NULL)
  ))
}

#' Check that this MARMOT can read a results file
#'
#' Stops with a plain message if the results were made by MARMOT 1.x, or by a
#' newer MARMOT than the one installed. Every loader calls this before reading
#' anything else.
#'
#' @param h5ad_path Path to `marmot_results.h5ad`.
#' @return The format, as from [read_results_format()], invisibly.
#' @export
check_results_format <- function(h5ad_path) {
  found <- read_results_format(h5ad_path)
  version <- found$format_version

  if (is.na(version) || version < .results_format_version) {
    .stop_results_format(
      "Oops! These results were dug by old marmots (MARMOT 1.x), and the new marmots ",
      "can't read them. Please re-run the analysis with MARMOT 2, or open them with ",
      "MARMOT 1.3.2, which still can.")
  }
  if (version > .results_format_version) {
    made_by <- if (is.na(found$marmot_version)) "a later version" else paste("MARMOT", found$marmot_version)
    .stop_results_format(
      "Oops! These results were dug by newer marmots (", made_by, ") than the ones ",
      "living here (MARMOT ", as.character(utils::packageVersion("MARMOT")), "). ",
      "Please update MARMOT and try again.")
  }
  invisible(found)
}
