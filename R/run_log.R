# The run log
#
# A small JSON file, marmot_run_log.json in the R_files folder, written as the
# pipeline goes: the versions, the settings, the shape of the data and the
# last chunk reached. If a run crashes, the file still says how far it got.
# It never holds cell-level values or anything that identifies a sample, so
# users can share it when asking for help. Nothing in MARMOT reads it back.

.run_log_time <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

.run_log_read <- function(path) jsonlite::fromJSON(path, simplifyVector = FALSE)

.run_log_write <- function(path, log) {
  jsonlite::write_json(log, path, pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
}

.run_log_seconds <- function(start, end) {
  s <- tryCatch(as.POSIXct(start, format = "%Y-%m-%dT%H:%M:%S%z"), error = function(e) NA)
  e <- tryCatch(as.POSIXct(end, format = "%Y-%m-%dT%H:%M:%S%z"), error = function(e) NA)
  if (is.na(s) || is.na(e)) return(NA_real_)
  as.numeric(difftime(e, s, units = "secs"))
}

#' The run log
#'
#' The pipeline writes a small JSON file, `marmot_run_log.json`, as it goes:
#' the R and package versions, the settings, the shape of the data and the
#' last chunk reached. If a run crashes, the file still says how far it got.
#' It never holds cell-level values or anything that identifies a sample.
#'
#' `run_log_start()` starts a new log, replacing any file already at `path`.
#' `run_log_record()` stores one named section. `run_log_checkpoint()` notes
#' the last chunk completed. `run_log_finish()` marks the run as successful
#' and records how long it took. The last three do nothing if the log has not
#' been started.
#'
#' @param path Path of the log file.
#' @param settings A named list of the settings to record.
#' @param section Name of the section to store.
#' @param values The section's contents, as a list.
#' @param chunk_label Name of the chunk just completed.
#' @return `path`, invisibly.
#' @name run_log
NULL

#' @rdname run_log
#' @export
run_log_start <- function(path, settings = list()) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  pkgs <- c("MARMOT", "diffcyt", "CATALYST", "flowCore", "anndataR", "quarto",
            "ggplot2", "SingleCellExperiment", "S4Vectors", "FlowSOM",
            "limma", "edgeR", "jsonlite", "qs2")
  pkg_versions <- stats::setNames(lapply(pkgs, function(p) tryCatch(
    as.character(utils::packageVersion(p)), error = function(e) NA_character_
  )), pkgs)
  sysinfo <- Sys.info()
  .run_log_write(path, list(
    system   = list(r_version = R.version.string,
                    os = as.character(sysinfo[["sysname"]]),
                    os_release = as.character(sysinfo[["release"]]),
                    arch = as.character(sysinfo[["machine"]]),
                    locale = Sys.getlocale("LC_CTYPE")),
    packages = pkg_versions,
    settings = as.list(settings),
    render   = list(start_time = .run_log_time(), end_time = NULL,
                    duration_seconds = NULL, status = "in_progress",
                    last_completed_chunk = NULL),
    warnings = list()
  ))
  invisible(path)
}

#' @rdname run_log
#' @export
run_log_record <- function(path, section, values) {
  if (!file.exists(path)) return(invisible(path))
  log <- .run_log_read(path)
  log[[section]] <- values
  .run_log_write(path, log)
  invisible(path)
}

#' @rdname run_log
#' @export
run_log_checkpoint <- function(path, chunk_label) {
  if (!file.exists(path)) return(invisible(path))
  log <- .run_log_read(path)
  log$render$last_completed_chunk <- chunk_label
  .run_log_write(path, log)
  invisible(path)
}

#' @rdname run_log
#' @export
run_log_finish <- function(path) {
  if (!file.exists(path)) return(invisible(path))
  log <- .run_log_read(path)
  log$render$end_time <- .run_log_time()
  log$render$status <- "success"
  log$render$duration_seconds <- .run_log_seconds(log$render$start_time, log$render$end_time)
  .run_log_write(path, log)
  invisible(path)
}
