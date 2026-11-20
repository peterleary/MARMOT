# QC cache stamp
#
# FlowAI and PeacoQC are slow, so the pipeline keeps their cleaned FCS files in
# a folder beside the data and reuses them on the next run. The stamp records
# what those cleaned files were made from, so they are only reused when the
# input files and the QC settings are exactly the same.

.qc_stamp_file <- "marmot_qc_stamp.json"

# Raise this whenever the pipeline changes what it writes into a QC folder, so
# that folders written by an older MARMOT are redone instead of being reused.
.qc_stamp_version <- 2L

#' Describe what a QC cache is made from
#'
#' Builds a stamp from the checksum of every input FCS file, the QC tool and
#' the settings that influence which cells it removes.
#'
#' @param fcs_files Paths to the input FCS files.
#' @param tool The QC tool, e.g. `"FlowAI"` or `"PeacoQC"`.
#' @param settings Named list of the settings that affect the QC result.
#' @return The stamp, as a single JSON string.
#' @export
qc_cache_stamp <- function(fcs_files, tool, settings = list()) {
  missing_files <- fcs_files[!file.exists(fcs_files)]
  if (length(missing_files) > 0) {
    stop("Cannot stamp the QC cache, file(s) not found: ",
         paste(basename(missing_files), collapse = ", "))
  }
  checksums <- unname(tools::md5sum(fcs_files))
  names(checksums) <- basename(fcs_files)
  checksums <- checksums[order(names(checksums))]

  as.character(jsonlite::toJSON(
    list(stamp_version = .qc_stamp_version,
         tool          = tool,
         settings      = settings,
         files         = as.list(checksums)),
    auto_unbox = TRUE, digits = NA, pretty = TRUE
  ))
}

#' Is a QC cache still valid for this run?
#'
#' @param qc_dir The QC results folder.
#' @param stamp A stamp from [qc_cache_stamp()] describing the current run.
#' @return `TRUE` if the folder holds a stamp identical to `stamp`. `FALSE` if
#'   the stamp differs or is absent, as it is for folders written by MARMOT 1.x.
#' @export
qc_cache_is_current <- function(qc_dir, stamp) {
  stamp_path <- file.path(qc_dir, .qc_stamp_file)
  if (!file.exists(stamp_path)) return(FALSE)
  on_disk <- paste(readLines(stamp_path, warn = FALSE), collapse = "\n")
  identical(on_disk, as.character(stamp))
}

#' Record what a QC cache was made from
#'
#' Call this only after QC has finished for every file, so that an interrupted
#' run leaves no stamp and is redone.
#'
#' @param qc_dir The QC results folder.
#' @param stamp A stamp from [qc_cache_stamp()].
#' @return The path of the stamp file, invisibly.
#' @export
qc_cache_write_stamp <- function(qc_dir, stamp) {
  stamp_path <- file.path(qc_dir, .qc_stamp_file)
  writeLines(as.character(stamp), stamp_path)
  invisible(stamp_path)
}
