#' Write annotated FCS files from the original FCS files
#'
#' Re-reads the original FCS files and writes one new FCS file per sample that
#' holds only the cells in the report. Each file gets extra columns appended
#' (for example UMAP x and y, and cluster codes), so the cells can be coloured
#' by cluster in FlowJo. Cells MARMOT did not analyse are not written.
#'
#' If an original file is missing, an error of class `marmot_fcs_not_found` is
#' raised. If a file no longer matches the checksum recorded in the results, an
#' error of class `marmot_fcs_changed` is raised.
#'
#' @param cells Data frame with one row per cell and the columns `file_name`
#'   (original FCS file), `sample_id`, `event_number` (1-based row in that
#'   file). Every other column must be numeric and is appended to the FCS file.
#' @param fcs_dir Folder holding the original FCS files.
#' @param out_dir Folder to write into (created if needed).
#' @param source_files Optional data frame with columns `file_name` and `md5`
#'   (as recorded in the results). Files are checked against it.
#'
#' @return The paths of the written files, invisibly.
#' @export
write_annotated_fcs <- function(cells, fcs_dir, out_dir, source_files = NULL) {
  needed <- c("file_name", "sample_id", "event_number")
  if (!is.data.frame(cells) || !all(needed %in% colnames(cells))) {
    stop("`cells` must be a data frame with the columns ",
         paste(needed, collapse = ", "), ".", call. = FALSE)
  }
  extra <- setdiff(colnames(cells), needed)
  if (length(extra) == 0L) stop("`cells` has no columns to append.", call. = FALSE)
  if (anyNA(cells$event_number)) {
    stop("Some cells have no event number, so they cannot be matched to the FCS file.", call. = FALSE)
  }

  files <- unique(as.character(cells$file_name))
  paths <- file.path(fcs_dir, files)
  missing <- !file.exists(paths)
  if (any(missing)) {
    stop(structure(class = c("marmot_fcs_not_found", "error", "condition"), list(
      message = paste0("Cannot find the original FCS file(s) ",
                       paste(files[missing], collapse = ", "),
                       " in the folder ", fcs_dir, "."),
      call = NULL, files = files[missing], folder = fcs_dir)))
  }

  # Check every file before writing anything
  if (!is.null(source_files) && all(c("file_name", "md5") %in% colnames(source_files))) {
    for (i in seq_along(files)) {
      recorded <- source_files$md5[match(files[i], source_files$file_name)]
      if (length(recorded) == 1L && !is.na(recorded) && nzchar(recorded) &&
          !identical(unname(as.character(tools::md5sum(paths[i]))), as.character(recorded))) {
        stop(structure(class = c("marmot_fcs_changed", "error", "condition"), list(
          message = paste0(files[i], " in ", fcs_dir, " is not the file the analysis was run on ",
                           "(its checksum differs from the one recorded), so it was not exported."),
          call = NULL, files = files[i], folder = fcs_dir)))
      }
    }
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  written <- character(0)
  for (i in seq_along(files)) {
    sub <- cells[as.character(cells$file_name) == files[i], , drop = FALSE]
    sample_id <- unique(as.character(sub$sample_id))
    if (length(sample_id) != 1L) {
      stop(files[i], " is linked to more than one sample, which cannot be exported.", call. = FALSE)
    }
    ev <- sub$event_number
    if (anyDuplicated(ev)) {
      stop("Some cells of ", files[i], " share an event number.", call. = FALSE)
    }
    fr <- flowCore::read.FCS(paths[i], transformation = FALSE, truncate_max_range = FALSE)
    if (any(ev < 1 | ev > nrow(fr))) {
      stop("Event numbers for ", files[i], " fall outside the file (it has ", nrow(fr),
           " events). It is probably not the file the analysis was run on.", call. = FALSE)
    }
    # Ascending event order, appended values follow the same order
    ord <- order(ev)
    appended <- as.matrix(sub[ord, extra, drop = FALSE])
    out <- flowCore::fr_append_cols(fr[ev[ord], ], appended)

    out_path <- file.path(out_dir, paste0(safe_file_name(sample_id), "_modified.fcs"))
    suppressWarnings(flowCore::write.FCS(out, filename = out_path, delimiter = "#"))
    written <- c(written, out_path)
  }
  invisible(written)
}
