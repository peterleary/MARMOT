# Event numbers
#
# MARMOT may analyse only some of the cells in an FCS file: QC removes some,
# and the memory cap and `downsampleTo` sample the rest. So that every cell in
# the results can be traced back to its event in the original file, each frame
# is given a column holding the event's position in that file. The column
# travels with the cells through every filtering and sampling step, and ends
# up in the results as `event_number`.

.event_column <- "MARMOT_event"

# FCS files store 32-bit numbers, which hold whole numbers exactly only up to
# this many. QC'd files are written to disk with the event column in them.
.max_events_per_file <- 2^24

#' Number the events of an FCS file
#'
#' Adds a column, `MARMOT_event`, holding each event's position in the file
#' (1 for the first event). Call it on a freshly read file, before any cells
#' are removed.
#'
#' @param ff A flowFrame, as read from the original FCS file.
#' @return The flowFrame with the extra column.
#' @export
add_event_numbers <- function(ff) {
  n <- nrow(ff)
  if (n > .max_events_per_file) {
    stop("This FCS file has ", format(n, big.mark = ","), " events. MARMOT can number at most ",
         format(.max_events_per_file, big.mark = ","), " events per file.")
  }
  if (.event_column %in% flowCore::colnames(ff)) {
    ff <- ff[, flowCore::colnames(ff) != .event_column]
  }
  numbers <- matrix(as.numeric(seq_len(n)), ncol = 1, dimnames = list(NULL, .event_column))
  flowCore::fr_append_cols(ff, numbers)
}

#' Record each cell's event number in a SingleCellExperiment
#'
#' Reads the `MARMOT_event` column of each frame and lines the numbers up with
#' the cells of `sce`, sample by sample. Stops if they cannot be lined up
#' exactly.
#'
#' The numbers are kept in the SCE's internal cell data, not in `colData`, so
#' they follow the cells through any subsetting but never appear as a
#' variable in plots or tables. Read them back with [event_numbers()].
#'
#' @param sce The SingleCellExperiment made from `fs` by `CATALYST::prepData()`.
#' @param fs The flowSet the cells came from. Its sample names are file names.
#' @param md The file metadata, with `file_name` and `sample_id`.
#' @return `sce`, with the event numbers recorded.
#' @export
record_event_numbers <- function(sce, fs, md) {
  per_file <- lapply(flowCore::sampleNames(fs), function(file_name) {
    frame <- fs[[file_name]]
    if (!.event_column %in% flowCore::colnames(frame)) {
      stop("No event numbers found for ", file_name, ".")
    }
    as.integer(round(flowCore::exprs(frame)[, .event_column]))
  })
  names(per_file) <- flowCore::sampleNames(fs)

  sample_of_cell <- as.character(sce$sample_id)
  numbers <- integer(length(sample_of_cell))
  for (s in unique(sample_of_cell)) {
    file_name <- as.character(md$file_name[match(s, as.character(md$sample_id))])
    cells <- which(sample_of_cell == s)
    from_file <- per_file[[file_name]]
    if (is.null(from_file) || length(from_file) != length(cells)) {
      stop("Cannot match the cells of sample ", s, " to the events of its FCS file.")
    }
    numbers[cells] <- from_file
  }
  SingleCellExperiment::int_colData(sce)$event_number <- numbers
  sce
}

#' Event numbers of the cells in a SingleCellExperiment
#'
#' @param sce A SingleCellExperiment from the pipeline or from saved results.
#' @return An integer vector, one per cell: the position of the cell's event
#'   in its original FCS file. `NULL` if none were recorded.
#' @export
event_numbers <- function(sce) {
  SingleCellExperiment::int_colData(sce)$event_number
}
