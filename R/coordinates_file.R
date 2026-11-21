# The coordinates file
#
# One Excel file holding everything needed to redraw the report's embedding
# plots somewhere else: the plotted cells with their coordinates, metadata,
# clusters and marker values, the colours the report used, and a sheet saying
# what is what. The pipeline builds the tables; this only writes them.

# An Excel sheet holds this many rows, one of which is the header.
.excel_max_rows <- 1048576L

# The "Read me" sheet.
.coordinates_readme <- function(cells, about, as_csv) {
  about <- about[!vapply(about, is.null, logical(1))]
  about <- vapply(about, function(v) paste(as.character(v), collapse = ", "), character(1))
  described <- c(
    "What this is" = paste0(
      "The cells drawn in the report's embedding plots, with everything needed to redraw ",
      "those plots in R, Python, Prism or anything else that reads a table."),
    "Cells" = paste0(
      "One row per plotted cell, in the order the report draws them. Marker columns hold ",
      "the values the report colours by: arcsinh-transformed, then scaled per marker so ",
      "that the 1st percentile is 0 and the 99th is 1."),
    "Expression (arcsinh)" = paste0(
      "The same cells in the same order, with the arcsinh-transformed marker values ",
      "before any scaling."),
    "Colours" = paste0(
      "The colour the report gives each cluster, condition and other group, and the ",
      "colours of the scale used for marker plots, from low to high."),
    "sample_id" = "The sample the cell belongs to.",
    "file_name" = "The FCS file the cell came from.",
    "event_number" = paste0(
      "Which event of that FCS file the cell is (1 is the first event), so that it can ",
      "be found again in FlowJo or any other tool."),
    "sce_idx" = "The cell's position among all the cells of the analysis.",
    "x, y" = "The cell's coordinates in the embedding.",
    "DRMethod" = "The dimension reduction method that made the coordinates.",
    "cluster_id" = "The cell's cluster. Columns named after a k value hold the clustering at that k."
  )
  if (as_csv) {
    described <- c(described, "Why separate files" = paste0(
      "There are more cells than fit on one Excel sheet, so each table is saved as its ",
      "own CSV file."))
  }
  # Describe only the columns this run has
  always <- c("What this is", "Cells", "Expression (arcsinh)", "Colours", "x, y", "Why separate files")
  described <- described[names(described) %in% c(always, names(cells))]
  data.frame(
    Item = c(names(described), names(about)),
    Description = c(unname(described), unname(about)),
    stringsAsFactors = FALSE
  )
}

#' Write the coordinates file
#'
#' Writes the plotted cells, their arcsinh marker values and the report's
#' colours as one Excel file, with a "Read me" sheet in front. If there are
#' more cells than fit on an Excel sheet, each table is written as a CSV file
#' instead, named after `file`.
#'
#' @param file Path of the Excel file to write, ending in `.xlsx`.
#' @param cells One row per plotted cell.
#' @param arcsinh The same cells in the same order, with arcsinh marker values.
#' @param colours The colours used in the report, as a table.
#' @param about A named list of facts about the run for the "Read me" sheet.
#' @return The paths written, invisibly.
#' @export
write_coordinates_file <- function(file, cells, arcsinh, colours, about = list()) {
  stopifnot(nrow(cells) == nrow(arcsinh))
  as_csv <- nrow(cells) + 1L > .excel_max_rows
  sheets <- list(
    "Read me" = .coordinates_readme(cells, about, as_csv),
    "Cells" = as.data.frame(cells),
    "Expression (arcsinh)" = as.data.frame(arcsinh),
    "Colours" = as.data.frame(colours)
  )
  if (!as_csv) {
    openxlsx::write.xlsx(sheets, file = file)
    return(invisible(file))
  }
  message("There are more cells than fit on one Excel sheet, so the coordinates are saved as CSV files.")
  stem <- sub("\\.xlsx$", "", file)
  paths <- paste0(stem, "_", gsub("[^A-Za-z]+", "_", sub("\\)$", "", names(sheets))), ".csv")
  for (i in seq_along(sheets)) {
    data.table::fwrite(sheets[[i]], paths[i])
  }
  invisible(paths)
}
