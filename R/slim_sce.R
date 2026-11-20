# The DA/DS copy of the cells
#
# DS analysis works on `sceDS`: the same cells as `sce`, with marker classes
# set for testing and clusters merged. It carries a second copy of every
# expression matrix and embedding. Those are dropped before saving and put
# back from `sce` on loading, so the results hold them once.

#' Drop the expression data and embeddings from an SCE before saving
#'
#' @param x A SingleCellExperiment whose assays and embeddings are also held
#'   by another SCE with the same cells (the pipeline's `sceDS` and `sce`).
#' @return `x` without assays or embeddings. Everything else is kept.
#' @export
slim_sce <- function(x) {
  if (is.null(x)) return(NULL)
  # Remember what was there, so exactly that is put back
  S4Vectors::metadata(x)$slimmed <- list(
    assays      = SummarizedExperiment::assayNames(x),
    reducedDims = SingleCellExperiment::reducedDimNames(x)
  )
  SummarizedExperiment::assays(x, withDimnames = FALSE) <- list()
  SingleCellExperiment::reducedDims(x) <- NULL
  x
}

#' Put the expression data and embeddings back into a slimmed SCE
#'
#' Stops if the two objects do not hold the same cells and markers in the
#' same order.
#'
#' @param x An SCE slimmed with [slim_sce()].
#' @param full The SCE that holds the assays and embeddings.
#' @return `x` with the assays and embeddings of `full`.
#' @export
restore_slim_sce <- function(x, full) {
  if (is.null(x)) return(NULL)
  slimmed <- S4Vectors::metadata(x)$slimmed
  if (is.null(slimmed)) return(x)   # was saved whole
  # Cell names are not compared: the pipeline's cells are unnamed, and names
  # are only given when the main results are saved.
  same_cells <- ncol(x) == ncol(full) &&
    identical(as.character(x$sample_id), as.character(full$sample_id)) &&
    identical(event_numbers(x), event_numbers(full))
  absent <- c(setdiff(slimmed$assays, SummarizedExperiment::assayNames(full)),
              setdiff(slimmed$reducedDims, SingleCellExperiment::reducedDimNames(full)))
  if (!same_cells || !identical(rownames(x), rownames(full)) || length(absent) > 0) {
    stop("The saved DA/DS results do not hold the same cells and markers as the main results.")
  }
  SummarizedExperiment::assays(x, withDimnames = FALSE) <-
    SummarizedExperiment::assays(full, withDimnames = FALSE)[slimmed$assays]
  for (name in slimmed$reducedDims) {
    embedding <- SingleCellExperiment::reducedDim(full, name, withDimnames = FALSE)
    rownames(embedding) <- colnames(x)
    SingleCellExperiment::reducedDim(x, name, withDimnames = FALSE) <- embedding
  }
  S4Vectors::metadata(x)$slimmed <- NULL
  x
}
