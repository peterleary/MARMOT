# Plotting tables for the embeddings
#
# The report and the Explorer plot from one data frame per embedding
# (`umapDFList`): the cell table, the two coordinates, the cluster columns and
# the quantile-normalised expression. Everything in them is already in the
# SingleCellExperiment, so they are not saved. The results record only which
# cells each table holds, and in what order, and the tables are rebuilt on
# loading.

# Which cells each plotting table holds, in order.
.dr_frame_index <- function(umapDFList) {
  frames <- Filter(function(df) is.data.frame(df) && all(c("sce_idx", "DRMethod") %in% names(df)), umapDFList)
  if (length(frames) == 0) return(NULL)
  list(
    frames = data.frame(
      name    = names(frames),
      method  = vapply(frames, function(df) as.character(df$DRMethod[1]), character(1)),
      n_cells = vapply(frames, nrow, integer(1)),
      stringsAsFactors = FALSE, row.names = NULL
    ),
    cells = data.frame(sce_idx = unlist(lapply(frames, function(df) as.integer(df$sce_idx)), use.names = FALSE)),
    columns = data.frame(
      frame  = rep(names(frames), vapply(frames, ncol, integer(1))),
      column = unlist(lapply(frames, names), use.names = FALSE),
      stringsAsFactors = FALSE
    )
  )
}

# One plotting table, built from the SCE as the pipeline's `build_dr_df` does.
.build_dr_frame <- function(sce, method, clustering_method) {
  coords <- SingleCellExperiment::reducedDim(sce, method)[, 1:2, drop = FALSE]
  colnames(coords) <- c("x", "y")
  df <- data.frame(
    SummarizedExperiment::colData(sce),
    sce_idx = seq_len(ncol(sce)),
    coords,
    DRMethod = method,
    check.names = FALSE
  )
  cluster_codes <- S4Vectors::metadata(sce)$cluster_codes
  if (!is.null(cluster_codes) && !is.null(clustering_method)) {
    df <- dplyr::left_join(df, cluster_codes, by = c("cluster_id" = clustering_method))
  }
  if (!is.null(clustering_method)) df[[clustering_method]] <- df$cluster_id
  qn <- t(SummarizedExperiment::assay(sce, "exprsQuantNorm"))
  dplyr::bind_cols(df, as.data.frame(qn))
}

#' Rebuild the plotting tables from saved results
#'
#' @param sce The SingleCellExperiment, as loaded from the results.
#' @param index The record of which cells each table holds, as saved in the
#'   results (`uns$dr_frames`).
#' @param clustering_method The clustering method of the run
#'   (`clusteringMethodToUse`).
#' @return A named list of data frames, as the pipeline's `umapDFList`.
#' @export
rebuild_dr_frames <- function(sce, index, clustering_method) {
  frames <- as.data.frame(index$frames)
  columns <- as.data.frame(index$columns)
  cells <- as.integer(as.data.frame(index$cells)$sce_idx)
  ends <- cumsum(frames$n_cells)
  starts <- ends - frames$n_cells + 1L
  full <- list()
  out <- lapply(seq_len(nrow(frames)), function(i) {
    method <- as.character(frames$method[i])
    if (is.null(full[[method]])) full[[method]] <<- .build_dr_frame(sce, method, clustering_method)
    idx <- if (frames$n_cells[i] > 0) cells[starts[i]:ends[i]] else integer(0)
    df <- full[[method]][idx, , drop = FALSE]
    wanted <- as.character(columns$column[columns$frame == frames$name[i]])
    absent <- setdiff(wanted, names(df))
    if (length(absent) > 0) {
      stop("Cannot rebuild the plotting table '", frames$name[i], "': missing ", paste(absent, collapse = ", "))
    }
    df <- df[, wanted, drop = FALSE]
    rownames(df) <- NULL
    # Tables holding a subset of the cells had unused levels dropped
    if (nrow(df) < ncol(sce)) df <- droplevels(df)
    df
  })
  stats::setNames(out, as.character(frames$name))
}

# The names of the plotting tables that rebuild exactly from the SCE.
.rebuildable_dr_frames <- function(sce, umapDFList, clustering_method) {
  index <- .dr_frame_index(umapDFList)
  if (is.null(index)) return(character(0))
  candidates <- as.character(index$frames$name)
  rebuilt <- tryCatch(rebuild_dr_frames(sce, index, clustering_method), error = function(e) NULL)
  if (is.null(rebuilt)) return(character(0))
  same <- vapply(candidates, function(n) {
    original <- umapDFList[[n]]
    rownames(original) <- NULL
    isTRUE(all.equal(as.data.frame(original), as.data.frame(rebuilt[[n]]), tolerance = 0, check.attributes = FALSE)) &&
      identical(lapply(original, class), lapply(rebuilt[[n]], class)) &&
      identical(lapply(original, levels), lapply(rebuilt[[n]], levels))
  }, logical(1))
  candidates[same]
}

# The plotting tables of saved results: rebuilt where the results hold a
# record of their cells, read whole where a table was saved whole.
.load_dr_frames <- function(uns, sce, clustering_method) {
  out <- list()
  if (!is.null(uns$dr_frames) && !is.null(sce)) {
    out <- rebuild_dr_frames(sce, uns$dr_frames, clustering_method)
  }
  if (!is.null(uns$dr_dataframes) && is.list(uns$dr_dataframes)) {
    whole <- lapply(uns$dr_dataframes, as.data.frame)
    names(whole) <- gsub("_", ".", names(whole))
    out <- c(out, whole)
  }
  if (length(out) == 0) NULL else out
}
