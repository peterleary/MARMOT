# Data Helpers
# Data extraction, aggregation, and format detection for MARMOT Shiny app

#' Compute median coordinates per group for label placement
#' @param df Data frame with x, y, and a grouping column
#' @param group_col Name of the grouping column
#' @param value_col Optional name of a value column for median/mean stats
#' @return Data frame with one row per group level
compute_label_positions <- function(df, group_col, value_col = NULL) {
  groups <- unique(df[[group_col]])
  result <- data.table::rbindlist(lapply(groups, function(g) {
    idx <- df[[group_col]] == g
    row <- data.frame(
      V1 = g,
      x = median(df[["x"]][idx]),
      y = median(df[["y"]][idx]),
      stringsAsFactors = FALSE
    )
    if (!is.null(value_col) && value_col %in% colnames(df)) {
      vals <- df[[value_col]][idx]
      row$median <- median(vals, na.rm = TRUE)
      row$mean <- mean(vals, na.rm = TRUE)
      row$max <- max(vals, na.rm = TRUE)
    }
    row
  }))
  colnames(result)[1] <- group_col
  as.data.frame(result)
}

#' Extract DR coordinates + all colData as a data.frame (SCE-native)
#'
#' Returns a data.frame with columns x, y, and every colData column.
#' Accepts both bare DR names (e.g. "UMAP") and "Downsampled.UMAP" keys.
#'
#' @param sce A SingleCellExperiment object
#' @param dr_method Name of the reduction (matches reducedDimNames(sce), or
#'   "Downsampled.<name>" prefix is stripped automatically)
#' @return data.frame: x, y + all colData columns (one row per cell)
extract_dr_df <- function(sce, dr_method) {
  rd_names <- SingleCellExperiment::reducedDimNames(sce)
  clean_name <- sub("^Downsampled\\.", "", dr_method)
  chosen <- if (dr_method %in% rd_names) dr_method else if (clean_name %in% rd_names) clean_name else rd_names[1]

  coords <- as.data.frame(SingleCellExperiment::reducedDim(sce, chosen))
  # Standardise first two coordinate columns to x, y
  n_coord_cols <- min(2L, ncol(coords))
  colnames(coords)[seq_len(n_coord_cols)] <- c("x", "y")[seq_len(n_coord_cols)]

  cd <- as.data.frame(SummarizedExperiment::colData(sce))
  cbind(coords, cd)
}

#' Extract a dense expression matrix from an SCE assay (markers × cells)
#'
#' @param sce A SingleCellExperiment object
#' @param assay_name Name of the assay to extract (falls back to first assay if absent)
#' @param markers Optional character vector of marker names to subset; NULL = all.
#'   Names may use "-" or "_"; both are tried.
#' @return Dense numeric matrix, markers × cells
extract_expr_matrix <- function(sce, assay_name, markers = NULL) {
  avail <- SummarizedExperiment::assayNames(sce)
  if (!assay_name %in% avail) assay_name <- avail[1]
  mat <- as.matrix(SummarizedExperiment::assay(sce, assay_name))
  if (!is.null(markers) && length(markers) > 0) {
    idx <- match(markers, rownames(mat))
    # Try with "_" ↔ "-" substitution for unmatched entries
    na_pos <- is.na(idx)
    if (any(na_pos)) {
      idx[na_pos] <- match(gsub("-", "_", markers[na_pos]), rownames(mat))
    }
    idx <- idx[!is.na(idx)]
    # Return empty matrix if nothing matched (not the full matrix)
    mat <- mat[idx, , drop = FALSE]
  }
  mat
}

#' Get plottable metadata columns (discrete, <100 levels)
#' @param sce A SingleCellExperiment object, or a data.frame of colData
#' @return Character vector of column names suitable for plotting
get_plottable_columns <- function(sce) {
  if (inherits(sce, "SingleCellExperiment")) {
    cd <- as.data.frame(SummarizedExperiment::colData(sce))
  } else {
    cd <- sce
  }
  all_cols <- colnames(cd)
  unlist(lapply(all_cols, function(col) {
    if (length(unique(cd[[col]])) < 100) col else NULL
  }))
}

#' Aggregate expression per group for dot/heatmap plots
#' @param expr_df Data frame: rows=cells, columns include markers + group_col
#' @param markers Character vector of marker column names
#' @param group_col Name of the grouping column
#' @return List with avg_expr (matrix) and pct_expr (matrix)
aggregate_expression <- function(expr_df, markers, group_col) {
  groups <- sort(unique(expr_df[[group_col]]))
  avg_mat <- matrix(NA, nrow = length(groups), ncol = length(markers),
                    dimnames = list(groups, markers))
  pct_mat <- avg_mat

  group_factor <- factor(expr_df[[group_col]], levels = groups)
  for (m in markers) {
    vals <- expr_df[[m]]
    avg_mat[, m] <- tapply(vals, group_factor, mean, na.rm = TRUE)
    pct_mat[, m] <- 100 * tapply(vals > 0, group_factor, mean, na.rm = TRUE)
  }

  list(avg_expr = avg_mat, pct_expr = pct_mat)
}

#' Apply cluster relabelling to SCE, umapDFList, and coloursList (pure)
#'
#' Pure transformation extracted from server-relabel.R. Does not mutate inputs.
#' Uses data.table::chmatch for O(n) string lookup instead of base match().
#'
#' @param sce A SingleCellExperiment object
#' @param umapDFList Named list of data.frames (each with source_column column)
#' @param coloursList Named list of named colour vectors
#' @param cluster_table data.frame with rownames = original cluster values,
#'   columns: relabelled_clusters, colours
#' @param source_column Name of the colData/umapDF column being relabelled
#'   (default "cluster_id" for backwards compatibility)
#' @return list(sce, umapDFList, coloursList) with relabelled_clusters added
apply_relabelling_pure <- function(sce, umapDFList, coloursList, cluster_table,
                                   source_column = "cluster_id") {
  # Map source_column → relabelled_clusters for every cell in the SCE
  # data.table::chmatch is O(n) hash-based string matching (faster than base match)
  relabelled <- cluster_table$relabelled_clusters[
    data.table::chmatch(
      as.character(SummarizedExperiment::colData(sce)[[source_column]]),
      rownames(cluster_table)
    )
  ]
  relabelled <- factor(relabelled, levels = unique(gtools::mixedsort(relabelled)))
  sce$relabelled_clusters <- relabelled

  # Build colour vector for relabelled clusters (deduplicate merged names)
  relabel_colours <- cluster_table$colours
  names(relabel_colours) <- cluster_table$relabelled_clusters
  relabel_colours <- relabel_colours[unique(names(relabel_colours))]
  coloursList[["relabelled_clusters"]] <- relabel_colours

  # Update each DR data frame (chmatch for O(n) lookup per frame)
  for (tab in names(umapDFList)) {
    if (!source_column %in% colnames(umapDFList[[tab]])) next
    umapDFList[[tab]]$relabelled_clusters <- cluster_table$relabelled_clusters[
      data.table::chmatch(
        as.character(umapDFList[[tab]][[source_column]]),
        rownames(cluster_table)
      )
    ]
    umapDFList[[tab]]$relabelled_clusters <- factor(
      x = umapDFList[[tab]]$relabelled_clusters,
      levels = gtools::mixedsort(unique(umapDFList[[tab]]$relabelled_clusters))
    )
  }

  list(sce = sce, umapDFList = umapDFList, coloursList = coloursList)
}

#' Filter DA clusters in a umap data frame
#'
#' Pure transformation extracted from server-dr.R. Replaces cluster_id values
#' based on DA mode (All/Up only/Down only/None).
#'
#' @param umap_df A data.frame with cluster_id column (character or factor)
#' @param clusters_to_plot Named list of DA cluster vectors (element 1 = up,
#'   element 2 = down), as produced by selectedClustersList subsetting
#' @param mode One of "All", "Up only", "Down only", "None"
#' @return list(umap_df = modified df, warning = NULL or message string)
filter_da_clusters <- function(umap_df, clusters_to_plot, mode = "None") {
  umap_df$cluster_id <- as.character(umap_df$cluster_id)
  warning_msg <- NULL

  if (mode == "All") {
    ctp <- unlist(clusters_to_plot)
    if (length(ctp) < 1) {
      warning_msg <- "There are no DA clusters in this contrast!"
      umap_df$cluster_id <- factor(umap_df$cluster_id,
                                    levels = gtools::mixedsort(unique(umap_df$cluster_id)))
    } else {
      umap_df$cluster_id[!umap_df$cluster_id %in% ctp] <- "Other"
      umap_df$cluster_id <- factor(umap_df$cluster_id, levels = c(ctp, "Other"))
    }
  } else if (mode == "Up only") {
    ctp <- clusters_to_plot[[1]]
    if (length(ctp) < 1) {
      warning_msg <- "There are no up DA clusters in this contrast!"
      umap_df$cluster_id <- factor(umap_df$cluster_id,
                                    levels = gtools::mixedsort(unique(umap_df$cluster_id)))
    } else {
      umap_df$cluster_id[!umap_df$cluster_id %in% ctp] <- "Other"
      umap_df$cluster_id <- factor(umap_df$cluster_id, levels = c(ctp, "Other"))
    }
  } else if (mode == "Down only") {
    ctp <- clusters_to_plot[[2]]
    if (length(ctp) < 1) {
      warning_msg <- "There are no down DA clusters in this contrast!"
      umap_df$cluster_id <- factor(umap_df$cluster_id,
                                    levels = gtools::mixedsort(unique(umap_df$cluster_id)))
    } else {
      umap_df$cluster_id[!umap_df$cluster_id %in% ctp] <- "Other"
      umap_df$cluster_id <- factor(umap_df$cluster_id, levels = c(ctp, "Other"))
    }
  } else {
    # "None" mode — just factor the cluster_id
    umap_df$cluster_id <- factor(umap_df$cluster_id,
                                  levels = gtools::mixedsort(unique(umap_df$cluster_id)))
  }

  list(umap_df = umap_df, warning = warning_msg)
}

#' Build cluster code table from a umap data frame
#'
#' Extracted from server-download.R (duplicated in two handlers).
#'
#' @param umap_df A data.frame with cluster_id column (factor)
#' @return data.frame with cluster_ids, cluster_id_codes, and optionally
#'   relabelled_clusters + new_cluster_codes columns
build_cluster_codes <- function(umap_df) {
  codes <- data.frame(
    cluster_ids = levels(umap_df$cluster_id),
    cluster_id_codes = seq_len(nlevels(umap_df$cluster_id)),
    stringsAsFactors = FALSE
  )
  if ("relabelled_clusters" %in% colnames(umap_df)) {
    codes$relabelled_clusters <- umap_df$relabelled_clusters[
      match(codes$cluster_ids, umap_df$cluster_id)
    ]
    codes$new_cluster_codes <- as.numeric(factor(codes$relabelled_clusters))
  }
  codes
}

#' Sample cells by group
#'
#' Randomly samples up to `cells_per_group` cells from each level of `group_col`.
#' Extracted from server-subset.R.
#'
#' @param metadata data.frame with rownames as cell IDs
#' @param group_col Column name to group by
#' @param cells_per_group Named numeric vector of cells to sample per group
#' @return Character vector of sampled cell IDs
sample_cells_by_group <- function(metadata, group_col, cells_per_group) {
  unlist(lapply(names(cells_per_group), function(x) {
    group_cells <- rownames(metadata)[metadata[[group_col]] == x]
    n_cells <- min(cells_per_group[[x]], length(group_cells))
    sample(group_cells, n_cells)
  }))
}

#' Parse marker pairs from study metadata
#'
#' Parses the "Marker Pairs" column from the study metadata data frame.
#' Each entry has format "CellType: Marker1 Marker2".
#'
#' @param smd data.frame — study metadata (must contain a "Marker Pairs" column)
#' @return list with:
#'   \item{pairs}{character vector of non-NA marker pair strings}
#'   \item{types}{character vector of unique cell type names}
#'   \item{models}{named list: type -> c(marker1, marker2)}
#'   Returns NULL if no Marker Pairs column or no non-NA entries.
parse_marker_pairs <- function(smd) {
  if (!"Marker Pairs" %in% colnames(smd)) return(NULL)
  pairs <- smd[["Marker Pairs"]][!is.na(smd[["Marker Pairs"]])]
  if (length(pairs) == 0) return(NULL)

  types <- unique(gsub("\\:.*", "", pairs))
  names(pairs) <- gsub("\\:.*", "", pairs)
  models <- lapply(types, function(tp) {
    entry <- pairs[names(pairs) == tp][[1]]
    markers <- strsplit(entry, "\\:\\s*")[[1]][[2]]
    strsplit(markers, "\\s+")[[1]]
  })
  names(models) <- types

  list(pairs = unname(pairs), types = types, models = models)
}

#' Build colour entries for scGate results
#'
#' Creates coloursList entries for Gated_Cells and is_* columns produced
#' by scGate gating.
#'
#' @param gated_cells character vector of unique Gated_Cells values
#' @param model_names character vector of marker pair type names
#' @param palette character vector of colours to draw from (e.g. catalystCols)
#' @return Named list of colour vectors suitable for adding to coloursList
setup_scgate_colours <- function(gated_cells, model_names, palette) {
  cols <- list()

  gate_pal <- palette[seq_along(gated_cells)]
  names(gate_pal) <- gated_cells
  cols[["Gated_Cells"]] <- gate_pal

  for (x in model_names) {
    y <- gsub(" |-", "", x)
    cols[[paste0("is_", y)]] <- setNames("steelblue", x)
  }

  cols
}

#' Proportional cell subsetting
#' @param metadata Data frame of cell metadata
#' @param group_col Column name to group by
#' @param total_cells Target total number of cells
#' @return Named numeric vector: cells per group
calculate_proportional_subset <- function(metadata, group_col, total_cells) {
  cell_counts <- table(metadata[[group_col]])
  proportions <- as.numeric(cell_counts) / sum(cell_counts)
  names(proportions) <- names(cell_counts)
  sc2 <- floor(total_cells * proportions)
  sc2[sc2 == 0] <- 1
  sc2
}
