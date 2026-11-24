# Data Helpers
# Data extraction, aggregation, and format detection for MARMOT Shiny app

#' The groups to plot, from the include and exclude lists
#'
#' Groups in the include list come first, in its order. Groups present in the
#' data but in neither list (new names after relabelling, say) follow, so they
#' are never dropped silently. Excluded groups are left out.
#' @param values The column's values
#' @param include,exclude The include and exclude lists
bucket_levels <- function(values, include, exclude = NULL) {
  present <- if (is.factor(values)) levels(droplevels(values)) else
    gtools::mixedsort(unique(as.character(values[!is.na(values)])))
  include <- as.character(unlist(include)); exclude <- as.character(unlist(exclude))
  c(intersect(include, present), setdiff(present, c(include, exclude)))
}

#' The results' labels sheet: the one the MARMOT extension reads
#' @param res The loaded results (with `dataDir`, the R_files folder)
labels_sheet_path <- function(res) {
  file.path(normalizePath(file.path(res$dataDir, ".."), mustWork = FALSE), "MARMOT_Cluster_Labels.xlsx")
}

#' Each cell's labelled population, as the last extension report made them
#'
#' @param cluster_id The cluster of each cell
#' @param rec The record, as read by `MARMOT::read_labelled_populations()`
#' @param cluster_colours The cluster colours (named by cluster), or NULL
#' @return A list: `population` (a factor: the populations in the order the
#'   record lists them, then "dropped"; a cluster the record does not know
#'   keeps its own name) and `colours` (each population takes the colour of
#'   its first cluster; "dropped" is grey)
labelled_population_column <- function(cluster_id, rec, cluster_colours = NULL) {
  cluster_id <- as.character(cluster_id)
  pop <- rec$population[match(cluster_id, rec$cluster)]
  pop[is.na(pop)] <- cluster_id[is.na(pop)]
  kept <- unique(rec$population[!rec$dropped])
  lev <- c(kept, setdiff(unique(pop), c(kept, "dropped")), if (any(pop == "dropped")) "dropped")
  colours <- NULL
  if (!is.null(cluster_colours)) {
    first_cluster <- rec$cluster[match(lev, rec$population)]
    first_cluster[is.na(first_cluster)] <- lev[is.na(first_cluster)]
    colours <- stats::setNames(unname(cluster_colours[first_cluster]), lev)
    colours[is.na(colours)] <- "grey60"
    if ("dropped" %in% lev) colours[["dropped"]] <- "grey85"
  }
  list(population = factor(pop, levels = lev), colours = colours)
}

#' Add the labels sheet to the cluster_id relabel table
#'
#' Adds each cluster's size and distinctive markers from the sheet, and the
#' labels already saved there.
#' @param tbl The relabel table (row names are the clusters)
#' @param sheet The sheet, as read by `MARMOT::cluster_labels_table()`
#' @return A list: `table`, and `labelled` (whether any label was found)
add_labels_sheet <- function(tbl, sheet) {
  m <- match(rownames(tbl), sheet$Cluster)
  if ("Cells" %in% names(sheet)) tbl$cells <- sheet$Cells[m]
  if ("Most distinctive markers" %in% names(sheet)) tbl$markers <- sheet$`Most distinctive markers`[m]
  lab <- sheet$Label[m]
  has <- !is.na(lab) & nzchar(lab)
  tbl$relabelled_clusters[has] <- lab[has]
  # Name, size and markers first; the colour last (the panel is narrow)
  first <- intersect(c("relabelled_clusters", "cells", "markers"), names(tbl))
  tbl <- tbl[, c(first, setdiff(names(tbl), first)), drop = FALSE]
  list(table = tbl, labelled = any(has))
}

#' Names as the MARMOT extension reads them: names differing only in capitals,
#' spaces, hyphens or underscores are one name, spelt as first typed
#' @param x Names
same_name_spelling <- function(x) {
  x <- as.character(x)
  key <- MARMOT::name_key(x)
  x[match(key, key)]
}

#' The labels to save from the relabel table: a cluster keeping its own name
#' is saved blank
#' @param tbl The relabel table (row names are the clusters)
labels_to_save <- function(tbl) {
  labs <- trimws(as.character(tbl$relabelled_clusters))
  labs[is.na(labs) | labs == rownames(tbl)] <- ""
  stats::setNames(labs, rownames(tbl))
}

#' Every map in the results, with plain names
#'
#' The results hold each map with every cell and, when the report showed a
#' subsample of cells, a "Downsampled." copy as well. When the subsample is
#' smaller, both are offered: the report's version first, so the Explorer
#' starts on what the report showed. When it holds every cell, the map is
#' offered once.
#' "All" and "Downsampled" are copies of the main map and are not listed again.
#' @param umapDFList The plotting tables of the results
#' @return A named character vector: names are the labels shown, values the
#'   keys in `umapDFList`
dr_map_choices <- function(umapDFList) {
  keys <- setdiff(names(umapDFList), c("All", "Downsampled"))
  maps <- keys[!startsWith(keys, "Downsampled.")]
  # Labels are "Name|note": the note is shown small and grey under the name
  # (see dr_map_render); without it, a plain label is shown as is.
  label <- function(k, cells = NULL) {
    base <- sub("[_.]uncorrected$", "", k)
    base <- switch(base, TSNE = "tSNE", Mpacmap = "PaCMAP (Mpacmap)", pacmap = "PaCMAP", base)
    note <- c(if (grepl("[_.]uncorrected$", k)) "before batch correction", cells)
    if (length(note)) paste0(base, "|", paste(note, collapse = ", ")) else base
  }
  out <- character(0)
  for (k in maps) {
    down <- paste0("Downsampled.", k)
    if (down %in% keys && nrow(umapDFList[[down]]) < nrow(umapDFList[[k]])) {
      out[label(k, "as in the report")] <- down
      out[label(k, "every cell")] <- k
    } else {
      out[label(k)] <- k
    }
  }
  out
}

#' Selectize options that draw a map label as its name with a small grey note
#' under it (labels from `dr_map_choices()`)
dr_map_render <- function() {
  draw <- "function(item, escape) {
    var p = item.label.split('|');
    return '<div style=\"line-height:1.2;padding:2px 0\">' + escape(p[0]) +
      (p.length > 1 ? '<div style=\"font-size:0.8em;opacity:0.6\">' + escape(p[1]) + '</div>' : '') + '</div>';
  }"
  list(
    render = I(paste0("{option: ", draw, ", item: ", draw, "}")),
    # Tall enough to show every map with its note without scrolling
    onInitialize = I("function() { this.$dropdown_content.css('max-height', '420px'); }")
  )
}

#' The default map: the UMAP, tSNE or PaCMAP the report is built on, as the
#' report showed it
#' @param choices As returned by `dr_map_choices()`
dr_map_default <- function(choices) {
  keys <- unname(choices)
  pick <- keys[1]
  for (pref in c("TSNE", "UMAP", "PaCMAP")) {
    hit <- grep(pref, keys[!grepl("[_.]uncorrected$", keys)], ignore.case = TRUE, value = TRUE)
    if (length(hit) > 0) pick <- hit[1]
  }
  pick
}

#' The plotting table for a map key, falling back to the default map
#' @param umapDFList The plotting tables of the results
#' @param key A key, as in `dr_map_choices()`
dr_map_frame <- function(umapDFList, key) {
  if (!is.null(key) && !is.null(umapDFList[[key]])) return(umapDFList[[key]])
  umapDFList[[dr_map_default(dr_map_choices(umapDFList))]]
}

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
  cd <- if (inherits(sce, "SingleCellExperiment")) SummarizedExperiment::colData(sce) else sce
  all_cols <- colnames(cd)
  # Clusterings are always offered, however many clusters they have; other
  # columns only when they have fewer than 100 values
  is_clustering <- all_cols %in% c("cluster_id", "relabelled_clusters", "population") |
    grepl("^(k|meta|p)[0-9]+$", all_cols)
  unlist(lapply(seq_along(all_cols), function(i) {
    if (is_clustering[i] || length(unique(cd[[all_cols[i]]])) < 100) all_cols[i] else NULL
  }))
}

#' Aggregate expression per group for dot/heatmap plots
#' @param expr_df Data frame: rows=cells, columns include markers + group_col
#' @param markers Character vector of marker column names
#' @param group_col Name of the grouping column
#' @param positive_df Optional data frame of the values that decide whether a
#'   cell counts as positive for a marker: same rows as `expr_df`, one column
#'   per marker. Defaults to `expr_df` itself. Pass the arcsinh-transformed
#'   values here when `expr_df` has been normalised or scaled for display, so
#'   the percentage does not change with a display option.
#' @param threshold A cell is positive for a marker when its value is above
#'   this. There is no natural zero in cytometry (every cell has some signal
#'   for every marker), so callers should set it deliberately.
#' @return List with avg_expr (matrix) and pct_expr (matrix: the percentage of
#'   cells in each group above `threshold`)
aggregate_expression <- function(expr_df, markers, group_col,
                                 positive_df = NULL, threshold = 0) {
  if (is.null(positive_df)) positive_df <- expr_df
  groups <- sort(unique(expr_df[[group_col]]))
  avg_mat <- matrix(NA, nrow = length(groups), ncol = length(markers),
                    dimnames = list(groups, markers))
  pct_mat <- avg_mat

  group_factor <- factor(expr_df[[group_col]], levels = groups)
  for (m in markers) {
    vals <- expr_df[[m]]
    avg_mat[, m] <- tapply(vals, group_factor, mean, na.rm = TRUE)
    pct_mat[, m] <- 100 * tapply(positive_df[[m]] > threshold, group_factor, mean, na.rm = TRUE)
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

#' Tidy the "Conditions To Test" column the way the pipeline does
#'
#' The pipeline drops blanks, trims and squeezes spaces, and keeps only rows of
#' the form "A over B". The Explorer must number contrasts the same way, or its
#' "Contrast 2" is not the pipeline's "Contrast 2".
#'
#' @param contrasts Character vector, as read from the study metadata
#' @return Character vector of contrasts, in pipeline order
tidy_contrasts <- function(contrasts) {
  contrasts <- as.character(contrasts)
  contrasts <- contrasts[!is.na(contrasts)]
  contrasts <- gsub("\\s+", " ", trimws(contrasts))
  contrasts[grepl(".+ over .+", contrasts)]
}

#' Look up the DA clusters of one contrast by name
#'
#' The pipeline names each entry of selectedClustersList
#' "Contrast <i>: <condition> Up". Entries are looked up by that name, never by
#' position: a saved results file leaves out empty entries and returns the rest
#' in alphabetical order, so position means nothing.
#'
#' @param selected_clusters_list Named list of DA cluster vectors
#' @param contrasts All contrasts from the study metadata
#' @param contrast The contrast to look up, e.g. "Treated over Control"
#' @return list(up = clusters higher in the first condition,
#'   down = clusters higher in the second condition); empty if none
get_da_clusters <- function(selected_clusters_list, contrasts, contrast) {
  none <- list(up = character(0), down = character(0))
  contrasts <- tidy_contrasts(contrasts)
  i <- match(tidy_contrasts(contrast), contrasts)
  if (length(i) != 1 || is.na(i)) return(none)

  entry <- function(condition) paste0("Contrast ", i, ": ", condition, " Up")
  # A condition name may itself contain " over ", so try each place the
  # contrast could be cut and keep the first whose entries exist.
  # (The look-ahead finds every " over ", including ones that share a space.)
  cuts <- gregexpr("(?= over )", contrasts[i], perl = TRUE)[[1]]
  pairs <- lapply(cuts, function(cut) {
    c(substr(contrasts[i], 1, cut - 1), substr(contrasts[i], cut + 6, nchar(contrasts[i])))
  })
  found <- vapply(pairs, function(pair) any(entry(pair) %in% names(selected_clusters_list)), logical(1))
  conditions <- pairs[[if (any(found)) which(found)[1] else 1]]
  lookup <- function(condition) {
    clusters <- selected_clusters_list[[entry(condition)]]
    if (is.null(clusters)) character(0) else as.character(clusters)
  }
  list(up = lookup(conditions[1]), down = lookup(conditions[2]))
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
    # unique(): a cluster with logFC exactly 0 is listed as both up and down
    ctp <- unique(unlist(clusters_to_plot))
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
#' @param seed The same settings always give the same cells. The session's own
#'   random state is left as it was.
#' @return Character vector of sampled cell IDs
sample_cells_by_group <- function(metadata, group_col, cells_per_group, seed = 2L) {
  with_fixed_seed(seed, unlist(lapply(names(cells_per_group), function(x) {
    group_cells <- rownames(metadata)[!is.na(metadata[[group_col]]) & metadata[[group_col]] == x]
    target <- suppressWarnings(as.numeric(cells_per_group[[x]]))
    if (length(target) != 1 || is.na(target)) target <- length(group_cells)
    n_cells <- max(0, min(floor(target), length(group_cells)))
    group_cells[sample.int(length(group_cells), n_cells)]
  })))
}

#' Evaluate with a fixed seed, leaving the session's random state as it was
#' @param seed The seed
#' @param expr The code to run
with_fixed_seed <- function(seed, expr) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = globalenv())
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = globalenv())
          else rm(".Random.seed", envir = globalenv()), add = TRUE)
  set.seed(seed)
  expr
}

#' The table a map is drawn from: only the columns it needs, in a fixed
#' shuffled order (so no cluster is always drawn on top, and the same
#' settings always give the same picture)
#' @param df The map's plotting table
#' @param cols The columns needed (others are dropped; missing ones ignored)
map_draw_table <- function(df, cols) {
  cols <- intersect(unique(c("x", "y", cols)), names(df))
  df[with_fixed_seed(2L, sample.int(nrow(df))), cols, drop = FALSE]
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
