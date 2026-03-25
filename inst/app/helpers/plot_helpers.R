# Plot Helpers
# Shared ggplot2 theme, geom, and rendering utilities for MARMOT Shiny app

#' Standard MARMOT DR plot theme
#' @param base_size Base font size
#' @param show_axes Logical: show axis titles/text
#' @param legend_position "right", "bottom", or "none"
#' @return A list of ggplot2 theme elements
marmot_dr_theme <- function(base_size = 14, show_axes = FALSE, legend_position = "right") {
  th <- theme_minimal(base_size = base_size) +
    theme(
      axis.text             = element_blank(),
      axis.ticks            = element_blank(),
      axis.line             = element_blank(),
      panel.grid            = element_blank(),
      plot.title            = element_text(face = "bold", hjust = 0),
      plot.subtitle         = element_text(face = "plain", hjust = 0),
      plot.caption          = element_text(face = "italic", hjust = 1),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.margin           = margin(t = 0, r = 0, b = 0, l = 0),
      plot.background       = element_rect(fill = "white", colour = "white"),
      panel.background      = element_rect(fill = "white", colour = "white"),
      legend.background     = element_rect(fill = "white", colour = "white"),
      legend.justification  = "center",
      legend.text           = element_text(size = base_size * 0.8, face = "bold"),
      legend.title          = element_text(face = "bold"),
      legend.position       = legend_position
    )
  if (!show_axes) {
    th <- th + theme(axis.title = element_blank())
  }
  th
}

#' Create a feature scatter plot (replaces SCpubr::do_FeaturePlot)
#' @param df Data frame with x, y columns and a marker column
#' @param marker Name of the marker column to colour by
#' @param palette Colour palette name
#' @param direction Palette direction (1 or -1)
#' @param point_size Point size
#' @param rasterise Logical: use scattermore for rasterisation
#' @param raster_dpi DPI for rasterised points
#' @param border Logical: show cell borders (SCpubr-style 3-layer sandwich)
#' @param border_size Multiplier for border point size (default 2.0)
#' @param border_density Quantile threshold for border cell selection (default 1 = all cells)
#' @param border_colour Colour for border points (default "black")
#' @param base_size Font base size
#' @param show_axes Logical: show axes
#' @param legend_position Legend position string
#' @return A ggplot object
make_feature_scatter <- function(df, marker, palette = "viridis", direction = 1,
                                 point_size = 0.8, alpha = 1,
                                 rasterise = FALSE, raster_dpi = 1024,
                                 border = FALSE, border_size = 2.0, border_density = 1,
                                 border_colour = "black",
                                 border_df = NULL,
                                 base_size = 14, show_axes = FALSE,
                                 legend_position = "right") {
  if (!marker %in% colnames(df)) {
    warning("Marker '", marker, "' not found in data frame")
    return(ggplot())
  }
  # Sort so high values plot on top
  df <- df[order(df[[marker]], decreasing = FALSE), ]

  p <- ggplot(df, aes(x = .data[["x"]], y = .data[["y"]]))

  if (border && nrow(df) > 10) {
    # SCpubr-style 3-layer sandwich: border → grey75 base → colored foreground
    # border_density=1 → all cells get borders; <1 → only low-density peripheral cells
    if (is.null(border_df)) {
      kde <- MASS::kde2d(df[["x"]], df[["y"]], n = 100L)
      ix  <- pmax(1L, pmin(findInterval(df[["x"]], kde$x), length(kde$x)))
      iy  <- pmax(1L, pmin(findInterval(df[["y"]], kde$y), length(kde$y)))
      cell_density <- kde$z[cbind(ix, iy)]
      border_df <- df[cell_density <= quantile(cell_density, border_density), ]
    }

    # SCpubr halves pt.size internally for border mode
    eff_size <- point_size / 2

    if (rasterise) {
      eff_raster <- (point_size * 2) + 0.6
      p <- p +
        # Layer 1: border (edge cells)
        scattermore::geom_scattermore(data = border_df,
          pointsize = eff_raster * border_size,
          pixels = c(raster_dpi, raster_dpi),
          color = border_colour) +
        # Layer 2: grey base (all cells)
        scattermore::geom_scattermore(
          pointsize = eff_raster,
          pixels = c(raster_dpi, raster_dpi),
          color = "grey75") +
        # Layer 3: colored foreground (all cells)
        scattermore::geom_scattermore(
          pointsize = eff_raster,
          pixels = c(raster_dpi, raster_dpi),
          aes(colour = .data[[marker]]))
    } else {
      p <- p +
        # Layer 1: border (edge cells)
        geom_point(data = border_df,
                   size = eff_size * border_size, colour = border_colour,
                   show.legend = FALSE, na.rm = TRUE) +
        # Layer 2: grey base (all cells)
        geom_point(colour = "grey75", size = eff_size,
                   show.legend = FALSE, na.rm = TRUE) +
        # Layer 3: colored foreground (all cells, with caller's alpha)
        geom_point(aes(colour = .data[[marker]]), size = eff_size,
                   alpha = alpha, na.rm = TRUE)
    }
    p <- apply_continuous_scale(p, palette, direction, "colour",
                                legend_position = legend_position)
  } else if (rasterise) {
    p <- p + scattermore::geom_scattermore(
      pointsize = (point_size * 2) + 0.6,
      pixels = c(raster_dpi, raster_dpi),
      alpha = alpha,
      aes(colour = .data[[marker]])
    )
    p <- apply_continuous_scale(p, palette, direction, "colour",
                                legend_position = legend_position)
  } else {
    p <- p + geom_point(
      aes(colour = .data[[marker]]),
      size = point_size, alpha = alpha
    )
    p <- apply_continuous_scale(p, palette, direction, "colour",
                                legend_position = legend_position)
  }

  p <- p + marmot_dr_theme(base_size, show_axes, legend_position) +
    ggtitle(marker)

  p
}

#' Create a violin plot (replaces SCpubr::do_ViolinPlot)
#' @param df Data frame with expression values and grouping columns
#' @param marker Name of the marker column
#' @param group_col Column to group violins by
#' @param split_col Optional column to split by (fills within group)
#' @param colours Named colour vector for groups
#' @param point_size Size of jittered points (0 = no points)
#' @param base_size Font size
#' @param show_boxplot Logical: overlay a boxplot inside the violin
#' @param trim Logical: trim violin tails to data range
#' @param show_quartiles Logical: draw Q25/Q75 dotted lines
#' @param show_median Logical: draw median solid line
#' @param violin_width Width of violin bodies
#' @param line_thickness Linewidth for quartile/median segments
#' @param bar_width Width of quartile/median line segments
#' @param axis_angle Angle for x-axis text (0, 45, 90, or 270)
#' @return A ggplot object
make_violin_plot <- function(df, marker, group_col, split_col = NULL,
                             colours = NULL, point_size = 0, base_size = 14,
                             show_boxplot = FALSE, trim = TRUE,
                             show_quartiles = FALSE, show_median = FALSE,
                             violin_width = 0.8, line_thickness = 0.5,
                             bar_width = 0.15, axis_angle = 45) {
  if (is.null(split_col)) {
    p <- ggplot(df, aes(
      x = .data[[group_col]],
      y = .data[[marker]],
      fill = .data[[group_col]]
    ))
  } else {
    p <- ggplot(df, aes(
      x = .data[[group_col]],
      y = .data[[marker]],
      fill = .data[[split_col]]
    ))
  }

  p <- p + geom_violin(scale = "width", trim = trim, alpha = 0.8, width = violin_width)

  # Quartile/median lines (before boxplot for correct layering)
  if (show_median || show_quartiles) {
    quantile_data <- df |>
      dplyr::group_by(.data[[group_col]]) |>
      dplyr::summarise(
        q25 = stats::quantile(.data[[marker]], 0.25, na.rm = TRUE),
        q50 = stats::quantile(.data[[marker]], 0.50, na.rm = TRUE),
        q75 = stats::quantile(.data[[marker]], 0.75, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(x_num = as.numeric(factor(.data[[group_col]])))

    if (show_median) {
      p <- p + geom_segment(data = quantile_data,
        aes(x = x_num - bar_width / 2, xend = x_num + bar_width / 2,
            y = q50, yend = q50),
        linewidth = line_thickness, colour = "black", inherit.aes = FALSE)
    }
    if (show_quartiles) {
      p <- p + geom_segment(data = quantile_data,
        aes(x = x_num - bar_width / 2, xend = x_num + bar_width / 2,
            y = q25, yend = q25),
        linewidth = line_thickness, colour = "black", linetype = "dotted",
        inherit.aes = FALSE)
      p <- p + geom_segment(data = quantile_data,
        aes(x = x_num - bar_width / 2, xend = x_num + bar_width / 2,
            y = q75, yend = q75),
        linewidth = line_thickness, colour = "black", linetype = "dotted",
        inherit.aes = FALSE)
    }
  }

  if (show_boxplot) {
    p <- p + geom_boxplot(width = 0.15, fill = "white", alpha = 0.7,
                          outlier.shape = NA, colour = "grey30")
  }

  if (point_size > 0) {
    p <- p + geom_jitter(width = 0.2, size = point_size, alpha = 0.3)
  }

  if (!is.null(colours)) {
    p <- p + scale_fill_manual(values = colours)
  }

  p <- p +
    ggprism::theme_prism(base_size = base_size, axis_text_angle = axis_angle) +
    theme(legend.position = "none") +
    labs(y = marker, x = NULL) +
    ggtitle(marker)

  p
}

#' Create a dot plot (replaces SCpubr::do_DotPlot)
#' @param avg_expr Matrix: groups x markers (mean expression)
#' @param pct_expr Matrix: groups x markers (percent expressed)
#' @param palette Colour palette name
#' @param direction Palette direction
#' @param dot_scale Scaling factor for dot size
#' @param flip Logical: flip axes
#' @param base_size Font size
#' @param scaling Expression scaling: "none", "zscore", or "quantile"
#' @param scale_basis Scaling computed across "cells" or "groups"
#' @param hide_border Logical: if TRUE use pch=20 (no border) instead of default
#' @param uniform_size Logical: if TRUE all dots same size (ignore pct_expr)
#' @param hide_legend Logical: if TRUE suppress legend
#' @param legend_position Legend position string
#' @return A ggplot object
make_dot_plot <- function(avg_expr, pct_expr, palette = "viridis", direction = 1,
                          dot_scale = 10, flip = TRUE, base_size = 14,
                          scaling = "none", scale_basis = "cells",
                          hide_border = FALSE, uniform_size = FALSE,
                          hide_legend = FALSE, legend_position = "right") {
  # Apply expression scaling
  if (scaling == "zscore") {
    for (m in colnames(avg_expr)) {
      vals <- avg_expr[, m]
      s <- stats::sd(vals, na.rm = TRUE)
      if (s > 0) {
        avg_expr[, m] <- (vals - mean(vals, na.rm = TRUE)) / s
      } else {
        avg_expr[, m] <- 0
      }
    }
  } else if (scaling == "quantile") {
    for (m in colnames(avg_expr)) {
      q99 <- stats::quantile(avg_expr[, m], 0.99, na.rm = TRUE)
      if (q99 > 0) avg_expr[, m] <- pmin(avg_expr[, m] / q99, 1)
    }
  }

  # Reshape to long format
  df <- expand.grid(
    group = rownames(avg_expr),
    marker = colnames(avg_expr),
    stringsAsFactors = FALSE
  )
  df$avg_expr <- as.vector(avg_expr)
  df$pct_expr <- as.vector(pct_expr)

  if (uniform_size) {
    p <- ggplot(df, aes(
      x = .data[["marker"]],
      y = .data[["group"]],
      colour = .data[["avg_expr"]]
    )) +
      geom_point(size = dot_scale / 2,
                 shape = if (hide_border) 20 else 21) +
      theme_classic(base_size = base_size)
  } else {
    p <- ggplot(df, aes(
      x = .data[["marker"]],
      y = .data[["group"]],
      size = .data[["pct_expr"]],
      colour = .data[["avg_expr"]]
    )) +
      geom_point(shape = if (hide_border) 20 else 21) +
      scale_size_continuous(range = c(0, dot_scale), name = "% Expressing") +
      theme_classic(base_size = base_size)
  }

  p <- p +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      legend.position = if (hide_legend) "none" else legend_position
    ) +
    labs(x = NULL, y = NULL)

  p <- apply_continuous_scale(p, palette, direction, "colour")

  if (flip) p <- p + coord_flip()

  p
}

#' Create a ridge plot (replaces SCpubr::do_RidgePlot)
#' @param df Data frame with marker values and group column
#' @param marker Name of the marker column
#' @param group_col Column to group ridges by
#' @param colours Named colour vector
#' @param base_size Font size
#' @param hide_legend Logical: if TRUE suppress legend
#' @param legend_position Legend position string (ignored when hide_legend = TRUE)
#' @return A ggplot object
make_ridge_plot <- function(df, marker, group_col, colours = NULL, base_size = 14,
                            hide_legend = TRUE, legend_position = "right") {
  final_legend_pos <- if (hide_legend) "none" else legend_position

  p <- ggplot(df, aes(
    x = .data[[marker]],
    y = .data[[group_col]],
    fill = .data[[group_col]]
  )) +
    ggridges::geom_density_ridges(scale = 1.2, alpha = 0.7) +
    theme_classic(base_size = base_size) +
    theme(legend.position = final_legend_pos) +
    labs(x = marker, y = NULL) +
    ggtitle(marker)

  if (!is.null(colours)) {
    p <- p + scale_fill_manual(values = colours)
  }

  p
}

#' Create a per-cell heatmap (replaces Seurat::DoHeatmap)
#' @param expr_mat Matrix: markers × cells (expression values)
#' @param group_ids Factor of group assignments per cell
#' @param group_colours Named colour vector for groups
#' @param palette Colour palette for heatmap values
#' @param direction Palette direction
#' @return A ComplexHeatmap object
make_percell_heatmap <- function(expr_mat, group_ids, group_colours = NULL,
                                 palette = "viridis", direction = 1) {
  # Order cells by group
  ord <- order(group_ids)
  expr_mat <- expr_mat[, ord, drop = FALSE]
  group_ids <- group_ids[ord]

  # Top annotation for groups — guard against missing or extra colour levels
  present_levels <- unique(as.character(group_ids))

  if (!is.null(group_colours)) {
    matched <- group_colours[names(group_colours) %in% present_levels]
    missing_lvls <- setdiff(present_levels, names(matched))
    if (length(missing_lvls) > 0) {
      extra <- setNames(
        grDevices::grey.colors(length(missing_lvls), start = 0.4, end = 0.8),
        missing_lvls
      )
      matched <- c(matched, extra)
    }
    col_list <- list(Group = matched)
  } else {
    col_list <- NULL
  }

  top_anno <- ComplexHeatmap::HeatmapAnnotation(
    Group = group_ids,
    col = col_list,
    show_annotation_name = TRUE
  )

  # Build colour function
  if (palette %in% viridisColours) {
    cols <- viridis::viridis(100, option = palette, direction = direction)
  } else if (palette %in% scicoColours) {
    cols <- scico::scico(100, palette = palette, direction = direction)
  } else {
    cols <- viridis::viridis(100, direction = direction)
  }
  col_fun <- circlize::colorRamp2(
    seq(min(expr_mat, na.rm = TRUE), max(expr_mat, na.rm = TRUE), length.out = 100),
    cols
  )

  ComplexHeatmap::Heatmap(
    expr_mat,
    name = "Expression",
    col = col_fun,
    top_annotation = top_anno,
    cluster_columns = FALSE,
    show_column_names = FALSE,
    row_names_gp = grid::gpar(fontsize = 10),
    use_raster = ncol(expr_mat) > 5000
  )
}

#' Create an expression heatmap (replaces SCpubr::do_ExpressionHeatmap)
#'
#' Aggregates mean expression per group x marker, optionally clusters rows
#' and columns via hclust, and renders as a geom_tile heatmap.
#'
#' @param sce A SingleCellExperiment object
#' @param features Character vector of marker names to plot
#' @param assay_name Name of the assay to extract expression from
#' @param group_col Column in colData to group cells by
#' @param cluster Logical: hierarchically cluster rows and columns
#' @param palette Colour palette name (passed to apply_continuous_scale)
#' @param direction 1 or -1 for palette direction
#' @param flip Logical: if TRUE (default) x = groups, y = features;
#'   if FALSE x = features, y = groups
#' @param scaling Expression scaling: "none", "zscore", or "quantile"
#' @param custom_limits Numeric vector of length 2 for colour scale limits
#'   (values outside the range are squished). NULL = auto limits.
#' @return A ggplot object
make_expression_heatmap <- function(sce, features, assay_name, group_col,
                                    cluster = TRUE, palette = "viridis",
                                    direction = 1, flip = TRUE,
                                    scaling = "none", custom_limits = NULL) {
  expr_mat <- extract_expr_matrix(sce, assay_name, features)
  cd <- as.data.frame(SummarizedExperiment::colData(sce))

  # Build cell-level df and aggregate via existing helper
  df <- as.data.frame(t(expr_mat))
  df[[group_col]] <- cd[[group_col]]
  markers_in_df <- rownames(expr_mat)[rownames(expr_mat) %in% colnames(df)]
  agg_result <- aggregate_expression(df, markers_in_df, group_col)
  mat <- agg_result$avg_expr  # groups x markers matrix
  mat[is.na(mat)] <- 0

  # Apply expression scaling
  if (scaling == "zscore") {
    for (m in seq_len(ncol(mat))) {
      vals <- mat[, m]
      s <- stats::sd(vals, na.rm = TRUE)
      if (s > 0) {
        mat[, m] <- (vals - mean(vals, na.rm = TRUE)) / s
      } else {
        mat[, m] <- 0
      }
    }
  } else if (scaling == "quantile") {
    for (m in seq_len(ncol(mat))) {
      q99 <- stats::quantile(mat[, m], 0.99, na.rm = TRUE)
      if (q99 > 0) mat[, m] <- pmin(mat[, m] / q99, 1)
    }
  }

  # Hierarchical clustering of rows (groups) and columns (features)
  if (cluster && nrow(mat) > 1) {
    row_ord <- rownames(mat)[hclust(dist(mat, "euclidean"), "ward.D")$order]
  } else {
    row_ord <- rownames(mat)
  }
  if (cluster && ncol(mat) > 1) {
    col_ord <- colnames(mat)[hclust(dist(t(mat), "euclidean"), "ward.D")$order]
  } else {
    col_ord <- colnames(mat)
  }

  # Reshape to long format for ggplot
  long <- reshape2::melt(mat, varnames = c("group", "feature"), value.name = "mean")
  long$group   <- factor(long$group,   levels = row_ord)
  long$feature <- factor(long$feature, levels = rev(col_ord))

  # Apply custom limits via squish if provided
  if (!is.null(custom_limits) && is.numeric(custom_limits) && length(custom_limits) == 2) {
    long$mean <- scales::squish(long$mean, range = custom_limits)
  }

  # Axis mapping: flip controls whether groups are on x or y
  if (flip) {
    p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$group, y = .data$feature, fill = .data$mean)) +
      ggplot2::labs(x = group_col, y = NULL)
  } else {
    p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$feature, y = .data$group, fill = .data$mean)) +
      ggplot2::labs(x = NULL, y = group_col)
  }

  p <- p +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::scale_x_discrete(expand = c(0, 0), position = "top") +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.text.x.top    = ggplot2::element_text(angle = 45, hjust = 0, vjust = 0),
      axis.text.x.bottom = ggplot2::element_blank(),
      axis.ticks.x.bottom = ggplot2::element_blank(),
      panel.grid         = ggplot2::element_blank(),
      panel.border       = ggplot2::element_rect(fill = NA, color = "black", linewidth = 1),
      legend.position    = "bottom",
      plot.background    = ggplot2::element_rect(fill = "white", color = "white"),
      panel.background   = ggplot2::element_rect(fill = "white", color = "white")
    )

  apply_continuous_scale(p, palette, direction, "fill")
}

#' Add facet wrapping with cell counts in strip labels
#' @param p A ggplot object
#' @param df The data frame used in the plot
#' @param split_col Column name to facet by
#' @param ncol Number of facet columns
#' @return Modified ggplot with faceting
add_facet_with_counts <- function(p, df, split_col, ncol = 1) {
  levels_to_split <- levels(as.factor(df[[split_col]]))
  labs <- setNames(
    vapply(levels_to_split, function(lev) {
      paste0(lev, "\n n = ", sum(df[[split_col]] == lev))
    }, character(1)),
    levels_to_split
  )
  p + facet_wrap(
    reformulate(split_col),
    ncol = ncol,
    labeller = as_labeller(labs)
  )
}

#' Create a cell count barplot
#' @param df Data frame with metadata columns
#' @param x_col Column for x-axis
#' @param fill_col Column for fill
#' @param colours Named colour vector
#' @param fractional Logical: show proportions instead of counts
#' @param show_numbers Logical: add count labels
#' @param base_size Font size
#' @return A ggplot object
make_barplot <- function(df, x_col, fill_col, colours = NULL,
                         fractional = FALSE, show_numbers = FALSE, base_size = 14) {
  position <- if (fractional) "fill" else "stack"

  p <- ggplot(df, aes(x = .data[[x_col]], fill = .data[[fill_col]])) +
    geom_bar(stat = "count", position = position) +
    theme_classic(base_size = base_size) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

  if (!is.null(colours)) {
    p <- p + scale_fill_manual(values = colours)
  }

  if (show_numbers) {
    p <- p + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)
  }

  p
}

#' Fast clustree — vectorized replacement for clustree::clustree()
#'
#' Builds the same cluster-resolution tree but skips the O(r²c²n) SC3
#' stability calculation that dominates clustree runtime.  Edge weights
#' are computed from vectorized contingency tables instead of per-pair
#' loops.  Typically 10-50× faster on real datasets.
#'
#' @param sce SingleCellExperiment with clustering columns in colData
#' @param prefix Column prefix (e.g. "meta", "k", "p")
#' @param prop_filter Minimum in_prop to draw an edge (default 0.1)
#' @param node_size_range ggplot2 size range for nodes
#' @param edge_width Static width for edges
#' @param node_text_size Cluster label size
#' @return A ggplot object (ggraph)
fast_clustree <- function(sce, prefix = "meta", prop_filter = 0.1,
                          node_size_range = c(4, 15), edge_width = 1.5,
                          node_text_size = 3) {
  requireNamespace("ggraph",    quietly = TRUE)
  requireNamespace("tidygraph", quietly = TRUE)

  cd <- as.data.frame(SummarizedExperiment::colData(sce))
  k_cols <- grep(paste0("^", prefix, "[0-9]+$"), colnames(cd), value = TRUE)
  k_nums <- as.integer(sub(paste0("^", prefix), "", k_cols))
  ord    <- order(k_nums)
  k_cols <- k_cols[ord]
  k_nums <- k_nums[ord]
  if (length(k_cols) < 2) stop("Need >= 2 '", prefix, "N' columns for clustree")

  # ── Nodes: one row per cluster-at-resolution ─────────────────────────────
  nodes <- do.call(rbind, lapply(seq_along(k_cols), function(i) {
    counts <- table(as.character(cd[[k_cols[i]]]))
    data.frame(
      node    = paste0(prefix, k_nums[i], "C", names(counts)),
      res     = k_nums[i],
      cluster = names(counts),
      size    = as.integer(counts),
      stringsAsFactors = FALSE
    )
  }))
  nodes$res_fac <- factor(nodes$res)

  # ── Edges: from contingency tables (vectorized) ──────────────────────────
  empty_edges <- data.frame(from_node = character(0), to_node = character(0),
                            count = integer(0), in_prop = numeric(0),
                            stringsAsFactors = FALSE)
  edges <- do.call(rbind, lapply(seq_len(length(k_cols) - 1), function(i) {
    ct  <- table(from = as.character(cd[[k_cols[i]]]),
                 to   = as.character(cd[[k_cols[i + 1]]]))
    idx <- which(ct > 0, arr.ind = TRUE)
    fc  <- rownames(ct)[idx[, 1]]
    tc  <- colnames(ct)[idx[, 2]]
    cnt <- as.integer(ct[idx])
    ip  <- cnt / colSums(ct)[tc]
    keep <- ip >= prop_filter
    if (!any(keep)) return(empty_edges)
    data.frame(
      from_node = paste0(prefix, k_nums[i],     "C", fc[keep]),
      to_node   = paste0(prefix, k_nums[i + 1], "C", tc[keep]),
      count     = cnt[keep],
      in_prop   = ip[keep],
      stringsAsFactors = FALSE
    )
  }))

  # Core edges = highest in_prop per target node (matches clustree default)
  if (is.null(edges) || nrow(edges) == 0) {
    stop("No edges survive prop_filter=", prop_filter,
         ". Try lowering it or check that clustering columns have real structure.")
  }
  edges$is_core <- ave(edges$in_prop, edges$to_node, FUN = max) == edges$in_prop
  core <- edges[edges$is_core, ]

  # ── Build tidygraph ──────────────────────────────────────────────────────
  graph <- tidygraph::tbl_graph(
    nodes = nodes,
    edges = data.frame(
      from    = match(core$from_node, nodes$node),
      to      = match(core$to_node,   nodes$node),
      count   = core$count,
      in_prop = core$in_prop
    ),
    directed = TRUE
  )

  # ── Render with ggraph ──────────────────────────────────────────────────
  ggraph::ggraph(graph, layout = "tree") +
    ggraph::geom_edge_link(
      ggplot2::aes(colour = .data$count, alpha = .data$in_prop),
      edge_width = edge_width,
      arrow   = grid::arrow(length = grid::unit(1.5, "mm"), type = "closed"),
      end_cap = ggraph::circle(1.5, "mm")
    ) +
    ggraph::scale_edge_colour_gradientn(colours = viridis::viridis(256),
                                       guide = "none") +
    ggraph::scale_edge_alpha(limits = c(0, 1), guide = "none") +
    ggraph::geom_node_point(
      ggplot2::aes(colour = .data$res_fac, size = .data$size)
    ) +
    ggplot2::scale_size(range = node_size_range) +
    ggraph::geom_node_text(
      ggplot2::aes(label = .data$cluster),
      size = node_text_size, colour = "black"
    ) +
    ggraph::theme_graph(base_family = "") +
    ggplot2::labs(colour = prefix, size = "Size")
}
