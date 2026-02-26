# Plot Helpers
# Shared ggplot2 theme, geom, and rendering utilities for MARMOT Shiny app

#' Standard MARMOT DR plot theme
#' @param base_size Base font size
#' @param show_axes Logical: show axis titles/text
#' @param legend_position "right", "bottom", or "none"
#' @return A list of ggplot2 theme elements
marmot_dr_theme <- function(base_size = 12, show_axes = FALSE, legend_position = "right") {
  th <- ggprism::theme_prism(base_size = base_size) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      legend.text = element_text(size = base_size * 0.8, face = "bold"),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5),
      legend.key.width = unit(0.4, "cm"),
      legend.position = legend_position
    )
  if (!show_axes) {
    th <- th + theme(
      axis.title = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
    )
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
#' @param border Logical: show density borders (MASS::kde2d 3-layer sandwich)
#' @param border_size Multiplier for border point size (default 2.0)
#' @param border_density Quantile threshold for border cell selection (default 0.35)
#' @param border_colour Colour for border points (default "black")
#' @param base_size Font base size
#' @param show_axes Logical: show axes
#' @param legend_position Legend position string
#' @return A ggplot object
make_feature_scatter <- function(df, marker, palette = "viridis", direction = 1,
                                 point_size = 0.8, alpha = 1,
                                 rasterise = FALSE, raster_dpi = 1024,
                                 border = FALSE, border_size = 2.0, border_density = 0.35,
                                 border_colour = "black",
                                 base_size = 14, show_axes = FALSE,
                                 legend_position = "right") {
  if (!marker %in% colnames(df)) {
    warning("Marker '", marker, "' not found in data frame")
    return(ggplot())
  }
  # Sort so high values plot on top
  df <- df[order(df[[marker]], decreasing = FALSE), ]

  p <- ggplot(df, aes(x = .data[["x"]], y = .data[["y"]]))

  if (rasterise) {
    p <- p + scattermore::geom_scattermore(
      pointsize = (point_size * 2) + 0.6,
      pixels = c(raster_dpi, raster_dpi),
      alpha = alpha,
      aes(colour = .data[[marker]])
    )
    p <- apply_continuous_scale(p, palette, direction, "colour")
  } else if (border) {
    # 3-layer sandwich: MASS::kde2d density → peripheral cell selection → dark halo + grey base + expression
    kde <- MASS::kde2d(df[["x"]], df[["y"]], n = 100L)
    ix  <- pmax(1L, pmin(findInterval(df[["x"]], kde$x), length(kde$x)))
    iy  <- pmax(1L, pmin(findInterval(df[["y"]], kde$y), length(kde$y)))
    cell_density <- kde$z[cbind(ix, iy)]
    border_df <- df[cell_density < quantile(cell_density, border_density), ]
    p <- p +
      geom_point(data = border_df,
                 size = point_size * border_size, colour = border_colour,
                 show.legend = FALSE) +
      geom_point(colour = "grey75", size = point_size, show.legend = FALSE) +
      geom_point(aes(colour = .data[[marker]]), size = point_size)
    p <- apply_continuous_scale(p, palette, direction, "colour")
  } else {
    p <- p + geom_point(
      aes(colour = .data[[marker]]),
      size = point_size, alpha = alpha
    )
    p <- apply_continuous_scale(p, palette, direction, "colour")
  }

  p <- p + marmot_dr_theme(base_size, show_axes, legend_position) +
    coord_fixed() +
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
#' @return A ggplot object
make_violin_plot <- function(df, marker, group_col, split_col = NULL,
                             colours = NULL, point_size = 0, base_size = 14) {
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

  p <- p + geom_violin(scale = "width", trim = TRUE)

  if (point_size > 0) {
    p <- p + geom_jitter(width = 0.2, size = point_size, alpha = 0.3)
  }

  if (!is.null(colours)) {
    p <- p + scale_fill_manual(values = colours)
  }

  p <- p +
    theme_classic(base_size = base_size) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    labs(y = marker, x = NULL) +
    ggtitle(marker)

  p
}

#' Create a dot plot (replaces SCpubr::do_DotPlot)
#' @param avg_expr Matrix: groups × markers (mean expression)
#' @param pct_expr Matrix: groups × markers (percent expressed)
#' @param palette Colour palette name
#' @param direction Palette direction
#' @param dot_scale Scaling factor for dot size
#' @param flip Logical: flip axes
#' @param base_size Font size
#' @return A ggplot object
make_dot_plot <- function(avg_expr, pct_expr, palette = "viridis", direction = 1,
                          dot_scale = 10, flip = TRUE, base_size = 14) {
  # Reshape to long format
  df <- expand.grid(
    group = rownames(avg_expr),
    marker = colnames(avg_expr),
    stringsAsFactors = FALSE
  )
  df$avg_expr <- as.vector(avg_expr)
  df$pct_expr <- as.vector(pct_expr)

  p <- ggplot(df, aes(
    x = .data[["marker"]],
    y = .data[["group"]],
    size = .data[["pct_expr"]],
    colour = .data[["avg_expr"]]
  )) +
    geom_point() +
    scale_size_continuous(range = c(0, dot_scale), name = "% Expressing") +
    theme_classic(base_size = base_size) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
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
#' @return A ggplot object
make_ridge_plot <- function(df, marker, group_col, colours = NULL, base_size = 14) {
  p <- ggplot(df, aes(
    x = .data[[marker]],
    y = .data[[group_col]],
    fill = .data[[group_col]]
  )) +
    ggridges::geom_density_ridges(scale = 1.2, alpha = 0.7) +
    theme_classic(base_size = base_size) +
    theme(legend.position = "none") +
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
