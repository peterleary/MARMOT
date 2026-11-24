# The look of MARMOT's embedding plots
#
# Every UMAP, tSNE, PCA and PaCMAP plot, in the report and the Explorer, is
# drawn the same way: no axes, a fixed aspect ratio (one unit across is one
# unit up), and two small arrows in the bottom-left corner naming the axes,
# as on the manuscript figures.

#' The axis names of an embedding
#'
#' @param method The embedding's name, as in the results (`"UMAP"`,
#'   `"Downsampled.TSNE"`, `"UMAP_uncorrected"`, `"PCA"`, `"Mpacmap"`, ...).
#' @return Two names, such as `c("UMAP1", "UMAP2")` or `c("PC1", "PC2")`.
#' @export
dr_axis_names <- function(method) {
  base <- sub("[_.]uncorrected$", "", sub("^Downsampled\\.", "", as.character(method)[1]))
  stem <- switch(toupper(base), UMAP = "UMAP", TSNE = "tSNE", PCA = "PC",
                 MPACMAP = "PaCMAP", PACMAP = "PaCMAP", base)
  paste0(stem, 1:2)
}

#' Corner arrows for an embedding plot
#'
#' Two arrows in the bottom-left corner, just outside the cells, labelled with
#' the axis names. Both are 15% of the horizontal range long, so they look the
#' same length on the fixed-ratio plot this adds. Add the result to a ggplot
#' with `+`.
#'
#' @param emb The embedding the plot shows: a two-column matrix or data frame
#'   (or one with columns `x` and `y`). Use every cell of the map, not a
#'   filtered or split part, so the arrows stay put when groups are hidden.
#' @param axis_names The two labels, e.g. from [dr_axis_names()].
#' @param fixed Add `coord_fixed()` (default `TRUE`). The arrows only look
#'   equal with it.
#' @return A list of ggplot2 layers.
#' @export
dr_corner_arrows <- function(emb, axis_names = c("UMAP1", "UMAP2"), fixed = TRUE) {
  x <- if (all(c("x", "y") %in% colnames(emb))) emb[["x"]] else emb[, 1]
  y <- if (all(c("x", "y") %in% colnames(emb))) emb[["y"]] else emb[, 2]
  xr <- range(x, na.rm = TRUE); yr <- range(y, na.rm = TRUE)
  ax_len <- 0.15 * diff(xr)
  x0 <- xr[1] - 0.02 * diff(xr); y0 <- yr[1] - 0.02 * diff(yr)
  hd <- grid::arrow(length = grid::unit(2.5, "mm"), type = "closed")
  c(list(
    ggplot2::annotate("segment", x = x0, y = y0, xend = x0 + ax_len, yend = y0, linewidth = 0.6, arrow = hd),
    ggplot2::annotate("segment", x = x0, y = y0, xend = x0, yend = y0 + ax_len, linewidth = 0.6, arrow = hd),
    ggplot2::annotate("text", x = x0 + ax_len / 2, y = y0, label = axis_names[1], vjust = 1.6, size = 3),
    ggplot2::annotate("text", x = x0, y = y0 + ax_len / 2, label = axis_names[2], angle = 90, vjust = -0.6, size = 3)),
    if (fixed) list(ggplot2::coord_fixed()))
}
