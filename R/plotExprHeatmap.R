# Default cluster colour palette (matches CATALYST internal .cluster_cols)
.marmot_cluster_cols <- c(
  "#DC050C", "#FB8072", "#1965B0", "#7BAFDE", "#882E72",
  "#B17BA6", "#FF7F00", "#FDB462", "#E7298A", "#E78AC3",
  "#33A02C", "#B2DF8A", "#55A1B1", "#8DD3C7", "#A6761D",
  "#E6AB02", "#7570B3", "#BEAED4", "#666666", "#999999",
  "#aa8282", "#d4b7b7", "#8600bf", "#ba5ce3", "#808000",
  "#aeae5c", "#1e90ff", "#00bfff", "#56ff0d", "#ffff00"
)

#' Expression Heatmap
#'
#' Create a heatmap of expression values with customizable clustering and annotations.
#' This function masks the CATALYST::plotExprHeatmap function but removes the factor relevelling
#'
#' @author Helena Crowell (original CATALYST implementation)
#'
#' @param x A SingleCellExperiment object
#' @param features Character vector of features to include. If NULL, uses all features.
#' @param by Character specifying grouping: "sample_id", "cluster_id", or "both"
#' @param k Character specifying clustering to use (default: "meta20")
#' @param m Character specifying metaclustering (optional)
#' @param assay Character specifying which assay to use (default: "exprs")
#' @param fun Character specifying aggregation function: "median", "mean", or "sum"
#' @param scale Character specifying when to scale: "first", "last", or "never"
#' @param q Numeric quantile for scaling (default: 0.01)
#' @param row_anno Logical or character vector for row annotations (default: TRUE)
#' @param col_anno Logical or character vector for column annotations (default: TRUE)
#' @param row_clust Logical for row clustering (default: TRUE)
#' @param col_clust Logical for column clustering (default: TRUE)
#' @param row_dend Logical for row dendrogram display (default: TRUE)
#' @param col_dend Logical for column dendrogram display (default: TRUE)
#' @param bars Logical for frequency bars (default: FALSE)
#' @param perc Logical for percentage display (default: FALSE)
#' @param bin_anno Logical for binary annotation display (default: FALSE)
#' @param hm_pal Colour palette for heatmap (default: rev(RColorBrewer::brewer.pal(11, "RdYlBu")))
#' @param k_pal Colour palette for cluster annotations (default: .marmot_cluster_cols)
#' @param m_pal Colour palette for metacluster annotations (default: k_pal)
#' @param distance Character specifying distance metric for clustering
#' @param linkage Character specifying linkage method for clustering
#'
#' @return A ComplexHeatmap object
#'
#' @note This function masks CATALYST::plotExprHeatmap. Load this package after CATALYST 
#'       or use \code{conflicted} package to manage function conflicts.
#'
#' @import ComplexHeatmap
#' @import RColorBrewer
#' @import circlize
#' @import grid
#' @import SummarizedExperiment
#' @import CATALYST
#' @importFrom stats quantile
#' @importFrom grDevices colorRampPalette
#'
#' @export
plotExprHeatmap <- function(x, features = NULL, by = c("sample_id", "cluster_id", 
                                                       "both"), k = "meta20", m = NULL, assay = "exprs", fun = c("median", 
                                                                                                                 "mean", "sum"), scale = c("first", "last", "never"), q = 0.01, 
                            row_anno = TRUE, col_anno = TRUE, row_clust = TRUE, col_clust = TRUE, 
                            row_dend = TRUE, col_dend = TRUE, bars = FALSE, perc = FALSE, 
                            bin_anno = FALSE, hm_pal = rev(RColorBrewer::brewer.pal(11, "RdYlBu")), 
                            k_pal = .marmot_cluster_cols, m_pal = k_pal, distance = c("euclidean", 
                                                                                         "maximum", "manhattan", "canberra", "binary", "minkowski"), 
                            linkage = c("average", "ward.D", "single", "complete", "mcquitty", 
                                        "median", "centroid", "ward.D2")) 
{
  args <- as.list(environment())
  CATALYST:::.check_args_plotExprHeatmap(args)
  distance <- match.arg(distance)
  linkage <- match.arg(linkage)
  scale <- match.arg(scale)
  fun <- match.arg(fun)
  by <- match.arg(by)
  x <- x[unique(CATALYST:::.get_features(x, features)), ]
  # if (by != "sample_id") {
  #   CATALYST:::.check_k(x, k)
  #   x$cluster_id <- CATALYST::cluster_ids(x, k)
  # }
  if (by == "both") 
    by <- c("cluster_id", "sample_id")
  .do_agg <- function() {
    z <- CATALYST:::.agg(x, by, fun, assay)
    if (length(by) > 1) {
      z <- do.call("rbind", z)
      rownames(z) <- levels(x$cluster_id)
    }
    return(z)
  }
  .do_scale <- function() {
    if (scale == "first") {
      z <- SummarizedExperiment::assay(x, assay)
      z <- CATALYST:::.scale_exprs(z, 1, q)
      SummarizedExperiment::assay(x, assay, FALSE) <- z
      return(x)
    }
    else CATALYST:::.scale_exprs(z, 1, q)
  }
  z <- switch(scale, first = {
    x <- .do_scale()
    .do_agg()
  }, last = {
    z <- .do_agg()
    .do_scale()
  }, never = {
    .do_agg()
  })
  if (length(by) == 1) 
    z <- t(z)
  if (scale != "never" && !(assay == "counts" && fun == "sum")) {
    qs <- round(quantile(z, c(0.01, 0.99)) * 5)/5
    lgd_aes <- list(at = seq(qs[1], qs[2], 0.2))
  }
  else lgd_aes <- list()
  lgd_aes$title_gp <- grid::gpar(fontsize = 10, fontface = "bold", 
                                 lineheight = 0.8)
  sids <- levels(droplevels(factor(x$sample_id)))
  if (!isFALSE(row_anno)) {
    left_anno <- switch(by[1], sample_id = CATALYST:::.anno_factors(x, 
                                                                    sids, row_anno, "row"), CATALYST:::.anno_clusters(x, k, m, 
                                                                                                                      k_pal, m_pal))
  }
  else left_anno <- NULL
  if (!isFALSE(col_anno) && length(by) == 2) {
    top_anno <- CATALYST:::.anno_factors(x, sids, col_anno, "colum")
  }
  else top_anno <- NULL
  if (bars) {
    right_anno <- CATALYST:::.anno_counts(x[[by[1]]], perc)
  }
  else right_anno <- NULL
  if (bin_anno) {
    cell_fun <- function(j, i, x, y, ...) grid::grid.text(gp = grid::gpar(fontsize = 8), 
                                                          sprintf("%.2f", z[i, j]), x, y)
  }
  else cell_fun <- NULL
  a <- ifelse(assay == "exprs", "expression", assay)
  f <- switch(fun, median = "med", fun)
  hm_title <- switch(scale, first = sprintf("%s %s\n%s", fun, 
                                            "scaled", a), last = sprintf("%s %s\n%s", "scaled", 
                                                                         fun, a), never = paste(fun, a, sep = "\n"))
  if (length(by) == 2) {
    col_title <- features
  }
  else if (length(features) == 1 && features %in% c("type", 
                                                    "state")) {
    col_title <- paste0(features, "_markers")
  }
  else col_title <- ""
  ComplexHeatmap::Heatmap(matrix = z, name = hm_title, col = circlize::colorRamp2(seq(min(z), 
                                                                                      max(z), l = n <- 100), grDevices::colorRampPalette(hm_pal)(n)), 
                          column_title = col_title, column_title_side = ifelse(length(by) == 
                                                                                 2, "top", "bottom"), cell_fun = cell_fun, cluster_rows = row_clust, 
                          cluster_columns = col_clust, show_row_dend = row_dend, 
                          show_column_dend = col_dend, clustering_distance_rows = distance, 
                          clustering_method_rows = linkage, clustering_distance_columns = distance, 
                          clustering_method_columns = linkage, show_row_names = (is.null(left_anno) || 
                                                                                   isTRUE(by == "sample_id")) && !perc, row_names_side = ifelse(by[1] == 
                                                                                                                                                  "cluster_id" || isFALSE(row_anno) && !row_dend || 
                                                                                                                                                  isFALSE(row_clust), "left", "right"), top_annotation = top_anno, 
                          left_annotation = left_anno, right_annotation = right_anno, 
                          rect_gp = grid::gpar(col = "white"), heatmap_legend_param = lgd_aes)
}



#' Frequency Heatmap
#'
#' Create a heatmap of cluster frequencies across samples.
#' This function masks the CATALYST::plotFreqHeatmap function but removes the factor relevelling
#'
#' @author Helena Crowell (original CATALYST implementation)
#'
#' @param x A SingleCellExperiment object
#' @param k Character specifying clustering to use (default: "meta20")
#' @param m Character specifying metaclustering (optional)
#' @param normalize Logical whether to z-normalize frequencies using arcsine-square-root transformation (default: TRUE)
#' @param row_anno Logical or character vector for row annotations (default: TRUE)
#' @param col_anno Logical or character vector for column annotations (default: TRUE)
#' @param row_clust Logical for row clustering (default: TRUE)
#' @param col_clust Logical for column clustering (default: TRUE)
#' @param row_dend Logical for row dendrogram display (default: TRUE)
#' @param col_dend Logical for column dendrogram display (default: TRUE)
#' @param bars Logical for frequency bars (default: TRUE)
#' @param perc Logical for percentage display (default: FALSE)
#' @param hm_pal Colour palette for heatmap (default: rev(RColorBrewer::brewer.pal(11, "RdBu")))
#' @param k_pal Colour palette for cluster annotations (default: .marmot_cluster_cols)
#' @param m_pal Colour palette for metacluster annotations (default: k_pal)
#'
#' @return A ComplexHeatmap object
#'
#' @note This function masks CATALYST::plotFreqHeatmap. Load this package after CATALYST 
#'       or use \code{conflicted} package to manage function conflicts.
#'
#' @import ComplexHeatmap
#' @import RColorBrewer
#' @import grid
#' @import SummarizedExperiment
#' @import CATALYST
#'
#' @export
plotFreqHeatmap <- function(x, k = "meta20", m = NULL, normalize = TRUE, row_anno = TRUE, 
                            col_anno = TRUE, row_clust = TRUE, col_clust = TRUE, row_dend = TRUE, 
                            col_dend = TRUE, bars = TRUE, perc = FALSE, hm_pal = rev(RColorBrewer::brewer.pal(11, 
                                                                                                              "RdBu")), k_pal = .marmot_cluster_cols, m_pal = k_pal) 
{
  args <- as.list(environment())
  CATALYST:::.check_args_plotFreqHeatmap(args)
  # x$cluster_id <- CATALYST::cluster_ids(x, k)
  ns <- table(x$cluster_id, x$sample_id)
  fq <- prop.table(ns, 2)
  y <- as.matrix(unclass(fq))
  if (normalize) 
    y <- CATALYST:::.z_normalize(asin(sqrt(y)))
  if (!isFALSE(row_anno)) {
    left_anno <- CATALYST:::.anno_clusters(x, k, m, k_pal, m_pal)
  }
  else left_anno <- NULL
  if (!isFALSE(col_anno)) {
    sids <- levels(droplevels(factor(x$sample_id)))
    top_anno <- CATALYST:::.anno_factors(x, sids, col_anno, "colum")
  }
  else top_anno <- NULL
  if (bars) {
    right_anno <- CATALYST:::.anno_counts(x$cluster_id, perc)
  }
  else right_anno <- NULL
  ComplexHeatmap::Heatmap(matrix = y, name = paste0("normalized\n"[normalize], 
                                                    "frequency"), col = hm_pal, na_col = "lightgrey", rect_gp = grid::gpar(col = "white"), 
                          column_title = "sample_id", column_title_side = "bottom", 
                          cluster_rows = row_clust, cluster_columns = col_clust, 
                          show_row_dend = row_dend, show_column_dend = col_dend, 
                          show_row_names = is.null(left_anno), row_names_side = "left", 
                          top_annotation = top_anno, left_annotation = left_anno, 
                          right_annotation = right_anno)
}
