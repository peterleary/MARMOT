#' @title plotUMAPNice
#' @description This is our little custom function for drawing nice and customisable DR plots 
#' @return A ggplot object
#' @author Peter Leary
#' @export
#' @import Rcpp
#' @importFrom Rcpp evalCpp
plotUMAPNice <- function(
    df = umapDF, 
    colour_by = "condition", 
    show_density = FALSE, 
    size = 0.7, 
    alpha = 1,
    text_size = 12, 
    facet_by = NULL, 
    show_grids = FALSE, 
    name = "Normalised\nExpression\n", 
    show_cluster_labels = FALSE, 
    cluster_labels = 'cluster_id',
    dimRed = dimRedMethodToUse,
    markers = useMarkers,
    colList = coloursList,
    pal = viridisColour
) {
  gp1 <- ggplot(df, aes_string("x", "y", colour = colour_by))
  if (show_density) {
    gp1 <- gp1 + 
      geom_density_2d(
        data = df[,c("x","y")],
        aes(x = x, y = y),
        colour = "lightgrey",
        size = 0.5,
        bins = 20,
        alpha = 0.8) + 
      ylim(min(df$y[!is.na(df$y)])-1, max(df$y[!is.na(df$y)])+1) +
      xlim(min(df$x[!is.na(df$x)])-1, max(df$x[!is.na(df$x)])+1)
  }
  gp1 <- gp1 + 
    geom_point(size = size, alpha = alpha, shape = 16) + 
    labs(x = paste(dimRed, "dim 1"), y = paste(dimRed, "dim 2")) + 
    ggtitle(colour_by) +
    theme_prism() +
    theme(
      axis.text = element_blank(), 
      axis.ticks = element_blank(), 
      axis.line = element_blank(), 
      legend.text = element_text(size = 10), 
      panel.border = element_rect(colour = "black", fill = NA, size = 1.5))
  if (colour_by == "condition") {
    gp1 <- gp1 + guides(colour = guide_legend(override.aes = list(size = 2)))
  }
  if (!show_grids) {
    gp1 <- gp1 + 
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  }
  if (colour_by %in% names(markers)) {
    gp1 <- gp1 +
      scale_colour_gradientn(name, colors = hcl.colors(64, pal, rev = FALSE))
  } else {
    gp1 <- gp1 +
      scale_colour_manual(values = colList[[colour_by]])
  }
  if (!is.null(facet_by)) {
    gp1 <- gp1 +
      facet_wrap(~ df[[facet_by]])
  }
  if (show_cluster_labels) {
    gp1 <- Seurat::LabelClusters(plot = gp1, id = cluster_labels, color = "black", box = T)
  }
  return(gp1)
}
