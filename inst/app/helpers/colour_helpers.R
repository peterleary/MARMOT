# Colour Helpers
# Colour palette definitions and scale management for MARMOT Shiny app

# CATALYST-style colour palette
catalystCols <- c(
  "#DC050C", "#FB8072", "#1965B0", "#7BAFDE", "#882E72", "#B17BA6",
  "#FF7B00", "#FDC362", "#E7298A", "#E78AC3", "#33A02C", "#B2DF8A",
  "#55A1B1", "#8DD3C7", "#A6761D", "#E6AB02", "#7570B3", "#BEAED4",
  "#666666", "#999999", "#aa8282", "#d4b7b7", "#8600bf", "#ba5ce3",
  "#808000", "#aeae5c", "#1e90ff", "#00bfff", "#56ff0d", "#ffff00"
)
cc2 <- colorspace::darken(catalystCols, 0.4)
catalystCols <- paste0(c(catalystCols, cc2), "FF")

chameleonCols <- chameleon::distinct_colors(
  n = 42, minimal_saturation = 30,
  minimal_lightness = 10, maximal_lightness = 100
)$name

brewerCols <- c(
  RColorBrewer::brewer.pal(12, "Paired"),
  RColorBrewer::brewer.pal(8, "Dark2"),
  RColorBrewer::brewer.pal(8, "Set2")
)
bb2 <- colorspace::darken(brewerCols, 0.4)
brewerCols <- c(brewerCols, bb2)

# Named colour scale categories
viridisColours <- c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")
scicoColours <- c("bam", "berlin", "brocO", "corkO", "lapaz", "lisbon", "romaO", "vikO")
divergingColours <- rownames(
  RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == "div", ]
)

#' Apply a continuous colour scale to a ggplot
#' @param p A ggplot object
#' @param palette Character name of the palette
#' @param direction 1 or -1 for scale direction
#' @param aesthetic "colour" or "fill"
#' @return Modified ggplot with colour scale applied
apply_continuous_scale <- function(p, palette, direction = 1, aesthetic = "colour") {
  scale_fn <- if (aesthetic == "colour") {
    list(
      viridis = function() scale_colour_viridis_c(option = palette, direction = direction),
      scico   = function() scico::scale_colour_scico(palette = palette, direction = direction),
      diverging = function() scale_colour_distiller(palette = palette, direction = direction, type = "div")
    )
  } else {
    list(
      viridis = function() scale_fill_viridis_c(option = palette, direction = direction),
      scico   = function() scico::scale_fill_scico(palette = palette, direction = direction),
      diverging = function() scale_fill_distiller(palette = palette, direction = direction, type = "div")
    )
  }

  if (palette %in% viridisColours) {
    p + scale_fn$viridis()
  } else if (palette %in% scicoColours) {
    p + scale_fn$scico()
  } else if (palette %in% divergingColours) {
    p + scale_fn$diverging()
  } else {
    p
  }
}

#' Initialize the colour palette list as a reactiveValues object
#' @param sce A SingleCellExperiment object
#' @return A reactiveValues object with named colour palettes
init_colour_palette_list <- function(sce) {
  reactiveValues(
    "Catalyst"   = catalystCols,
    "Seurat"     = scales::hue_pal()(length(unique(sce$cluster_id))),
    "Chameleon"  = chameleonCols,
    "Alphabet"   = as.character(pals::alphabet(n = 26)),
    "Alphabet2"  = as.character(pals::alphabet2(n = 26)),
    "Cols25"     = as.character(pals::cols25(n = 25)),
    "Glasbey"    = as.character(pals::glasbey(n = 32)),
    "Kelly"      = as.character(pals::kelly(n = 22)),
    "Polychrome" = as.character(pals::polychrome(n = 36)),
    "Brewer"     = brewerCols
  )
}
