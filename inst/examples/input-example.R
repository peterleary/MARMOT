input <- list(
  # umapColumnToPlot = inputDataReactive$Results$conditions[[1]],
  umapColumnToPlot = "new_clusters",
  umapColumnToSplit = inputDataReactive$Results$conditions[[2]],
  umapColourPalette = "Catalyst",
  umapShowAxes = FALSE,
  umapShowLabels = TRUE,
  pointSizeUMAP = 1.5,
  pointAlphaUMAP = 0.9,
  borderSizeUMAP = 0.1,
  umapBorderColour = "black",
  textSizeUMAP = 12,
  labelSizeUMAP = 4,
  labelShiftUMAP = 0,
  figWidthUMAP = 650,
  figHeightUMAP = 600, 
  umapMainNcol = 1,
  featurePlotType = "Feature Plot",
  fpAssayToPlot = "data",
  fpFeatureToPlot = inputDataReactive$Results$panel$marker_name[1],
  fpColumnToPlot = "cluster_id",
  fpColumnToSplit = inputDataReactive$Results$conditions[[1]],
  fpShowAxes = FALSE,
  fpShowLabels = FALSE,
  cellBordersFP = TRUE,
  pointSizeFP = 1,
  borderSizeFP = 0.1,
  viridisColourFP = "mako", 
  flipViridisFP = FALSE, 
  textSizeFP = 12,
  ncolFPGene = 1,
  ncolFPSplit = 1,
  fpLabelColour = "median",
  figWidthFP = 650,
  figHeightFP = 500,
  dlFormat = "PDF",
  keepBucketFP = inputDataReactive$Results$panel$marker_name[1:2],
  fpLegendPosition = "right",
  plotByKeepBucket = levels(inputDataReactive$Results$sce$cluster_id),
  umapFeaturePlotDotplotFlip = TRUE,
  umapFeaturePlotHeatmapFlip = TRUE,
  umapFeaturePlotHeatmapCluster = TRUE,
  umapFeaturePlotHeatmapCluster = TRUE,
  fpBarplotPercentage = FALSE,
  fpBarplotShowNumbers = FALSE,
  fpDRCustomMinMax = FALSE,
  umapShowDAClusters = "Up only",
  umapContrastToUse = inputDataReactive$Results$smd$`Conditions To Test` %>% .[!is.na(.)] %>% .[3],
  importFile = "~/Desktop/FGCZ/MARMOT/clusterInfos_paper_2025-02-28.xlsx",
  fpNebulosaPlotTogether = TRUE,
  fpNebulosaPlotTogetherOnly = FALSE,
  rasteriseFP = FALSE,
  rasterFP_DPI = 1024,
  splitByKeepBucket = levels(inputDataReactive$Results$sce$condition),
  fpContrastToUse = inputDataReactive$Results$smd$`Conditions To Test` %>% .[!is.na(.)] %>% .[3],
  fpShowDAClusters = "Up only",
  fpHeatmapPlotAll = TRUE
)

catalystCols <- c(
  "#DC050C", "#FB8072", "#1965B0", "#7BAFDE", "#882E72", "#B17BA6", 
  "#FF7B00", "#FDC362", "#E7298A", "#E78AC3", "#33A02C", "#B2DF8A", 
  "#55A1B1", "#8DD3C7", "#A6761D", "#E6AB02", "#7570B3", "#BEAED4", 
  "#666666", "#999999", "#aa8282", "#d4b7b7", "#8600bf", "#ba5ce3", 
  "#808000", "#aeae5c", "#1e90ff", "#00bfff", "#56ff0d", "#ffff00"
)
cc2 <- catalystCols
cc2 <- colorspace::darken(cc2, 0.4)
catalystCols <- c(catalystCols, cc2)
catalystCols <- paste0(catalystCols, "FF")
chameleonCols <- distinct_colors(n = 42, minimal_saturation = 30, minimal_lightness = 10, maximal_lightness = 100)$name
brewerCols <- c(brewer.pal(12, "Paired"), brewer.pal(8, "Dark2"), brewer.pal(8, "Set2"))
bb2 <- colorspace::darken(brewerCols, 0.4)
brewerCols <- c(brewerCols, bb2)
viridisColours <- c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")
scicoColours <- c("bam", "berlin", "brocO", "corkO", "lapaz", "lisbon", "romaO", "vikO")
divergingColours <- rownames(brewer.pal.info[brewer.pal.info$category == "div",])
colourPaletteList <- reactiveValues(
  "Catalyst" = catalystCols,
  "Seurat" = hue_pal()(length(unique(inputDataReactive$Results$sce$cluster_id))),
  "Chameleon" = chameleonCols,
  "Alphabet" = as.character(alphabet(n = 26)),
  "Alphabet2" = as.character(alphabet2(n = 26)),
  "Cols25" = as.character(cols25(n = 25)),
  "Glasbey" = as.character(glasbey(n = 32)),
  "Kelly" = as.character(kelly(n = 22)),
  "Polychrome" = as.character(polychrome(n = 36)),
  "Brewer" = brewerCols
)

# Update inputs ----
# Get the metadata columns that can be used for plotting (i.e., discrete variables)
allCols <- colnames(colData(inputDataReactive$Results$sce))
colsThatCanBePlot <- unlist(lapply(seq_along(allCols), function(i) {
  if (length(unique(inputDataReactive$Results$sce[[allCols[i]]])) < 100) {
    allCols[i]
  }
}))

clusterTableReactive <- reactiveValues(table = NULL)
clusterTableReactive$table <- data.frame(
  "cluster_id" = levels(inputDataReactive$Results[["sce"]]@colData$cluster_id),
  "new_clusters" = levels(inputDataReactive$Results[["sce"]]@colData$cluster_id),
  "colour" = inputDataReactive$Results$coloursList$cluster_id[match(levels(inputDataReactive$Results[["sce"]]@colData$cluster_id), names(inputDataReactive$Results$coloursList$cluster_id))]
)
rownames(clusterTableReactive$table) <- NULL
clusterTableReactive$table <- column_to_rownames(clusterTableReactive$table, "cluster_id")
clusterTableReactive$table$new_clusters[match(inputDataReactive$Results[["sce"]]@colData$cluster_id, rownames(clusterTableReactive$table))] <- 
  clusterTableReactive$table$new_clusters[match(inputDataReactive$Results[["sce"]]@colData$cluster_id, rownames(clusterTableReactive$table))] %>%
  gsub("^p1$|^p2$", "test", .)

# fileSE <- system.file("examples/", package = "shinyMarmot")
importedClusters <- readxl::read_xlsx(input$importFile)
fpFeaturesToPlot <- input$keepBucketFP

input$keepBucketFP <- "CD19"

importedClusters <- reactiveValues(table = NULL)
importedClusters$table <- openxlsx::read.xlsx(input$importFile, colNames = T)
importedClusters$table <- importedClusters$table %>% data.frame(check.names = F) %>% column_to_rownames("original")
clusterTableReactive$table <- importedClusters$table
