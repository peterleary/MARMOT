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
colsList1 <- inputDataReactive[["Results"]][["coloursList"]][inputDataReactive[["Results"]][["conditions"]]]
colsList1 <- colsList1[!sapply(colsList1,is.null)]

input <- list(
  # Citation acceptance
  acceptCite = TRUE,
  
  # Download options
  dlFormat = "PDF",
  pngRes = 600,
  
  # UMAP plot settings
  umapDRToPlot = "UMAP",
  umapColumnToPlot = "cluster_id",
  umapColumnToSplit = "None",
  umapContrastToUse = inputDataReactive$Results$smd$`Conditions To Test`[[1]],
  umapShowLabels = TRUE,
  umapShowAxes = FALSE,
  umapLegendPosition = "Right",
  pointSizeUMAP = 1.2,
  textSizeUMAP = 10,
  pointAlphaUMAP = 0.8,
  pointBorderUMAP = TRUE,
  borderSizeUMAP = 0.4,
  umapBorderColour = "black",
  labelSizeUMAP = 3,
  labelShiftUMAP = 2,
  umapShowDAClusters = "None",
  umapMainNcol = 2,
  figWidthUMAP = 600,
  figHeightUMAP = 600,
  
  # Feature Plot settings
  featurePlotType = "Feature Plot",
  fpAssayToPlot = "data",
  fpDRToPlot = "UMAP",
  fpFeatureToPlot = c("MHCII", "CD19"),
  excludeBucketFP = character(0),
  fpColumnToPlot = "condition",
  fpColumnToSplit = "None",
  textSizeFP = 10,
  pointSizeFP = 1,
  borderSizeFP = 0.2,
  cellBordersFP = TRUE,
  rasteriseFP = FALSE,
  rasterFP_DPI = 1024,
  fpLabelColour = "Gene median",
  fpShowLabels = TRUE,
  fpShowAxes = FALSE,
  fpContrastToUse = inputDataReactive$Results$smd$`Conditions To Test`[[1]],
  fpShowDAClusters = "All",
  ncolFPGene = 2,
  ncolFPSplit = 2,
  fpLegendPosition = "Bottom",
  viridisColourFP = "magma",
  flipViridisFP = FALSE,
  fpHeatmapPlotAll = FALSE,
  umapFeaturePlotDotplotFlip = TRUE,
  umapFeaturePlotHeatmapCluster = TRUE,
  umapFeaturePlotHeatmapFlip = TRUE,
  fpNebulosaPlotTogether = TRUE,
  fpNebulosaPlotTogetherOnly = FALSE,
  
  # Bucket lists
  plotByKeepBucket = c("APE", "AP"),
  plotByExcludeBucket = character(0),
  splitByKeepBucket = c("APE", "AP"),
  splutByExcludeBucket = character(0),
  
  # Barplot options
  fpBarplotPercentage = FALSE,
  fpBarplotShowNumbers = TRUE,
  
  # Export dimensions
  figWidthFP = 600,
  figHeightFP = 600,
  
  # Modal file uploads (mocked)
  importFile = data.frame(datapath = "mock/path/cluster_file.xlsx", stringsAsFactors = FALSE)
)

input$clusterLabelTable_cell_edit <- data.frame(row = c(1, 2), col = c(1, 1), value = c("hi", "hi"))
importedDf <- read_xlsx("~/Desktop/FGCZ/MARMOT/Data/Paper/Results_Files_2025-06-30_16.45.46/R_files/clusterInfos.xlsx")
importedDf <- importedDf %>% data.frame(check.names = F) %>% column_to_rownames("original")
clusterTableReactive <- reactiveValues(table = NULL)
clusterTableReactive$table <- data.frame(
  "cluster_id" = levels(inputDataReactive$Results[["sce"]]@colData$cluster_id),
  "relabelled_clusters" = levels(inputDataReactive$Results[["sce"]]@colData$cluster_id),
  "colours" = inputDataReactive$Results$coloursList$cluster_id[match(levels(inputDataReactive$Results[["sce"]]@colData$cluster_id), names(inputDataReactive$Results$coloursList$cluster_id))]
)
rownames(clusterTableReactive$table) <- NULL
clusterTableReactive$table <- column_to_rownames(clusterTableReactive$table, "cluster_id")
inputDataReactive$Results$coloursList[["relabelled_clusters"]] <- inputDataReactive$Results$coloursList$cluster_id