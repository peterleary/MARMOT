queryList = parseQueryString(session$clientData$url_search) 
if (is.list(queryList)){
  dataUrl <- queryList$data
} else {
  dataUrl <- NULL
}

if (!is.null(dataUrl)) {
  urlDataRoot = c("/srv/gstore/projects", "/srv/GT/analysis/course_sushi/public/gstore/projects")
  dataDir <- file.path(urlDataRoot, dataUrl)
  dataDir <- dataDir[file.exists(dataDir)][1]
} else if (is.null(dataUrl) & !exists("marmot_output")) {
  # dataDir <- "/srv/GT/analysis/peter/MARMOT_Paper/Results_Files_2024-10-29_09.12.41/R_files/" # use this one 
  dataDir <- "~/Desktop/FGCZ/MARMOT/files/Results_Files_2025-02-06_14.42.22/R_files/"
}

# 2025-01-29: Read in local proteomics file if specified 
if(exists("marmot_output")) {
  dataDir <- marmot_output
}

if(!file.exists(dataDir)) {
  showModal(modalDialog(
    title = "Something went wrong",
    "It looks like either the dataset you're looking for doesn't exist, or has not finished being processed in SUSHI yet."
  ))
  stopApp(returnValue = invisible())
}

tryCatch({
  if(file.exists(file.path(dataDir, "sce.qs"))) {
    waiter <- waiter::Waiter$new(color = "#96B3D2", fadeout = TRUE)
    waiter$show()
    on.exit(waiter$hide())
    
    filesToLoad <- c(
      "md.qs", "clusteringMethodToUse.qs", "sce.qs", "coloursList.qs", "smd.qs", 
      "umapDFList.qs", "downsampleTo.qs", "selectedClustersList.qs")
    files <- setNames(lapply(filesToLoad, function(x) {
      qs::qread(file = file.path(dataDir, x), nthreads = 4)
    }), (filesToLoad %>% gsub("\\.qs", "", .)))
    
    conditions <- c("condition", colnames(files$md)[!colnames(files$md) %in% c("file_name", "sample_id", "condition")])
    conditions <- gsub("-", ".", conditions)
    mergeBy <- switch(
      files$clusteringMethodToUse,
      "Rphenograph" = "k",
      "FastPG" = "k",
      "PARC" = "p",
      "FlowSOM" = "meta"
    )
    sce <- files$sce
    exprsToUse <- "exprsTransformed"
    clusteringMethodToUse <- files$clusteringMethodToUse
    pET <- plotExprHeatmap(x = sce, assay = exprsToUse, features = "type", by = "cluster_id", k = clusteringMethodToUse)
    pETdf <- suppressWarnings(pET@matrix[row_order(pET),])
    topLineageTable <- data.frame("Cluster" = paste("Cluster", rownames(pETdf)))
    topLineageTable$Top_Lineage_Markers <- NA
    for (i in seq_along(1:nrow(topLineageTable))) {
      topLineageTable$Top_Lineage_Markers[i] <- paste(names(head(sort(pETdf[i,] %>% .[. > 0.4], decreasing = T), n = 2)), collapse = " ")
    }
    topLineageTable <- topLineageTable[mixedorder(topLineageTable$Cluster), ]
    
    pES <- plotExprHeatmap(x = sce, features = "state", by = "cluster_id", k = clusteringMethodToUse)
    pESdf <- suppressWarnings(pES@matrix[row_order(pES),])
    topStateTable <- data.frame("Cluster" = paste("Cluster", rownames(pESdf)))
    topStateTable$Top_State_Markers <- NA
    for (i in seq_along(1:nrow(topStateTable))) {
      topStateTable$Top_State_Markers[i] <- paste(names(head(sort(pESdf[i,], decreasing = T), n = 3)), collapse = " ")
      topStateTable$Bottom_State_Markers[i] <- paste(names(tail(sort(pESdf[i,], decreasing = T), n = 3)), collapse = " ")
    }
    topStateTable <- topStateTable[mixedorder(topStateTable$Cluster), ]
    topMarkerTable <- left_join(topLineageTable, topStateTable, by = "Cluster")
    
    files[["conditions"]] <- conditions
    files[["mergeBy"]] <- mergeBy
    files[["topLineageTable"]] <- topLineageTable
    files[["topStateTable"]] <- topStateTable
    files[["topMarkerTable"]] <- topMarkerTable
    
    # Make the scData the same length as the main DRs 
    if(!is.null(files$smd$`Conditions Order`)) {
      conditionOrder <- files$smd$`Conditions Order`
      conditionOrder <- conditionOrder[!is.na(conditionOrder)]
    }
    drCellPerCondition <- files$smd$`Cells per condition in UMAPs etc.`[!is.na(files$smd$`Cells per condition in UMAPs etc.`)]
    drCellPerCondition <- unlist(setNames(lapply(seq_along(drCellPerCondition), function(i) {
      eval(parse(text = drCellPerCondition[[i]]))
    }), conditionOrder))
    if (length(drCellPerCondition) >= 2) {
      cellsToKeep <- unlist(lapply(levels(colData(sce)$sample_id), function(x) {
        cells <- grep(x, sce@colData$sample_id)
        dst <- as.numeric(drCellPerCondition[as.character(files$md$condition[files$md$sample_id == x])])
        set.seed(42)
        if (length(cells) > dst) {
          idx <- sample(length(cells), dst)
          cells <- cells[idx]
        }
        cells
      }))
      sce <- sce[, cellsToKeep]
    }
    
    files[["scData"]] <- suppressWarnings(Seurat::as.Seurat(x = sce, counts = "exprsTransformed", data = "exprsQuantNorm"))
    files[["scData"]] <- Seurat::ScaleData(files[["scData"]], assay = "originalexp", verbose = FALSE)
    
    inputDataReactive <- reactiveValues(Results = NULL)
    inputDataReactive[["Results"]] = files
      
  } else {
    showModal(modalDialog(
      title = "The file does not exist", 
      "Either the analysis has not yet finished running, you have made a mistake in the URL, or you have not pointed to any dataset. Please try again! If the issue persists, email peter.leary@uzh.ch"
    ))
    stopApp(returnValue = invisible())
  }

  
}, error = function(e) {
  stopApp(paste("An error occurred:", e$message))
})


