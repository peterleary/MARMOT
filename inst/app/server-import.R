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
  # dataDir <- system.file("examples/R_files/", package = "MARMOT")
  dataDir <- "/Users/peterleary/Desktop/FGCZ/MARMOT/Data/Paper/Results_Files_2025-06-30_16.45.46/R_files/"
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
  sce_file <- file.path(dataDir, "sce.qs")
  if (!file.exists(sce_file)) {
    showModal(modalDialog(
      title = "The file does not exist", 
      "Either the analysis has not yet finished running, you have made a mistake in the URL, or you have not pointed to any dataset. Please try again! If the issue persists, email peter.leary@uzh.ch"
    ))
    stopApp(returnValue = invisible())
  }
  
  waiter <- waiter::Waiter$new(color = "#96B3D2", fadeout = TRUE)
  waiter$show()
  on.exit(waiter$hide())
  
  filesToLoad <- c(
    "md.qs", "clusteringMethodToUse.qs", "sce.qs", "coloursList.qs", "smd.qs", 
    "umapDFList.qs", "selectedClustersList.qs", "frames.qs", "scData.qs"
  )
  
  files <- purrr::map(filesToLoad, ~{
    f <- file.path(dataDir, .x)
    if (file.exists(f)) qs::qread(f, nthreads = 4) else NULL
  }) |> setNames(gsub("\\.qs$", "", filesToLoad))
  
  # Set useful variables
  md <- files$md
  sce <- files$sce
  clusteringMethodToUse <- files$clusteringMethodToUse
  exprsToUse <- "exprsTransformed"
  
  conditions <- setdiff(colnames(md), c("file_name", "sample_id", "condition"))
  files$conditions <- gsub("-", ".", c("condition", conditions))
  
  files$mergeBy <- switch(
    clusteringMethodToUse,
    "Rphenograph" = "k",
    "FastPG"      = "k",
    "PARC"        = "p",
    "FlowSOM"     = "meta"
  )
  
  ## Top lineage/state marker table
  if (file.exists(file.path(dataDir, "../", "Excel_Files/topMarkerTable.xlsx"))) {
    topMarkerTable <- readxl::read_xlsx(file.path(dataDir, "../", "Excel_Files/topMarkerTable.xlsx")) |> dplyr::arrange(gtools::mixedorder(Cluster))
    files$topMarkerTable <- topMarkerTable
  }
  
  if (!"scData" %in% names(files)) {
    # Seurat conversion and scaling
    files$scData <- suppressWarnings(Seurat::as.Seurat(sce, counts = exprsToUse, data = "exprsQuantNorm"))
    files$scData <- Seurat::ScaleData(files$scData, assay = "originalexp", verbose = FALSE)
    files$scData <- scData
  }
  files$scDataToFP <- files$scData
  
  # Load into reactive values
  inputDataReactive <- reactiveValues(Results = files)
  
}, error = function(e) {
  stopApp(paste("An error occurred:", e$message))
})


