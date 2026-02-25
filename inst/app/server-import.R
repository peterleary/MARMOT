queryList <- parseQueryString(session$clientData$url_search)
if (is.list(queryList)) {
  dataUrl <- queryList$data
} else {
  dataUrl <- NULL
}

if (!is.null(dataUrl)) {
  urlDataRoot <- c("/srv/gstore/projects", "/srv/GT/analysis/course_sushi/public/gstore/projects")
  dataDir <- file.path(urlDataRoot, dataUrl)
  dataDir <- dataDir[file.exists(dataDir)][1]
} else if (is.null(dataUrl) && !exists("marmot_output")) {
  dataDir <- system.file("examples/R_files/", package = "MARMOT")
}

# Read in local results path if specified via shinyMarmot()
if (exists("marmot_output")) {
  dataDir <- marmot_output
}

if (!file.exists(dataDir)) {
  showModal(modalDialog(
    title = "Something went wrong",
    paste(
      "It looks like either the dataset you're looking for",
      "doesn't exist, or has not finished being processed",
      "in SUSHI yet."
    )
  ))
  stopApp(returnValue = invisible())
}

tryCatch({
  # Detect data format: Parquet or qs
  data_format <- detect_data_format(dataDir)

  waiter <- waiter::Waiter$new(color = "#96B3D2", fadeout = TRUE)
  waiter$show()
  on.exit(waiter$hide())

  if (data_format == "parquet") {
    # ── Parquet loading path ──
    message("Loading data from Parquet...")
    pq_dir <- file.path(dataDir, "parquet")
    files <- MARMOT::load_parquet_for_shiny(pq_dir)

  } else {
    # ── Legacy qs loading path ──
    message("Loading data from .qs files...")

    sce_file <- file.path(dataDir, "sce.qs")
    if (!file.exists(sce_file)) {
      showModal(modalDialog(
        title = "The file does not exist",
        paste(
          "Either the analysis has not yet finished running,",
          "you have made a mistake in the URL, or you have not pointed to any dataset.",
          "Please try again! If the issue persists, email peter.leary@uzh.ch"
        )
      ))
      stopApp(returnValue = invisible())
    }

    filesToLoad <- c(
      "md.qs", "clusteringMethodToUse.qs", "sce.qs", "coloursList.qs", "smd.qs",
      "umapDFList.qs", "selectedClustersList.qs", "frames.qs"
    )

    files <- purrr::map(filesToLoad, ~{
      f <- file.path(dataDir, .x)
      if (file.exists(f)) qs::qread(f, nthreads = 4) else NULL
    }) |> setNames(gsub("\\.qs$", "", filesToLoad))

    # Set useful variables
    md <- files$md
    sce <- files$sce
    clusteringMethodToUse <- files$clusteringMethodToUse

    conditions <- setdiff(colnames(md), c("file_name", "sample_id", "condition"))
    files$conditions <- gsub("-", ".", c("condition", conditions))

    files$mergeBy <- switch(
      clusteringMethodToUse,
      "Rphenograph" = "k",
      "FastPG"      = "k",
      "PARC"        = "p",
      "FlowSOM"     = "meta"
    )
  }

  # ── Common post-load: top marker table ──
  topMarkerPath <- file.path(dataDir, "../", "Excel_Files/topMarkerTable.xlsx")
  if (file.exists(topMarkerPath)) {
    topMarkerTable <- readxl::read_xlsx(topMarkerPath) |> dplyr::arrange(gtools::mixedorder(Cluster))
    files$topMarkerTable <- topMarkerTable
  }

  # ── Load framesList for FCS export (qs only, stays as qs) ──
  framesListPath <- file.path(dataDir, "framesList.qs")
  if (file.exists(framesListPath)) {
    files$framesList <- qs::qread(framesListPath, nthreads = 4)
  }

  # ── Load into reactive values ──
  inputDataReactive <- reactiveValues(Results = files)

}, error = function(e) {
  stopApp(paste("An error occurred:", e$message))
})
