queryList <- parseQueryString(isolate(session$clientData$url_search))
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

if (is.na(dataDir) || length(dataDir) == 0 || !nzchar(dataDir)) {
  showModal(modalDialog(
    title = "No data path found",
    "No valid data directory could be found. Please check your URL or session parameters."
  ))
  stopApp(returnValue = invisible())
}

if (!file.exists(dataDir)) {
  showModal(modalDialog(
    title = "Something went wrong",
    paste(
      "It looks like either the dataset you're looking for",
      "doesn't exist, or has not finished being processed."
    )
  ))
  stopApp(returnValue = invisible())
}

tryCatch({
  pq_dir <- file.path(dataDir, "parquet")
  pq_manifest <- file.path(pq_dir, "_manifest.json")

  if (!file.exists(pq_manifest)) {
    showModal(modalDialog(
      title = "Parquet data not found",
      paste(
        "This dataset does not contain Parquet output.",
        "Please re-run the MARMOT pipeline to generate it."
      )
    ))
    stopApp(returnValue = invisible())
  }

  waiter <- waiter::Waiter$new(color = "#96B3D2", fadeout = TRUE)
  waiter$show()
  on.exit(waiter$hide())

  message("Loading data from Parquet...")
  files <- MARMOT::load_parquet_for_shiny(pq_dir)

  # Dataset size metadata — used for adaptive debounce, rasterisation, subsampling
  files$sorted_markers_cache <- gtools::mixedsort(rownames(files$sce))
  files$ncell               <- ncol(files$sce)
  files$rasterise_auto      <- files$ncell > 150000L
  files$fp_subsample_n      <- if (files$ncell > 50000L) 50000L else NULL

  # ── Top marker table (Excel, written by pipeline) ──
  topMarkerPath <- file.path(dataDir, "../", "Excel_Files/topMarkerTable.xlsx")
  if (file.exists(topMarkerPath)) {
    topMarkerTable <- readxl::read_xlsx(topMarkerPath) |> dplyr::arrange(gtools::mixedorder(Cluster))
    files$topMarkerTable <- topMarkerTable
  }

  # ── FCS frame objects (stored as qs — not decomposable to Parquet) ──
  framesListPath <- file.path(dataDir, "framesList.qs")
  if (file.exists(framesListPath)) {
    files$framesList <- qs::qread(framesListPath, nthreads = 4)
  }

  # ── Load into reactive values ──
  inputDataReactive <- reactiveValues(Results = files)

}, error = function(e) {
  stopApp(paste("An error occurred:", e$message))
})
