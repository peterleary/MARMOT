# ── server-import.R ──────────────────────────────────────────────────────────
# Data loading module for MARMOT Shiny app.
# Loads h5ad results with a waiter splash screen.
# ─────────────────────────────────────────────────────────────────────────────

inputDataReactive <- reactiveValues(Results = NULL)

# ── Waiter splash ────────────────────────────────────────────────────────────
loading_html <- tags$div(
  style = "text-align: center;",
  tags$img(
    src    = "MARMOT_Logo_2_bw_small.png",
    height = "120px",
    width  = "auto",
    style  = "margin-bottom: 20px; border-radius: 12px;"
  ),
  tags$br(),
  waiter::spin_fading_circles(),
  tags$br(),
  tags$h4("Loading MARMOT data...", style = "color: white; margin-top: 15px;"),
  tags$p(
    id    = "loading-delay-msg",
    "Still working! Large datasets take longer to load \u2014 please be patient.",
    style = "color: #a1a1aa; display: none; margin-top: 10px;"
  ),
  tags$script(HTML("
    setTimeout(function() {
      var msg = document.getElementById('loading-delay-msg');
      if (msg) msg.style.display = 'block';
    }, 30000);
  "))
)

w <- waiter::Waiter$new(
  html    = loading_html,
  color   = "#18181b",
  fadeout = TRUE
)

observe({
  w$show()
  on.exit(w$hide())

  tryCatch({

    # ── Resolve data directory ───────────────────────────────────────────────
    queryList <- parseQueryString(isolate(session$clientData$url_search))
    dataUrl   <- if (is.list(queryList)) queryList$data else NULL

    if (exists("marmot_output")) {
      dataDir <- marmot_output
    } else if (!is.null(dataUrl)) {
      dataDir <- dataUrl
    } else {
      dataDir <- system.file("examples/R_files/", package = "MARMOT")
    }

    # Validate
    if (is.null(dataDir) || is.na(dataDir) || !nzchar(dataDir)) {
      showModal(modalDialog(
        title = "No data path found",
        "No valid data directory could be found. Please check your URL or session parameters."
      ))
      return(invisible())
    }

    if (!file.exists(dataDir)) {
      showModal(modalDialog(
        title = "Data directory not found",
        paste("The data directory does not exist:", dataDir)
      ))
      return(invisible())
    }

    h5ad_path <- file.path(dataDir, "marmot_results.h5ad")

    if (!file.exists(h5ad_path)) {
      showModal(modalDialog(
        title = "Data not found",
        paste(
          "No marmot_results.h5ad found in", dataDir, ".",
          "Please re-run the MARMOT pipeline to generate it."
        )
      ))
      return(invisible())
    }

    # ── Load h5ad ────────────────────────────────────────────────────────────
    message("Loading data from h5ad: ", h5ad_path)
    files <- MARMOT::load_h5ad_for_shiny(h5ad_path)

    # ── Derived quantities ───────────────────────────────────────────────────
    files$sorted_markers_cache <- gtools::mixedsort(rownames(files$sce))
    files$ncell                <- ncol(files$sce)
    files$rasterise_auto       <- files$ncell > 150000L
    files$fp_subsample_n       <- if (files$ncell > 50000L) 50000L else files$ncell

    # ── Top marker table (Excel, written by pipeline) ────────────────────────
    topMarkerPath <- file.path(dataDir, "../", "Excel_Files/topMarkerTable.xlsx")
    # Optional: if it cannot be read, everything else still loads
    if (file.exists(topMarkerPath)) {
      files$topMarkerTable <- tryCatch({
        tmt <- readxl::read_xlsx(topMarkerPath)
        if ("Cluster" %in% names(tmt)) tmt[gtools::mixedorder(tmt$Cluster), ] else tmt
      }, error = function(e) {
        showNotification(paste("The top marker table could not be read, so it is left out:", conditionMessage(e)),
                         type = "warning", duration = 10)
        NULL
      })
    }

    # Kept so the FCS export can find the original FCS files
    files$dataDir <- dataDir

    # Markers' display names (Display names for markers), for figures
    files$markerDisplay <- tryCatch(marker_display_names(files$smd), error = function(e) character())

    # ── Labelled populations, as the last extension report made them ───────
    # Optional: offered as a "population" column wherever clusters are
    rec <- tryCatch(MARMOT::read_labelled_populations(dataDir), error = function(e) {
      showNotification(paste("The labelled populations could not be read, so they are left out:", conditionMessage(e)),
                       type = "warning", duration = 10)
      NULL
    })
    if (!is.null(rec)) {
      lp <- labelled_population_column(files$sce$cluster_id, rec, files$coloursList$cluster_id)
      files$sce$population <- lp$population
      if (!is.null(lp$colours)) files$coloursList$population <- lp$colours
      message("Labelled populations: ", nlevels(lp$population), " (from ", basename(dataDir), "/labelled_populations.json)")
    }

    # ── Store in reactive values ─────────────────────────────────────────────
    inputDataReactive$Results <- files
    message("MARMOT data loaded: ", files$ncell, " cells, ",
            length(files$sorted_markers_cache), " markers")

  }, marmot_results_format = function(e) {
    # Results from another MARMOT: the message says it all
    showModal(modalDialog(title = "These results can't be opened here", conditionMessage(e)))
  }, error = function(e) {
    showModal(modalDialog(
      title = "Error loading data",
      paste("An error occurred while loading MARMOT data:", e$message)
    ))
  })
})
