# ── server-colours.R ────────────────────────────────────────────────────────
# Colour palette management, input updates, marker table, and reactive
# initialisation for MARMOT Shiny app.
# Runs after server-import.R has populated inputDataReactive$Results.
# ────────────────────────────────────────────────────────────────────────────

# ── Wait for data ──────────────────────────────────────────────────────────
observeEvent(inputDataReactive$Results, once = TRUE, {
  res                  <- inputDataReactive$Results
  sce                  <- res$sce
  umapDFList           <- res$umapDFList
  coloursList          <- res$coloursList
  ncell                <- res$ncell
  sorted_markers_cache <- res$sorted_markers_cache

  # ── Colour palette list ────────────────────────────────────────────────
  colourPaletteList <<- init_colour_palette_list(sce)

  # (Adaptive debounce is handled per-module as reactives in server-dr.R
  # and server-plots.R, reading ncell from inputDataReactive$Results.)

  # ── Update DR selectInput ──────────────────────────────────────────────
  # Use umapDFList keys (what we actually plot from), not reducedDimNames
  dr_names <- names(umapDFList)
  if (length(dr_names) == 0) dr_names <- SingleCellExperiment::reducedDimNames(sce)
  dr_default <- dr_names[1]
  for (pref in c("TSNE", "UMAP", "PaCMAP")) {
    hit <- grep(pref, dr_names, ignore.case = TRUE, value = TRUE)
    if (length(hit) > 0) dr_default <- hit[1]
  }
  updateSelectInput(session, "umapDRToPlot",
    choices = dr_names, selected = dr_default)

  # ── Plottable columns (categorical, <100 levels) ──────────────────────
  colsThatCanBePlot <- get_plottable_columns(sce)

  updateSelectInput(session, "umapColumnToPlot",
    choices = colsThatCanBePlot, selected = "cluster_id")
  updateSelectInput(session, "umapColumnToSplit",
    choices = c("None", colsThatCanBePlot), selected = "None")

  # ── Contrast dropdowns ─────────────────────────────────────────────────
  # Populate from the actual contrast names in study metadata (same as main branch).
  # selectedClustersList is indexed positionally: odd = up, even = down.
  contrast_names <- na.omit(res$smd$`Conditions To Test`)
  contrast_names <- contrast_names[nzchar(contrast_names)]
  if (length(contrast_names) == 0) contrast_names <- "None"
  updateSelectInput(session, "umapContrast", choices = contrast_names)
  updateSelectInput(session, "fpContrast",   choices = contrast_names)

  # ── Colour palette selector ────────────────────────────────────────────
  updateSelectInput(session, "umapColourPalette",
    choices = reactiveValuesToList(colourPaletteList) |> names())

  # ── Feature plot inputs ────────────────────────────────────────────────
  updateSelectInput(session, "fpColumnToPlot",
    choices = c("None", colsThatCanBePlot), selected = "cluster_id")
  updateSelectInput(session, "fpColumnToSplit",
    choices = c("None", colsThatCanBePlot), selected = "None")
  updateSelectInput(session, "fpAssayToPlot",
    choices = c(
      "Quantile Normalised" = "exprsQuantNorm",
      "Arcsinh Transformed" = "exprsTransformed",
      "Z-Scaled"            = "norm",
      "Raw Counts"          = "counts"
    ),
    selected = "exprsQuantNorm")

  # ── Server-side selectize for markers ──────────────────────────────────
  marker_choices <- sorted_markers_cache %||% rownames(sce)
  updateSelectizeInput(session, "fpFeatureToPlot",
    choices = marker_choices, selected = NULL, server = TRUE)

  # ── Large dataset optimisations ────────────────────────────────────────
  if (ncell > 150000) {
    updateSliderInput(session, "pointSizeUMAP",  value = 0.5)
    updateSliderInput(session, "pointAlphaUMAP", value = 0.6)
  }

}) # end once-only data observer


# Note: colour pickers are not in the v2 UI -- colour management is handled
# through the palette selector (umapColourPalette) and the relabel table
# colours column.  The plotByBucket uiOutput is created by server-plots.R
# for the sortable bucket list.


# ── Metadata table ─────────────────────────────────────────────────────────
output$metadataTable <- DT::renderDataTable({
  req(inputDataReactive$Results)
  res <- inputDataReactive$Results
  cd  <- as.data.frame(SummarizedExperiment::colData(res$sce))
  DT::datatable(
    cd,
    filter   = "top",
    rownames = FALSE,
    options  = list(
      pageLength = 20,
      scrollX    = TRUE
    )
  )
})


# ── Marker (top-marker) table ─────────────────────────────────────────────
output$posMarkerUI <- renderUI({
  req(inputDataReactive$Results)
  res <- inputDataReactive$Results
  if ("topMarkerTable" %in% names(res)) {
    DT::dataTableOutput(outputId = "posMarkerTable")
  } else {
    tags$p("No marker table was found in this dataset.")
  }
})

output$posMarkerUI2 <- renderUI({
  req(inputDataReactive$Results)
  res <- inputDataReactive$Results
  if ("topMarkerTable" %in% names(res)) {
    tagList(
      actionButton(
        inputId = "addMarkersFromTable",
        label   = "Add selected markers to plot list",
        icon    = icon("plus"),
        class   = "btn-primary btn-sm",
        style   = "margin-bottom:6px;"
      ),
      actionButton(
        inputId = "resetPosMarkerTableSelectRows",
        label   = "Deselect all",
        class   = "btn-sm",
        style   = "margin-bottom:6px; margin-left:4px;"
      )
    )
  }
})

observe({
  req(inputDataReactive$Results)
  res <- inputDataReactive$Results
  if (!"topMarkerTable" %in% names(res)) return()

  tmt <- res$topMarkerTable
  # Format numeric columns to 3 significant figures
  num_cols <- vapply(tmt, is.numeric, logical(1))
  dt <- DT::datatable(
    tmt,
    filter    = "top",
    rownames  = FALSE,
    selection = list(mode = "multiple", target = "row"),
    extensions = "Buttons",
    options   = list(
      dom        = "Bfrtip",
      buttons    = list("csv", "excel", "copy"),
      pageLength = 15,
      scrollX    = TRUE
    )
  )
  if (any(num_cols)) {
    dt <- DT::formatSignif(dt, columns = names(tmt)[num_cols], digits = 3)
  }
  output$posMarkerTable <- DT::renderDataTable(dt, server = TRUE)

  proxy <- DT::dataTableProxy("posMarkerTable")

  # Deselect all rows
  observeEvent(input$resetPosMarkerTableSelectRows, {
    proxy |> DT::selectRows(NULL)
  })

  # Click-to-add: append selected markers to fpFeatureToPlot
  observeEvent(input$addMarkersFromTable, {
    sel_rows <- input$posMarkerTable_rows_selected
    if (is.null(sel_rows) || length(sel_rows) == 0) return(invisible(NULL))

    marker_col <- intersect(colnames(tmt),
      c("Marker", "marker", "Feature", "feature", "gene", "Gene"))
    if (length(marker_col) == 0) {
      for (cn in colnames(tmt)) {
        if (any(as.character(tmt[[cn]][sel_rows]) %in% rownames(res$sce))) {
          marker_col <- cn
          break
        }
      }
    } else {
      marker_col <- marker_col[1]
    }
    if (length(marker_col) == 0) return(invisible(NULL))

    new_markers <- as.character(tmt[[marker_col]][sel_rows])
    new_markers <- intersect(new_markers, rownames(res$sce))
    if (length(new_markers) == 0) return(invisible(NULL))

    current  <- isolate(input$fpFeatureToPlot) %||% character(0)
    combined <- unique(c(current, new_markers))
    marker_choices <- res$sorted_markers_cache %||% rownames(res$sce)
    updateSelectizeInput(session, "fpFeatureToPlot",
      choices = marker_choices, selected = combined, server = TRUE)
  })
})


# ── Gene selection (debounced) ─────────────────────────────────────────────
# Combine dropdown, text area, and marker table selections into a single
# reactive vector of valid marker names.
genes_raw <- reactive({
  dropdown <- input$fpFeatureToPlot %||% character(0)

  # Parse text input: split on whitespace, commas, newlines, tabs
  text_val <- input$fpFeatureToPlotText %||% ""
  text_markers <- if (nzchar(trimws(text_val))) {
    trimws(unlist(strsplit(text_val, "[\\s,;\t\n]+")))
  } else {
    character(0)
  }
  text_markers <- text_markers[nzchar(text_markers)]

  combined <- unique(c(dropdown, text_markers))

  # Validate against known marker names
  req(inputDataReactive$Results)
  valid <- rownames(inputDataReactive$Results$sce)
  intersect(combined, valid)
})

genes_debounced <- genes_raw |> debounce(millis = 200)

observe({
  genesReactive$genes <- genes_debounced()
})
