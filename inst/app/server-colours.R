# server-colours.R
# Colour palette management, colour pickers, and colour update observers

# Fireworks ----
fw <- Fireworks$new()
observeEvent(input$acceptCite, {
  fw$start()
  Sys.sleep(3)
  fw$stop(fadeOut = TRUE)
})
observeEvent(input$acceptCite, {
  output$showPDFs <- renderUI({
    column(
      width = 12,
      h4("Download Figures"),
      splitLayout(
        selectInput(
          inputId = "dlFormat", label = "Download Format",
          choices = c("PDF", "SVG", "PNG"),
          selected = "PDF", width = "85%"
        ),
        sliderInput(
          inputId = "pngRes", label = "PNG Resolution",
          min = 100, max = 1000, value = 600,
          step = 100, width = "85%", ticks = FALSE
        )
      ),
      downloadButton(outputId = "dlUMAP", label = "Download DR Plot"),
      downloadButton(outputId = "dlFP", label = "Download Feature Plot"),
      hr(style = "border-top: 1px solid #000000;"), h4("Download App Settings"),
      helpText("Download all the app settings as an Excel sheet."),
      downloadButton("downloadInputsE", "Download settings (Excel)"),
      hr(style = "border-top: 1px solid #000000;"), h4("Download FCS Files"),
      helpText(
        "Use this button to download modified FCS files.
        These contain the original intensity values, in addition to the DR coordinates,
        as well as the cluster IDs (and annotation labels) coded as numerical values.
        An Excel file mapping the numerical IDs to their original values is also included."
      ),
      downloadButton("downloadFCS", "Download FCS files"),
      downloadButton("downloadClusterCodes", "Download cluster codes"),
      hr(style = "border-top: 1px solid #000000;"), h4("Main Citations"),
      tags$p("Please also include citations for the main parts of this pipeline:"),
      helpText("CATALYST; flowCore; FlowSOM; Phenograph; diffcyt; ComplexHeatmap; edgeR; FlowAI, PARC; PacMAP")
    )
  })
  showNotification(
    ui = "Thanks for agreeing to cite us! You made the marmots very happy!",
    duration = 20
  )
})

# Cache loaded data at startup (isolate: one-time read, no reactive dependency)
res <- isolate(inputDataReactive$Results)

# Colour palette list ----
colourPaletteList <- init_colour_palette_list(res$sce)

# Colour pickers for condition groups ----
colsList1 <- res[["coloursList"]][res[["conditions"]]]
colsList1 <- colsList1[!sapply(colsList1, is.null)]
output$uiColourPicker <- renderUI({
  lapply(names(colsList1), function(col) {
    lapply(names(colsList1[[col]]), function(lor) {
      colourpicker::colourInput(
        inputId = paste0("GroupColour", col, lor),
        label = paste0(col, ": ", lor),
        value = inputDataReactive$Results$coloursList[[col]][[lor]],
        palette = "square",
        closeOnClick = TRUE,
        returnName = TRUE
      )
    })
  })
})

# Observer: update coloursList when user changes a colour picker
observeEvent({
  lapply(names(colsList1), function(col) {
    lapply(names(colsList1[[col]]), function(lor) {
      input[[paste0("GroupColour", col, lor)]]
    })
  })
}, {
  lapply(names(colsList1), function(col) {
    lapply(names(colsList1[[col]]), function(lor) {
      req(!is.null(input[[paste0("GroupColour", col, lor)]]))
      inputDataReactive$Results$coloursList[[col]][[lor]] <- input[[paste0("GroupColour", col, lor)]]
    })
  })
})

# Update inputs ----
allCols <- colnames(SummarizedExperiment::colData(res$sce))
colsThatCanBePlot <- get_plottable_columns(res$sce)

# Update the DR types that can be plotted
# Default DR priority: PaCMAP > UMAP > TSNE > first available
dr_names <- SingleCellExperiment::reducedDimNames(res$sce)
dr_default <- dr_names[1]
for (pref in c("TSNE", "UMAP", "PaCMAP")) {
  hit <- grep(pref, dr_names, ignore.case = TRUE, value = TRUE)
  if (length(hit) > 0) dr_default <- hit[1]
}
updateSelectInput(session = session, inputId = "umapDRToPlot",
  choices = dr_names, selected = dr_default)
# Update the colData columns available to plot by (categorical)
updateSelectInput(session = session, inputId = "umapColumnToPlot", choices = colsThatCanBePlot, selected = "cluster_id")
# Update the available categorical metadata columns to split by
updateSelectInput(
  session = session, inputId = "umapColumnToSplit",
  choices = c("None", colsThatCanBePlot), selected = "None"
)
# Update the available contrasts
contrasts_available <- res$smd$`Conditions To Test`
contrasts_available <- contrasts_available[!is.na(contrasts_available)]
updateSelectInput(session, "umapContrastToUse", choices = contrasts_available)
updateSelectInput(session, "fpContrastToUse", choices = contrasts_available)
# Update a bunch of feature-plot-associated input options
updateSelectInput(session, "fpColumnToPlot",
  choices = c("None", colsThatCanBePlot), selected = "cluster_id")
updateSelectInput(session, "fpColumnToSplit",
  choices = c("None", colsThatCanBePlot), selected = "None")
# Server-side selectize for markers: backed by sorted_markers_cache for fast lookup
marker_choices <- res$sorted_markers_cache %||% rownames(res$sce)
updateSelectizeInput(session, "fpFeatureToPlot",
  choices = marker_choices, selected = NULL, server = TRUE)

# Metadata table ----
output$metadataTable <- DT::renderDataTable(
  res$md |> dplyr::select(-file_name)
)
labelList <- setNames(lapply(res$conditions, function(x) {
  levels(as.factor(res$md[[x]]))
}), res$conditions)
labelDf <- data.frame(
  "Factor" = unlist(lapply(seq_along(labelList), function(i) {
    rep(names(labelList)[[i]], lengths(labelList)[[i]])
  })),
  "Levels" = as.character((unlist(labelList)))
)
labelReactive <- reactiveValues(labelList = labelList, labelDf = labelDf)
output$changeLabelTable <- DT::renderDataTable(
  DT::datatable(
    labelDf, class = "display",
    selection = "none", editable = TRUE, rownames = FALSE
  )
)

# posMarkers table ----
output$posMarkerUI <- renderUI({
  if ("topMarkerTable" %in% names(inputDataReactive$Results)) {
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
      ),
      DT::dataTableOutput(outputId = "posMarkerTable")
    )
  } else {
    p("No marker table was found in this dataset.")
  }
})
if ("topMarkerTable" %in% names(res)) {
  dt1 <- DT::datatable(
    data = res[["topMarkerTable"]],
    filter = "top",
    rownames = FALSE,
    selection = list(mode = "multiple", target = "row")
  )
  output$posMarkerTable <- DT::renderDataTable(
    dt1, server = TRUE
  )
  proxy <- dataTableProxy("posMarkerTable")

  # Deselect all button
  observeEvent(input$resetPosMarkerTableSelectRows, {
    proxy |> selectRows(NULL)
  })

  # Click-to-add: append selected marker names to fpFeatureToPlot selectize
  observeEvent(input$addMarkersFromTable, {
    sel_rows <- input$posMarkerTable_rows_selected
    if (is.null(sel_rows) || length(sel_rows) == 0) return(invisible(NULL))

    tmt <- res[["topMarkerTable"]]
    marker_col <- intersect(colnames(tmt), c("Marker", "marker", "Feature", "feature", "gene", "Gene"))
    if (length(marker_col) == 0) {
      # Fallback: use first column that has values matching rownames(sce)
      for (cn in colnames(tmt)) {
        candidates <- as.character(tmt[[cn]][sel_rows])
        if (any(candidates %in% rownames(res$sce))) {
          marker_col <- cn
          break
        }
      }
    } else {
      marker_col <- marker_col[1]
    }
    if (length(marker_col) == 0) return(invisible(NULL))

    new_markers <- as.character(tmt[[marker_col]][sel_rows])
    # Intersect with known rownames to avoid adding garbage
    new_markers <- intersect(new_markers, rownames(res$sce))
    if (length(new_markers) == 0) return(invisible(NULL))

    current <- isolate(input$fpFeatureToPlot) %||% character(0)
    combined <- unique(c(current, new_markers))
    updateSelectizeInput(session, "fpFeatureToPlot",
      choices = marker_choices, selected = combined, server = TRUE)
  })
}
