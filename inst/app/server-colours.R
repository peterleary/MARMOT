# server-colours.R
# Colour palette management, colour pickers, and colour update observers

# Fireworks ----
fw <- Fireworks$new()
observe({
  fw$start()
  Sys.sleep(3)
  fw$stop(fadeOut = TRUE)
}) |> bindEvent(input$acceptCite)
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
      hr(style = "border-top: 1px solid #000000;"), h4("Download App Data"),
      helpText("App data will download as a qs file, which can be imported into R with `qs::qread()`"),
      downloadButton("downloadData", "Download app data"),
      hr(style = "border-top: 1px solid #000000;"), h4("Download App Settings"),
      helpText("Download all the app settings as either an Excel sheet, or as a qs file of the settings as a list."),
      downloadButton("downloadInputsE", "Download settings (Excel)"),
      downloadButton("downloadInputsR", "Download settings (qs)"),
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

# Colour palette list ----
colourPaletteList <- init_colour_palette_list(inputDataReactive$Results$sce)

# Colour pickers for condition groups ----
colsList1 <- inputDataReactive[["Results"]][["coloursList"]][inputDataReactive[["Results"]][["conditions"]]]
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
allCols <- colnames(SummarizedExperiment::colData(inputDataReactive$Results$sce))
colsThatCanBePlot <- get_plottable_columns(inputDataReactive$Results$sce)

# Update the DR types that can be plotted
updateSelectInput(session = session, inputId = "umapDRToPlot",
  choices = SingleCellExperiment::reducedDimNames(inputDataReactive$Results$sce),
  selected = SingleCellExperiment::reducedDimNames(inputDataReactive$Results$sce)[[2]])
# Update the colData columns available to plot by (categorical)
updateSelectInput(session = session, inputId = "umapColumnToPlot", choices = colsThatCanBePlot, selected = "cluster_id")
# Update the available categorical metadata columns to split by
updateSelectInput(
  session = session, inputId = "umapColumnToSplit",
  choices = c("None", colsThatCanBePlot), selected = "None"
)
# Update the available contrasts
contrasts_available <- inputDataReactive$Results$smd$`Conditions To Test`
contrasts_available <- contrasts_available[!is.na(contrasts_available)]
updateSelectInput(session, "umapContrastToUse", choices = contrasts_available)
updateSelectInput(session, "fpContrastToUse", choices = contrasts_available)
# Update a bunch of feature-plot-associated input options
updateSelectInput(session, "fpColumnToPlot",
  choices = c("None", colsThatCanBePlot), selected = "cluster_id")
updateSelectInput(session, "fpColumnToSplit",
  choices = c("None", colsThatCanBePlot), selected = "None")
updateSelectInput(session, "fpFeatureToPlot",
  choices = names(inputDataReactive$Results$sce), selected = NULL)

# Metadata table ----
output$metadataTable <- DT::renderDataTable(
  inputDataReactive$Results$md |> dplyr::select(-file_name)
)
labelList <- setNames(lapply(inputDataReactive$Results$conditions, function(x) {
  levels(as.factor(inputDataReactive$Results$md[[x]]))
}), inputDataReactive$Results$conditions)
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
    DT::dataTableOutput(outputId = "posMarkerTable")
  } else {
    renderText("No Marker Gene table was loaded.")
  }
})
if ("topMarkerTable" %in% names(inputDataReactive$Results)) {
  dt1 <- DT::datatable(
    data = inputDataReactive$Results[["topMarkerTable"]],
    filter = "top",
    rownames = FALSE
  )
  output$posMarkerTable <- DT::renderDataTable(
    dt1, server = TRUE, selection = list(target = "row")
  )
  proxy <- dataTableProxy("posMarkerTable")
  observeEvent(input$resetPosMarkerTableSelectRows, {
    proxy |> selectRows(NULL)
  })
}
