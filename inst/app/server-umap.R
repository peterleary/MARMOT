# Fireworks ----
fw <- Fireworks$new()
observe({
  fw$start()
  Sys.sleep(3)
  fw$stop(fadeOut = TRUE)
}) |> bindEvent(input$acceptCite)
observeEvent(input$acceptCite, {
  # fireworks(id = "myFireworks", options = list(fadeOut = TRUE))
  output$showPDFs <- renderUI({
    column(
      width = 12,
      h4("Download Figures"),
      splitLayout(
        selectInput(inputId = "dlFormat", label = "Download Format", choices = c("PDF", "SVG", "PNG"), selected = "PDF", width = "85%"),
        sliderInput(inputId = "pngRes", label = "PNG Resolution", min = 100, max = 1000, value = 600, step = 100, width = "85%", ticks = F)
      ),
      downloadButton(outputId = "dlUMAP", label = "Download DR Plot"),
      downloadButton(outputId = "dlFP", label = "Download Feature Plot"),
      hr(style = "border-top: 1px solid #000000;"), h4("Download App Data"),
      helpText("App data will download as a qs file, which can be imported into R with `qs::qread()`"),
      downloadButton("downloadData", "Download app data"),
      hr(style = "border-top: 1px solid #000000;"), h4("Download App Settings"),
      helpText("Download all the app settings as either an Excel sheet, or as a qs file of the settings as a list."),
      downloadButton("downloadInputsE", "Download settings (Excel)"), downloadButton("downloadInputsR", "Download settings (qs)"),
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
  showNotification(ui = "Thanks for agreeing to cite us! You made the marmots very happy!", duration = 20)
})

# Get some nice colours for things ----
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
output$uiColourPicker <- renderUI({
  lapply(names(colsList1), function(col) {
    lapply(names(colsList1[[col]]), function(lor) {
      colourpicker::colourInput(
        inputId = paste0("GroupColour", col, lor),
        label = paste0(col, ": ", lor),
        value =  inputDataReactive$Results$coloursList[[col]][[lor]],
        palette = "square",
        closeOnClick = TRUE,
        returnName = TRUE
      )
    })
  })
})
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
# Get the metadata columns that can be used for plotting (i.e., discrete variables)
allCols <- colnames(colData(inputDataReactive$Results$sce))
colsThatCanBePlot <- unlist(lapply(seq_along(allCols), function(i) {
  if (length(unique(inputDataReactive$Results$sce[[allCols[i]]])) < 100) {
    allCols[i]
  }
}))

# Update the DR types that can be plot
updateSelectInput(session = session, inputId = "umapDRToPlot", choices = reducedDimNames(inputDataReactive$Results$sce), selected = reducedDimNames(inputDataReactive$Results$sce)[[2]])
# Update the colData columns available to plot by (categorical)
updateSelectInput(session = session, inputId = "umapColumnToPlot", choices = colsThatCanBePlot, selected = "cluster_id")
# Update the available categorical metadata columns to split by
updateSelectInput(session = session, inputId = "umapColumnToSplit", choices = c("None", colsThatCanBePlot), selected = "None")
# Update the available contrasts 
updateSelectInput(session = session, inputId = "umapContrastToUse", choices = inputDataReactive$Results$smd$`Conditions To Test` %>% .[!is.na(.)])
updateSelectInput(session = session, inputId = "fpContrastToUse", choices = inputDataReactive$Results$smd$`Conditions To Test` %>% .[!is.na(.)])
# Update a bunch of feature-plot-associated input options
updateSelectInput(session = session, inputId = "fpColumnToPlot", choices = c("None", colsThatCanBePlot), selected = "cluster_id")
updateSelectInput(session = session, inputId = "fpColumnToSplit", choices = c("None", colsThatCanBePlot), selected = "None")
updateSelectInput(session = session, inputId = "fpFeatureToPlot", choices = names(inputDataReactive$Results$sce), selected = NULL)

# Metadata table ----
# Output the metadata table
output$metadataTable <- DT::renderDataTable((inputDataReactive$Results$md %>% dplyr::select(-file_name)))
labelList <- setNames(lapply(inputDataReactive$Results$conditions, function(x) {
  levels(as.factor(inputDataReactive$Results$md[[x]]))
}), inputDataReactive$Results$conditions)
labelDf <- data.frame(
  "Factor" = unlist(lapply(seq_along(labelList), function(i) {rep(names(labelList)[[i]], lengths(labelList)[[i]])})),
  "Levels" = as.character((unlist(labelList)))
)
labelReactive <- reactiveValues(labelList = labelList, labelDf = labelDf)
# Make a table to be able to edit factor levels for visualisation purposes
output$changeLabelTable <- DT::renderDataTable(DT::datatable(labelDf, class = "display", selection = 'none', editable = TRUE, rownames = F))
# If the user edits one of the labels, change the data 
observeEvent({
  input$clusterLabelTable_cell_edit
}, ignoreNULL = FALSE, ignoreInit = TRUE, {
  clusterTableReactive$table <<- editData(clusterTableReactive$table, input$clusterLabelTable_cell_edit)
  
})


# posMarkers table ----
# If the posMarkers xlsx file was loaded, create the UI to display it
output$posMarkerUI <- renderUI({
  if ("topMarkerTable" %in% names(inputDataReactive$Results)) {
    # req(!is.null(inputDataReactive$Results[["topMarkerTable"]]))
    DT::dataTableOutput(outputId = "posMarkerTable")
  } else {
    renderText("No Marker Gene table was loaded.")
  }
})
# If the posMarkers xlsx file was loaded, table the table in the table UI
if ("topMarkerTable" %in% names(inputDataReactive$Results)) {
  # req(!is.null(inputDataReactive$Results[["topMarkerTable"]]))
  dt1 <- DT::datatable(
    data = inputDataReactive$Results[["topMarkerTable"]], 
    filter = "top", 
    rownames = FALSE
    )
  output$posMarkerTable <- DT::renderDataTable(dt1, server = TRUE, selection = list(target = 'row'))
  proxy = dataTableProxy("posMarkerTable")
  observeEvent(input$resetPosMarkerTableSelectRows, {
    proxy %>% selectRows(NULL)
  })
}

# Clusters table ----
# Create the ability to relabel clusters
clusterTableReactive <- reactiveValues(table = NULL)
clusterTableReactive$table <- data.frame(
  "cluster_id" = levels(inputDataReactive$Results[["sce"]]@colData$cluster_id),
  "relabelled_clusters" = levels(inputDataReactive$Results[["sce"]]@colData$cluster_id),
  "colours" = inputDataReactive$Results$coloursList$cluster_id[match(levels(inputDataReactive$Results[["sce"]]@colData$cluster_id), names(inputDataReactive$Results$coloursList$cluster_id))]
)
rownames(clusterTableReactive$table) <- NULL
clusterTableReactive$table <- column_to_rownames(clusterTableReactive$table, "cluster_id")
# Download button for clusterLabels
output$saveClusterLabels <- downloadHandler(
  filename = function() { "clusterInfos.xlsx" },
  content = function(file) {
    openxlsx::write.xlsx(data.frame(clusterTableReactive$table) %>% rownames_to_column("original"), file = file)
  }
)
# show the cluster table
output$clusterLabelTable <- DT::renderDataTable({
  DT::datatable(
    data = clusterTableReactive$table,
    class = "display",
    selection = 'none',
    editable = TRUE,
    options = list(
      dom = "ft",
      pageLength = 10000
    )
  ) %>%
    DT::formatStyle(
      columns = "colours",
      backgroundColor = DT::styleEqual(
        clusterTableReactive$table$colours,
        clusterTableReactive$table$colours
      )
    )
})

inputDataReactive$Results$coloursList[["relabelled_clusters"]] <- inputDataReactive$Results$coloursList$cluster_id

# If user edits cluster label table, add new column to cell meta data
observeEvent({
  input$clusterLabelTable_cell_edit
}, ignoreNULL = FALSE, ignoreInit = TRUE, {
  clusterTableReactive$table <<- editData(clusterTableReactive$table, input$clusterLabelTable_cell_edit)
  
  relabelledClusterColour <- clusterTableReactive$table$colours
  names(relabelledClusterColour) <- clusterTableReactive$table$relabelled_clusters
  relabelledClusterColour <- relabelledClusterColour[unique(names(relabelledClusterColour))]
  inputDataReactive$Results$coloursList[["relabelled_clusters"]] <- relabelledClusterColour
  
  # Update the main sce and scData objects
  relabelled_clusters <- clusterTableReactive$table$relabelled_clusters[match(inputDataReactive$Results[["scData"]]@meta.data$cluster_id, rownames(clusterTableReactive$table))]
  relabelled_clusters <- factor(relabelled_clusters, levels = unique(gtools::mixedsort(relabelled_clusters)))
  inputDataReactive$Results[["sce"]]@colData$relabelled_clusters <- relabelled_clusters
  inputDataReactive$Results[["scData"]]@meta.data$relabelled_clusters <- relabelled_clusters
  inputDataReactive$Results[["scDataToFP"]]@meta.data$relabelled_clusters <- relabelled_clusters
  inputDataReactive$clusterInfos$ClusterLabel <- clusterTableReactive$table$relabelled_clusters[match(inputDataReactive$clusterInfos$Cluster, rownames(clusterTableReactive$table))]
  # Update the DR data frames 
  for (tab in names(inputDataReactive$Results$umapDFList)) {
    if (tab %in% names(inputDataReactive$Results$umapDFList)) {
      inputDataReactive$Results$umapDFList[[tab]]$relabelled_clusters <- clusterTableReactive$table$relabelled_clusters[match(inputDataReactive$Results$umapDFList[[tab]]$cluster_id, rownames(clusterTableReactive$table))]
      inputDataReactive$Results$umapDFList[[tab]]$relabelled_clusters <- factor(
        x = inputDataReactive$Results$umapDFList[[tab]]$relabelled_clusters, 
        levels = mixedsort(unique(inputDataReactive$Results$umapDFList[[tab]]$relabelled_clusters))
      )
    }
  }
  
  allCols <- colnames(inputDataReactive$Results[["scData"]]@meta.data[, sapply(inputDataReactive$Results[["scData"]]@meta.data, class) %in% c("character", "factor")])
  colsThatCanBePlot <- lapply(seq_along(allCols), function(i) {
    if (length(unique(inputDataReactive$Results[["scData"]]@meta.data[[allCols[i]]])) < 100) {
      allCols[i]
    }
  }) %>% unlist()
  updateSelectInput(session = session, inputId = "umapColumnToPlot", choices = colsThatCanBePlot, selected = "relabelled_clusters")
  updateSelectInput(session = session, inputId = "fpColumnToPlot", choices = c("None", colsThatCanBePlot), selected = "relabelled_clusters")
  updateSelectInput(session = session, inputId = "umapColumnToSplit", choices = c("None", colsThatCanBePlot), selected = input$umapColumnToSplit)
  updateSelectInput(session = session, inputId = "fpColumnToSplit", choices = c("None", colsThatCanBePlot), selected = input$fpColumnToSplit)
})

observeEvent(input$importFile, {
  importedDf <- openxlsx::read.xlsx(input$importFile[1, 'datapath'], colNames = T)
  importedDf <- importedDf %>% data.frame(check.names = F) %>% column_to_rownames("original")
  if(any(!inputDataReactive$Results[["scData"]]@meta.data$cluster_id %in% rownames(importedDf))) {
    shinyalert::shinyalert(title = "The marmots say no 🦫🚫", text = "You uploaded a file that has different original cluster IDs or different numbers of original clusters. Are you sure it's from this study?", closeOnEsc = TRUE, closeOnClickOutside = TRUE, showCancelButton = TRUE, imageUrl = "./Resetti_CF.webp.png")
  } else {
    importedDf$relabelled_clusters <- factor(importedDf$relabelled_clusters, levels = unique(gtools::mixedsort(as.character(importedDf$relabelled_clusters))))
    clusterTableReactive$table <- importedDf
    
    relabelledClusterColour <- clusterTableReactive$table$colours
    names(relabelledClusterColour) <- clusterTableReactive$table$relabelled_clusters
    relabelledClusterColour <- relabelledClusterColour[unique(names(relabelledClusterColour))]
    inputDataReactive$Results$coloursList[["relabelled_clusters"]] <- relabelledClusterColour
    
    # Update the main sce and scData objects
    relabelled_clusters <- clusterTableReactive$table$relabelled_clusters[match(inputDataReactive$Results[["scData"]]@meta.data$cluster_id, rownames(clusterTableReactive$table))]
    relabelled_clusters <- factor(relabelled_clusters, levels = unique(gtools::mixedsort(relabelled_clusters)))
    inputDataReactive$Results[["sce"]]@colData$relabelled_clusters <- relabelled_clusters
    inputDataReactive$Results[["scData"]]@meta.data$relabelled_clusters <- relabelled_clusters
    inputDataReactive$Results[["scDataToFP"]]@meta.data$relabelled_clusters <- relabelled_clusters
    inputDataReactive$clusterInfos$ClusterLabel <- clusterTableReactive$table$relabelled_clusters[match(inputDataReactive$clusterInfos$Cluster, rownames(clusterTableReactive$table))]
    # Update the DR data frames 
    for (tab in names(inputDataReactive$Results$umapDFList)) {
      if (tab %in% names(inputDataReactive$Results$umapDFList)) {
        inputDataReactive$Results$umapDFList[[tab]]$relabelled_clusters <- clusterTableReactive$table$relabelled_clusters[match(inputDataReactive$Results$umapDFList[[tab]]$cluster_id, rownames(clusterTableReactive$table))]
        inputDataReactive$Results$umapDFList[[tab]]$relabelled_clusters <- factor(
          x = inputDataReactive$Results$umapDFList[[tab]]$relabelled_clusters, 
          levels = mixedsort(unique(inputDataReactive$Results$umapDFList[[tab]]$relabelled_clusters))
        )
      }
    }
    
    allCols <- colnames(inputDataReactive$Results[["scData"]]@meta.data[, sapply(inputDataReactive$Results[["scData"]]@meta.data, class) %in% c("character", "factor")])
    colsThatCanBePlot <- lapply(seq_along(allCols), function(i) {
      if (length(unique(inputDataReactive$Results[["scData"]]@meta.data[[allCols[i]]])) < 100) {
        allCols[i]
      }
    }) %>% unlist()
    colourPaletteList$relabelled_clusters <- clusterTableReactive$table$colours
    updateSelectInput(session = session, inputId = "umapColumnToPlot", choices = colsThatCanBePlot, selected = "relabelled_clusters")
    updateSelectInput(session = session, inputId = "fpColumnToPlot", choices = c("None", colsThatCanBePlot), selected = "relabelled_clusters")
    updateSelectInput(session = session, inputId = "umapColumnToSplit", choices = c("None", colsThatCanBePlot), selected = input$umapColumnToSplit)
    updateSelectInput(session = session, inputId = "fpColumnToSplit", choices = c("None", colsThatCanBePlot), selected = input$fpColumnToSplit)
  }
})

# Subsetting UI and guff ----
observeEvent({
  input$fpSubsetCells
}, ignoreNULL = FALSE, ignoreInit = TRUE, {
  req(inputDataReactive$Results[["scDataToFP"]])
  if (input$fpSubsetCells) {
    # Show UI elements when checked
    output$fpSubsetCellsByColumnUI1 <- renderUI({
      selectInput(
        inputId = "fpColumnToSubset",
        label = "Subset cells proportionally by",
        choices = colnames(inputDataReactive$Results[["scDataToFP"]]@meta.data),
        selected = "condition"
      )
    })

    output$fpSubsetCellsByColumnUI2 <- renderUI({
      numericInput(
        inputId = "fpSubsetToGlobal",
        label = "Subset proportionally to",
        value = isolate({
          if (!is.null(input$fpSubsetToGlobal)) input$fpSubsetToGlobal else ncol(inputDataReactive$Results[["scDataToFP"]])
        }),
        min = 1,
        max = ncol(inputDataReactive$Results[["scDataToFP"]]),
        step = 1
      )
    })
  } else {
    # Reset everything when unchecked
    output$fpSubsetCellsByColumnUI1 <- NULL
    output$fpSubsetCellsByColumnUI2 <- NULL
    output$fpSubsetCellsTableUI <- NULL

    # Reset reactive values
    cellsToKeepReactive$sc2 <- NULL

    # Reset data to original
    inputDataReactive$Results[["scDataToFP"]] <- inputDataReactive$Results[["scDataToFP"]]
  }
})

# Reactive values for storing subset information
cellsToKeepReactive <- reactiveValues(sc2 = NULL)

# Observer for column and global subset changes
observeEvent({
  input$fpColumnToSubset
  input$fpSubsetToGlobal
}, ignoreNULL = FALSE, ignoreInit = TRUE, {
  req(nrow(inputDataReactive$Results[["scDataToFP"]]) > 10)
  if (isTRUE(input$fpSubsetCells)) {
    req(input$fpColumnToSubset, input$fpSubsetToGlobal)

    # Calculate proportions
    cell_counts <- table(inputDataReactive$Results[["scDataToFP"]][[input$fpColumnToSubset]])
    proportions <- as.numeric(cell_counts) / sum(cell_counts)
    names(proportions) <- names(cell_counts)

    # Calculate subset counts
    sc2 <- floor(input$fpSubsetToGlobal * proportions)
    # Ensure at least 1 cell per group
    sc2[sc2 == 0] <- 1

    # Create UI for individual group controls
    output$fpSubsetCellsTableUI <- renderUI({
      group_levels <- names(cell_counts)
      lapply(group_levels, function(x) {
        numericInput(
          inputId = paste0("fpSubset", x, "ToThis"),
          label = paste("Cells for", x),
          value = sc2[[x]],
          min = 1,
          max = as.numeric(cell_counts[[x]]),
          step = 1
        )
      })
    })

    # Store calculated values
    cellsToKeepReactive$sc2 <- sc2
  } else {
    output$fpSubsetCellsTableUI <- NULL
    cellsToKeepReactive$sc2 <- NULL
  }
})

# Observer for individual group input changes
observeEvent({
  if (isTRUE(input$fpSubsetCells) && !is.null(input$fpColumnToSubset)) {
    group_levels <- names(table(inputDataReactive$Results[["scDataToFP"]][[input$fpColumnToSubset]]))
    lapply(group_levels, function(x) {
      input[[paste0("fpSubset", x, "ToThis")]]
    })
  }
}, ignoreNULL = TRUE, ignoreInit = TRUE, {
  req(nrow(inputDataReactive$Results[["scDataToFP"]]) > 10)
  if (isTRUE(input$fpSubsetCells) && !is.null(input$fpColumnToSubset)) {
    group_levels <- names(table(inputDataReactive$Results[["scDataToFP"]][[input$fpColumnToSubset]]))

    # Update reactive values with user inputs
    for (x in group_levels) {
      input_id <- paste0("fpSubset", x, "ToThis")
      if (!is.null(input[[input_id]]) && !is.null(cellsToKeepReactive$sc2)) {
        cellsToKeepReactive$sc2[[x]] <- as.numeric(input[[input_id]])
      }
    }
  }
})

# Observer for final subsetting
observeEvent(cellsToKeepReactive$sc2, ignoreNULL = TRUE, ignoreInit = TRUE, {
  req(nrow(inputDataReactive$Results[["scDataToFP"]]) > 10)
  req(input$fpColumnToSubset, cellsToKeepReactive$sc2)

  # Sample cells from each group
  cellsToKeep <- unlist(lapply(names(cellsToKeepReactive$sc2), function(x) {
    group_cells <- rownames(inputDataReactive$Results[["scDataToFP"]]@meta.data)[
      inputDataReactive$Results[["scDataToFP"]]@meta.data[[input$fpColumnToSubset]] == x
    ]
    n_cells <- min(cellsToKeepReactive$sc2[[x]], length(group_cells))
    sample(group_cells, n_cells)
  }))

  # Create subset data
  inputDataReactive$Results[["scDataToFPSubset"]] <- subset(inputDataReactive$Results[["scDataToFP"]], cells = cellsToKeep)
})

# Download inputs etc. ---- 
# Download all currently selected inputs 
output$downloadInputsE <- downloadHandler(
  filename = function() {
    "Input_Options.xlsx"
  },
  content = function(file) { 
    x <- reactiveValuesToList(input)
    x <- data.frame(unlist(x))
    x <- rownames_to_column(x, "Input")
    colnames(x)[[2]] <- "Value"
    writexl::write_xlsx(x, path = file)
  }
)
output$downloadInputsR <- downloadHandler(
  filename = function() {
    "Input_Options.qs"
  },
  content = function(file) {
    qs::qsave(x = reactiveValuesToList(input), file = file, nthreads = 8)
  }
)
# Download current reactive data
output$downloadData <- downloadHandler(
  filename = function() {
    paste0("MARMOT_Data_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".qs")
  },
  content = function(file) { 
    showNotification(ui = "Preparing data for download. Will download automatically when ready. Please do not click download multiple times!", duration = 30)
    qs::qsave(x = inputDataReactive$Results, file = file, nthreads = 8)
  }
)


# DR Plot ---- 
umapReactive <- eventReactive(
  {
    input$umapDRToPlot
    input$umapColumnToPlot
    input$textSizeUMAP
    input$pointSizeUMAP
    input$umapShowLabels
    input$umapShowAxes
    input$umapLegendPosition
    input$umapColumnToSplit
    input$pointBorderUMAP
    input$borderSizeUMAP
    input$umapMainNcol
    input$pointAlphaUMAP
    input$umapBorderColour
    input$labelSizeUMAP
    input$labelShiftUMAP
    input$umapShowDAClusters
    input$umapContrastToUse
    clusterTableReactive$table
    lapply(names(colsList1), function(col) {
      lapply(names(colsList1[[col]]), function(lor) {
        input[[paste0("GroupColour", col, lor)]]
      })
    })
  },
  ignoreNULL = FALSE,
  {
    tryCatch({

      if (input$umapColumnToSplit == "None") {
        umapColumnToSplit <- NULL
      } else if (is.null(input$umapColumnToSplit)) {
        umapColumnToSplit <- NULL
      } else {
        umapColumnToSplit <- input$umapColumnToSplit
      }
      umapDF <- inputDataReactive$Results$umapDFList[[paste0("Downsampled.", input$umapDRToPlot)]]

      contrastToUse <- grep(input$umapContrastToUse, inputDataReactive$Results$smd$`Conditions To Test`)
      contrastIndexes <- seq(1, 11, by = 2)[contrastToUse]
      clustersToPlot <- inputDataReactive$Results$selectedClustersList[c(contrastIndexes, contrastIndexes+1)]

      umapDF$cluster_id <- as.character(umapDF$cluster_id)
      if (input$umapShowDAClusters == "All") {
        ctp <- unlist(clustersToPlot)
        if (length(ctp) < 1) {
          showNotification("There are no DA clusters in this contrast!", type = "error")
          umapDF$cluster_id <- factor(umapDF$cluster_id, levels = gtools::mixedsort(unique(umapDF$cluster_id)))
        } else {
          umapDF$cluster_id[which(!umapDF$cluster_id %in% ctp)] <- "Other"
          umapDF$cluster_id <- factor(umapDF$cluster_id, levels = c(ctp, "Other"))
        }
      } else if (input$umapShowDAClusters == "Up only") {
        ctp <- clustersToPlot[[1]]
        if (length(ctp) < 1) {
          showNotification("There are no up DA clusters in this contrast!", type = "error")
          umapDF$cluster_id <- factor(umapDF$cluster_id, levels = gtools::mixedsort(unique(umapDF$cluster_id)))
        } else {
          umapDF$cluster_id[which(!umapDF$cluster_id %in% ctp)] <- "Other"
          umapDF$cluster_id <- factor(umapDF$cluster_id, levels = c(ctp, "Other"))
        }
      } else if (input$umapShowDAClusters == "Down only") {
        ctp <- clustersToPlot[[2]]
        if (length(ctp) < 1) {
          showNotification("There are no down DA clusters in this contrast!", type = "error")
          umapDF$cluster_id <- factor(umapDF$cluster_id, levels = gtools::mixedsort(unique(umapDF$cluster_id)))
        } else {
          umapDF$cluster_id[which(!umapDF$cluster_id %in% ctp)] <- "Other"
          umapDF$cluster_id <- factor(umapDF$cluster_id, levels = c(ctp, "Other"))
        }
      } else {
        umapDF$cluster_id <- factor(umapDF$cluster_id, levels = gtools::mixedsort(unique(umapDF$cluster_id)))
      }

      if (input$umapShowDAClusters != "None") {
        inputDataReactive$Results$coloursList$cluster_id[["Other"]] <- "grey80"
      }

      umapPlot <- ggplot(umapDF, aes(x = x, y = y))
      umapInteractive <- umapPlot
      umapStatic <- umapPlot

      # Interactive plot settings
      if (input$borderSizeUMAP > 0) {
        umapInteractive <- umapInteractive + geom_point(pch = 21, alpha = input$pointAlphaUMAP, size = input$pointSizeUMAP*0.8, stroke = input$borderSizeUMAP, colour = input$umapBorderColour, aes_string(fill = input$umapColumnToPlot))
        umapInteractive <- umapInteractive + scale_fill_manual(values = inputDataReactive$Results$coloursList[[input$umapColumnToPlot]], na.value = "grey78")
        umapInteractive <- umapInteractive + guides(fill = guide_legend(override.aes = list(shape = 21, size = 5, stroke = 0.2)))
      } else {
        umapInteractive <- umapInteractive + geom_point(pch = 20, alpha = input$pointAlphaUMAP, size = input$pointSizeUMAP*0.4, aes_string(colour = input$umapColumnToPlot))
        umapInteractive <- umapInteractive + scale_colour_manual(values = inputDataReactive$Results$coloursList[[input$umapColumnToPlot]], na.value = "grey78")
        umapInteractive <- umapInteractive + guides(colour = guide_legend(override.aes = list(shape = 20, size = 6, stroke = 0.2)))
      }
      umapInteractive <- umapInteractive + theme_void()
      umapInteractive <- umapInteractive + theme(
        legend.text = element_text(size = input$textSizeUMAP, face = "bold"),
        legend.title = element_text(size = input$textSizeUMAP, face = "bold")
      )
      if (!is.null(umapColumnToSplit)) {
        umapInteractive <- eval(parse(text = paste0("umapInteractive + facet_wrap(~", umapColumnToSplit, ", ncol = ", input$umapMainNcol, ")")))
      }

      if (input$borderSizeUMAP > 0) {
        umapStatic <- umapStatic + geom_point(pch = 21, alpha = input$pointAlphaUMAP, size = input$pointSizeUMAP, stroke = input$borderSizeUMAP, colour = input$umapBorderColour, aes_string(fill = input$umapColumnToPlot))
        umapStatic <- umapStatic + guides(fill = guide_legend(override.aes = list(shape = 21, size = 5, stroke = 0.2)))
      } else {
        umapStatic <- umapStatic + geom_point(pch = 20, alpha = input$pointAlphaUMAP, size = input$pointSizeUMAP, aes_string(colour = input$umapColumnToPlot))
        umapStatic <- umapStatic + guides(colour = guide_legend(override.aes = list(shape = 20, size = 6, stroke = 0.2)))
      }
      umapStatic <- umapStatic + theme_prism(base_size = input$textSizeUMAP) +
        theme(
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          legend.text = element_text(size = input$textSizeUMAP*0.8, face = "bold"),
          panel.border = element_rect(colour = "black", fill = NA, size = 1.5),
          legend.key.width = unit(0.4,"cm")
        )
      if (!input$umapShowAxes) {
        umapStatic <- umapStatic + theme(axis.title = element_blank(), element_rect(colour = "black", fill = NA, size = 0.5))
      }
      if (!is.null(umapColumnToSplit)) {
        levelsToSplit <- levels(as.factor(umapDF[[umapColumnToSplit]]))
        labs <- unlist(setNames(lapply(seq_along(levelsToSplit), function(i) {
          paste0(levelsToSplit[[i]], "\n n = ", as.numeric(table(umapDF[[umapColumnToSplit]])[[i]]))
        }), levelsToSplit))
        umapStatic <- eval(parse(text = paste0("umapStatic + facet_wrap(~", umapColumnToSplit, ", ncol = ", input$umapMainNcol, ", labeller = labeller('", umapColumnToSplit,"' = labs))")))
      }
      if (input$umapShowLabels) {
        median <- data.table::rbindlist(lapply(unique(umapDF[[input$umapColumnToPlot]]), function(x) {
          data.frame(
            "V1" = x,
            "x" = median(umapDF[["x"]][umapDF[[input$umapColumnToPlot]] == x]),
            "y" = median(umapDF[["y"]][umapDF[[input$umapColumnToPlot]] == x])
          )
        }))
        colnames(median)[1] <- input$umapColumnToPlot
        umapStatic <- umapStatic + geom_label_repel(data = median, aes_string(label = input$umapColumnToPlot, x = "x", y = "y", fill = input$umapColumnToPlot), show.legend = FALSE, size = input$labelSizeUMAP, nudge_y = input$labelShiftUMAP/5, nudge_x = input$labelShiftUMAP/5)
      }
      umapStatic <- umapStatic + scale_fill_manual(values = inputDataReactive$Results$coloursList[[input$umapColumnToPlot]])
      umapStatic <- umapStatic + scale_colour_manual(values = inputDataReactive$Results$coloursList[[input$umapColumnToPlot]])
      umapStaticLegend <- cowplot::get_legend(umapStatic)

      return(list(
        "umapInteractive" = umapInteractive,
        "umapStatic" = umapStatic
      ))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
)
output$umapInteractive <- renderUI({
  ggplotly(umapReactive()$umapInteractive, height = input$figHeightUMAP, width = input$figWidthUMAP) %>%
    layout(
      legend = list(
        font = list(family = "Arial", size = input$textSizeUMAP),
        title = list(
          font = list(family = "Arial", size = input$textSizeUMAP+2)
        )
      )
    )
})
output$umapStatic <- renderPlot({
  umapReactive()$umapStatic
}, height = function(){input$figHeightUMAP}, width = function(){input$figWidthUMAP})

output$dlUMAP <- downloadHandler(
  filename = function() {
    paste(input$umapColumnToPlot, tolower(input$dlFormat), sep = ".")
  },
  content = function(file) {
    if (input$dlFormat == "PDF") {
      pdf(file = file, width = as.numeric(input$figWidthUMAP / 60), height = as.numeric(input$figHeightUMAP / 60))
    } else if (input$dlFormat == "SVG") {
      svg(file = file, width = as.numeric(input$figWidthUMAP / 60), height = as.numeric(input$figHeightUMAP / 60))
    } else if (input$dlFormat == "PNG") {
      png(filename = file, width = as.numeric(input$figWidthUMAP / 60), height = as.numeric(input$figHeightUMAP / 60), units = "in", res = as.numeric(input$pngRes))
    }
    plot(umapReactive()$umapStatic)
    dev.off()
  }
)

# Feature Plot Inputs ----
observeEvent({
  input$fpColumnToPlot
}, {

  if (input$fpColumnToPlot == "None") {
    fpColumnToPlot <- NULL
  } else if (is.null(input$fpColumnToPlot)) {
    fpColumnToPlot <- NULL
  } else {
    fpColumnToPlot <- input$fpColumnToPlot
  }

  output$plotByBucket <- renderUI({
    bucket_list(
      header = "Drag and drop groups in order to be plotted",
      group_name = "bucket_list_group1",
      orientation = "horizontal",
      add_rank_list(
        text = "Include these groups",
        labels = as.list(levels(as.factor(inputDataReactive$Results[["sce"]][[fpColumnToPlot]]))),
        input_id = "plotByKeepBucket"),
      add_rank_list(
        text = "Exclude these groups",
        labels = NULL,
        input_id = "plotByExcludeBucket")
    )
  })
  outputOptions(output, "plotByBucket", suspendWhenHidden = FALSE)
}, suspended = FALSE)

observeEvent({
  input$fpColumnToSplit
}, {
  if (input$fpColumnToSplit == "None") {
    fpColumnToSplit <- NULL
  } else if (is.null(input$fpColumnToSplit)) {
    fpColumnToSplit <- NULL
  } else {
    fpColumnToSplit <- input$fpColumnToSplit
  }
  if (input$fpColumnToSplit != "None") {
    output$splitByBucket <- renderUI({
      bucket_list(
        header = "Drag and drop groups in order to be plotted",
        group_name = "bucket_list_group2",
        orientation = "horizontal",
        add_rank_list(
          text = "Include these groups",
          labels = as.list(levels(as.factor(inputDataReactive$Results[["sce"]][[fpColumnToSplit]]))),
          input_id = "splitByKeepBucket"),
        add_rank_list(
          text = "Exclude these groups",
          labels = NULL,
          input_id = "splutByExcludeBucket")
      )
    })
    outputOptions(output, "splitByBucket", suspendWhenHidden = FALSE)
  } else {
    output$splitByBucket <- renderText({"Select a variable to split the plots by first!"})
  }
}, suspended = FALSE)
# Make some settings/warnings available depending on user selections
observeEvent(
  {
    input$featurePlotType
  },
  ignoreNULL = FALSE,
  {
    # req(nchar(input$featurePlotType) > 2)
    if (input$featurePlotType == "Feature Plot") {
      output$umapFeaturePlotSettingsUI7 <- renderUI({
        checkboxInput(inputId = "fpDRCustomMinMax", label = "Use custom min/max values?", value = FALSE)
      })
      splitLayout(
        output$umapFeaturePlotSettingsUI8 <- renderUI({
          numericInput(inputId = "fpDRCustomMin", label = "Min", value = 0, min = -Inf, max = Inf, step = 0.5, width = "66%")
        }),
        output$umapFeaturePlotSettingsUI9 <- renderUI({
          numericInput(inputId = "fpDRCustomMax", label = "Max", value = 6, min = -Inf, max = Inf, step = 0.5, width = "66%")
        })
      )
    } else {
      lapply(7:9, function(i) {
        output[[paste0("umapFeaturePlotSettingsUI", i)]] <- renderUI({
          NULL
        })
      })
    }
    if (input$featurePlotType == "Feature Plot" | input$featurePlotType == "Nebulosa Plot") {
      output$umapFeaturePlotSettingsUI0 <- renderUI({
        selectInput(inputId = "fpDRToPlot", label = "DR to plot", choices = names(inputDataReactive$Results$scData@reductions), selected = "UMAP", multiple = FALSE, width = "85%")
      })
      output$umapFeaturePlotSettingsUI1 <- renderUI({
        checkboxInput(inputId = "fpShowAxes", label = "Show plot axes?", value = FALSE)
      })
      output$umapFeaturePlotSettingsUI2 <- renderUI({
        checkboxInput(inputId = "fpShowLabels", label = "Show cluster labels?", value = FALSE)
      })
      output$umapFeaturePlotSettingsUI3 <- renderUI({
        checkboxInput(inputId = "cellBordersFP", label = "Show cell border?", value = TRUE)
      })
      splitLayout(
        output$umapFeaturePlotSettingsUI4 <- renderUI({
          sliderInput(inputId = "pointSizeFP", label = "Dot Size", min = 0.1, max = 4, value = 0.8, step = 0.1, width = "85%", ticks = F)
        }),
        output$umapFeaturePlotSettingsUI5 <- renderUI({
          sliderInput(inputId = "borderSizeFP", label = "Dot border size", min = 0, max = 10, value = 0, step = 1, width = "85%", ticks = F)
        })
      )
      output$umapFeaturePlotSettingsUI6 <- renderUI({
        radioButtons(inputId = "fpLabelColour", label = "Colour cluster labels by:", choiceNames = c("Label colour", "Gene median", "Gene mean"), choiceValues = c("label", "median", "mean"))
      })
      output$umapFeaturePlotSettingsUI10 <- renderUI({
        checkboxInput(inputId = "rasteriseFP", label = "Rasterise?", value = FALSE)
      })
      output$umapFeaturePlotSettingsUI11 <- renderUI({
        numericInput(inputId = "rasterFP_DPI", label = "Raster DPI", value = 1024, min = 0, max = 2000, step = 5, width = "85%")
      })
    } else {
      lapply(c(0, 1:6, 10, 11), function(i) {
        output[[paste0("umapFeaturePlotSettingsUI", i)]] <- renderUI({
          NULL
        })
      })
    }
    if (input$featurePlotType == "Nebulosa Plot") {
      output$fpNebulosaOutputUI1 <- renderUI({
        checkboxInput(inputId = "fpNebulosaPlotTogether", label = "Show joint plot?", value = TRUE)
      })
      output$fpNebulosaOutputUI2 <- renderUI({
        checkboxInput(inputId = "fpNebulosaPlotTogetherOnly", label = "Show only joint plot?", value = FALSE)
      })
    } else {
      output$fpNebulosaOutputUI1 <- NULL
      output$fpNebulosaOutputUI2 <- NULL
    }

    if (input$featurePlotType == "Dot Plot") {
      output$umapFeaturePlotDotPlotUI2 <- renderUI({
        checkboxInput(inputId = "umapFeaturePlotDotplotFlip", label = "Flip dot plot?", value = TRUE)
      })
      outputOptions(output, "umapFeaturePlotDotPlotUI2", suspendWhenHidden = FALSE)
    } else {
      output$umapFeaturePlotDotPlotUI1 <- renderUI({
        NULL
      })
      output$umapFeaturePlotDotPlotUI2 <- renderUI({
        NULL
      })
    }
    
    if (input$featurePlotType == "Heatmap") {
      if (!is.null(input$fpQCToPlot)) {
        output$umapFeaturePlotWarningUI <- renderUI({
          renderText("QC metrics are not available for Heatmap plots. Please select a different plot type or remove them from your selection.")
        })
      } else {
        output$umapFeaturePlotWarningUI <- renderUI({
          NULL
        })
      }
      output$umapFeaturePlotHeatmapUI1 <- renderUI({
        checkboxInput(inputId = "umapFeaturePlotHeatmapCluster", label = "Cluster heatmap?", value = TRUE)
      })
      output$umapFeaturePlotHeatmapUI2 <- renderUI({
        checkboxInput(inputId = "umapFeaturePlotHeatmapFlip", label = "Flip heatmap?", value = TRUE)
      })
    } else {
      output$umapFeaturePlotHeatmapUI1 <- renderUI({
        NULL
      })
      output$umapFeaturePlotHeatmapUI2 <- renderUI({
        NULL
      })
    }
    if (input$featurePlotType %in% c("Heatmap", "Individual Heatmap", "Dot Plot")) {
      output$fpHeatmapOutputUI1 <- renderUI({
        checkboxInput(inputId = "fpHeatmapPlotAll", label = "Plot all available features?", value = FALSE)
      })
      outputOptions(output, "fpHeatmapOutputUI1", suspendWhenHidden = FALSE)
    } else {
      output$fpHeatmapOutputUI1 <- renderUI({NULL})
    }
    if (input$featurePlotType == "Barplot") {
      output$fpBarplotOptionsUI1 <- renderUI({
        checkboxInput(inputId = "fpBarplotPercentage", label = "Make barplot fractional?", value = FALSE)
      })
      output$fpBarplotOptionsUI2 <- renderUI({
        checkboxInput(inputId = "fpBarplotShowNumbers", label = "Show numbers?", value = FALSE)
      })
      output$fpBarplotOutputUI3 <- renderUI({
        downloadButton(outputId = "dlBarplotCounts", label = "Download Barplot Counts")
      })
    } else {
      output$fpBarplotOutputUI1 <- NULL
      output$fpBarplotOutputUI2 <- NULL
      output$fpBarplotOptionsUI1 <- NULL
      output$fpBarplotOptionsUI2 <- NULL
      output$fpBarplotOutputUI3 <- NULL
    }
  }
)

# Some stuff so that if the user plots all features in the heatmap, when they go to a different feature plot, the bucket goes back to how it was
previousFeatureSelection <- reactiveVal(NULL)
observeEvent({
  input$fpHeatmapPlotAll
  input$featurePlotType
  }, ignoreNULL = TRUE, {
  req(!is.null(input$featurePlotType))
  req(!is.null(input$fpHeatmapPlotAll))
  if (input$featurePlotType %in% c("Heatmap", "Individual Heatmap", "Dot Plot")) {
    if (input$fpHeatmapPlotAll) {
      currentlySelectedGenes <- previousFeatureSelection(input$fpFeatureToPlot)
      updateSelectInput(session = session, inputId = "fpFeatureToPlot", selected = names(inputDataReactive$Results$sce))
    } else {
      updateSelectInput(session = session, inputId = "fpFeatureToPlot", selected = previousFeatureSelection())
    }
  } else {
    updateSelectInput(session = session, inputId = "fpFeatureToPlot", selected = previousFeatureSelection())
  }
})

# Feature plots ----
featurePlotReactive <- reactiveValues(fp = NULL)
observeEvent(input$featurePlotType, {
  featurePlotReactive <- reactiveValues(fp = NULL)
})
observeEvent(
  {
    input$fpDRToPlot
    input$fpFeatureToPlot
    input$featurePlotType
    input$fpAssayToPlot
    input$fpColumnToPlot
    input$fpColumnToSplit
    input$pointSizeFP
    input$textSizeFP
    input$ncolFPGene
    input$ncolFPSplit
    input$fpShowLabels
    input$viridisColourFP
    input$flipViridisFP
    input$umapFeaturePlotHeatmapCluster
    input$umapFeaturePlotHeatmapFlip
    input$fpLegendPosition
    input$cellBordersFP
    input$borderSizeFP
    input$fpShowAxes
    input$umapFeaturePlotDotplotFlip
    input$fpBarplotPercentage
    input$fpBarplotShowNumbers
    input$plotByKeepBucket
    input$splitByKeepBucket
    input$fpLabelColour
    input$fpNebulosaPlotTogether
    input$fpNebulosaPlotTogetherOnly
    input$rasteriseFP
    input$rasterFP_DPI
    input$fpContrastToUse
    input$fpShowDAClusters
    input$fpHeatmapPlotAll
    input$fpSubsetCells
    input$fpSubsetToGlobal
    lapply(names(colsList1), function(col) {
      lapply(names(colsList1[[col]]), function(lor) {
        input[[paste0("GroupColour", col, lor)]]
      })
    })
  },
  ignoreNULL = FALSE,
  {
    tryCatch({
      if (input$fpSubsetCells) {
        scDataToFP <- inputDataReactive$Results$scDataToFPSubset
      } else {
        scDataToFP <- inputDataReactive$Results$scDataToFP
      }
      
      if (input$viridisColourFP %in% viridisColours) {
        use_viridis = TRUE
        viridis.palette = input$viridisColourFP
      } else {
        use_viridis = FALSE
        viridis.palette = "viridis"
      }
      if (input$flipViridisFP) {
        viridisFlip <- -1
      } else {
        viridisFlip <- 1
      }

      if (input$fpColumnToPlot == "None") {
        fpColumnToPlot <- NULL
      } else if (is.null(input$fpColumnToPlot)) {
        fpColumnToPlot <- NULL
      } else {
        fpColumnToPlot <- input$fpColumnToPlot
        Seurat::Idents(scDataToFP) <- input$fpColumnToPlot
      }
      if (input$fpColumnToSplit == "None") {
        fpColumnToSplit <- NULL
      } else if (is.null(input$fpColumnToSplit)) {
        fpColumnToSplit <- NULL
      } else {
        fpColumnToSplit <- input$fpColumnToSplit
      }

      # Take the inputs from the buckets and re-order/remove as required
      cellsToKeep <- rownames(scDataToFP@meta.data)[which(scDataToFP@meta.data[[fpColumnToPlot]] %in% input$plotByKeepBucket)]
      scDataToFP <- subset(scDataToFP, cells = cellsToKeep)
      scDataToFP@meta.data[[fpColumnToPlot]] <- factor(scDataToFP@meta.data[[fpColumnToPlot]], levels = input$plotByKeepBucket)
      if (!is.null(fpColumnToSplit)) {
        cellsToKeep <- rownames(scDataToFP@meta.data)[which(scDataToFP@meta.data[[fpColumnToSplit]] %in% input$splitByKeepBucket)]
        scDataToFP <- subset(scDataToFP, cells = cellsToKeep)
        scDataToFP@meta.data[[fpColumnToSplit]] <- factor(scDataToFP@meta.data[[fpColumnToSplit]], levels = input$splitByKeepBucket)
      }

      # If user selects to plot only DA clusters, subset to those cells
      contrastToUse <- grep(input$fpContrastToUse, inputDataReactive$Results$smd$`Conditions To Test`)
      contrastIndexes <- seq(1, 11, by = 2)[contrastToUse]
      clustersToPlot <- inputDataReactive$Results$selectedClustersList[c(contrastIndexes, contrastIndexes+1)]
      if (input$fpShowDAClusters == "All") {
        cellsToKeep <- rownames(scDataToFP@meta.data)[which(scDataToFP@meta.data$cluster_id %in% as.character(unlist(clustersToPlot)))]
        if (length(cellsToKeep) <= 1) {
          showNotification("There are no DA clusters in this contrast!", type = "error")
        } else {
          scDataToFP <- subset(scDataToFP, cells = cellsToKeep)
        }
      } else if (input$fpShowDAClusters == "Up only") {
        cellsToKeep <- rownames(scDataToFP@meta.data)[which(scDataToFP@meta.data$cluster_id %in% clustersToPlot[[1]])]
        if (length(cellsToKeep) <= 1) {
          showNotification("There are no up DA clusters in this contrast!", type = "error")
        } else {
          scDataToFP <- subset(scDataToFP, cells = cellsToKeep)
        }
      } else if (input$fpShowDAClusters == "Down only") {
        cellsToKeep <- rownames(scDataToFP@meta.data)[which(scDataToFP@meta.data$cluster_id %in% clustersToPlot[[2]])]
        if (length(cellsToKeep) <= 1) {
          showNotification("There are no down DA clusters in this contrast!", type = "error")
        } else {
          scDataToFP <- subset(scDataToFP, cells = cellsToKeep)
        }
      }

      fpFeaturesToPlot <- input$fpFeatureToPlot %>% gsub("_", "-", .)
      
      # Make the plots!
      if (input$featurePlotType == "Feature Plot") {
        fp <- lapply(fpFeaturesToPlot, function(gene) {
          gtp <- gene #%>% gsub("-", "_", .)
          # gene <- gene %>% gsub("_", "-", .)
          umapDF <- inputDataReactive$Results$umapDFList$Downsampled
          umapDF <- umapDF[order(umapDF[[gtp]], decreasing = F),]
          median <- data.table::rbindlist(lapply(levels(as.factor(umapDF[[input$fpColumnToPlot]])), function(x) {
            data.frame(
              "V1" = x,
              "x" = median(umapDF[["x"]][umapDF[[input$fpColumnToPlot]] == x]),
              "y" = median(umapDF[["y"]][umapDF[[input$fpColumnToPlot]] == x]),
              "median" = median(umapDF[[gtp]][umapDF[[input$fpColumnToPlot]] == x], na.rm = TRUE),
              "mean" = mean(umapDF[[gtp]][umapDF[[input$fpColumnToPlot]] == x], na.rm = TRUE),
              "max" = max(umapDF[[gtp]][umapDF[[input$fpColumnToPlot]] == x], na.rm = TRUE)
            )
          }))
          colnames(median)[1] <- input$fpColumnToPlot
          fp1 <- do_FeaturePlot(
            sample = scDataToFP,
            assay = "originalexp",
            slot = input$fpAssayToPlot,
            reduction = input$fpDRToPlot,
            features = gene
          )
          fpData <- fp1[[1]][["data"]] %>% data.frame(check.names = F) %>% rownames_to_column("cell-id")
          colnames(fpData)[[5]] <- gene
          fPData <- left_join(fpData, (scDataToFP@meta.data %>% data.frame(check.names = F) %>% rownames_to_column("cell-id")), by = "cell-id")
          fPData <- fPData[order(fPData[[gene]], decreasing = F),]
          colnames(fPData)[2:3] <- c('dim1', 'dim2')
          fp2 <- ggplot(fPData, aes(x = dim1, y = dim2))
          if (!input$rasteriseFP) {
            if(!input$cellBordersFP | input$borderSizeFP == 0) {
              fp2 <- fp2 + geom_point(aes(colour = .data[[gene]]), size = input$pointSizeFP)
              if (input$viridisColourFP %in% viridisColours) {
                fp2 <- fp2 + scale_colour_viridis_c(option = input$viridisColourFP, direction = viridisFlip)
              } else if (input$viridisColourFP %in% scicoColours) {
                fp2 <- fp2 + scale_colour_scico(palette = input$viridisColourFP, direction = viridisFlip)
              } else if (input$viridisColourFP %in% divergingColours) {
                fp2 <- fp2 + scale_colour_distiller(palette = input$viridisColourFP, direction = viridisFlip, type = "div")
              }
            } else {
              fp2 <- fp2 + geom_point(aes(colour = .data[[gene]]), size = input$pointSizeFP, pch = 21, stroke = input$borderSizeFP/10)
              if (input$viridisColourFP %in% viridisColours) {
                fp2 <- fp2 + scale_fill_viridis_c(option = input$viridisColourFP, direction = viridisFlip)
              } else if (input$viridisColourFP %in% scicoColours) {
                fp2 <- fp2 + scale_fill_scico(palette = input$viridisColourFP, direction = viridisFlip)
              } else if (input$viridisColourFP %in% divergingColours) {
                fp2 <- fp2 + scale_fill_distiller(palette = input$viridisColourFP, direction = viridisFlip, type = "div")
              }
            }
          } else {
            fp2 <- fp2 + geom_scattermore(pointsize = (input$pointSizeFP*2)+0.6, pixels = c(input$rasterFP_DPI, input$rasterFP_DPI), aes(colour = .data[[gene]]))
            if (input$viridisColourFP %in% viridisColours) {
              fp2 <- fp2 + scale_colour_viridis_c(option = input$viridisColourFP, direction = viridisFlip)
            } else if (input$viridisColourFP %in% scicoColours) {
              fp2 <- fp2 + scale_colour_scico(palette = input$viridisColourFP, direction = viridisFlip)
            } else if (input$viridisColourFP %in% divergingColours) {
              fp2 <- fp2 + scale_colour_distiller(palette = input$viridisColourFP, direction = viridisFlip, type = "div")
            }
          }
          fp2 <- fp2 + theme_prism(base_size = input$textSizeFP) +
            theme(
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.line = element_blank(),
              legend.text = element_text(size = input$textSizeFP*0.8, face = "bold"),
              panel.border = element_rect(colour = "black", fill = NA, size = 1.5),
              legend.key.width = unit(0.4,"cm")
            ) +
            theme(legend.position = tolower(input$fpLegendPosition)) +
            ggtitle(gene)
          if (!input$fpShowAxes) {
            fp2 <- fp2 + theme(axis.title = element_blank(), element_rect(colour = "black", fill = NA, size = 0.5))
          }
          if (!is.null(fpColumnToSplit)) {
            levelsToSplit <- levels(as.factor(umapDF[[fpColumnToSplit]]))
            labs <- unlist(setNames(lapply(seq_along(levelsToSplit), function(i) {
              paste0(levelsToSplit[[i]], "\n n = ", as.numeric(table(fPData[[fpColumnToSplit]])[[i]]))
            }), levelsToSplit))
            fp2 <- eval(parse(text = paste0("fp2 + facet_wrap(~", fpColumnToSplit, ", ncol = ", input$ncolFPSplit, ", labeller = labeller('", fpColumnToSplit,"' = labs))")))
          }
          if (input$fpShowLabels) {
            fp2 <- fp2 + new_scale_color() + new_scale_fill()
            if (input$fpLabelColour == "label") {
              fp2 <- fp2 +
                geom_label_repel(data = median, aes_string(label = input$fpColumnToPlot, x = "x", y = "y", fill = input$fpColumnToPlot), show.legend = FALSE, size = input$textSizeFP/4, max.overlaps = 100) +
                scale_fill_manual(values = inputDataReactive$Results$coloursList[[input$fpColumnToPlot]])
            } else {
              fp2 <- fp2 +
                geom_label_repel(data = median, aes_string(label = input$fpColumnToPlot, x = "x", y = "y", fill = input$fpLabelColour), show.legend = FALSE, size = input$textSizeFP/4, max.overlaps = 100)
              if (input$viridisColourFP %in% viridisColours) {
                fp2 <- fp2 + scale_fill_viridis_c(option = input$viridisColourFP, direction = viridisFlip)
              } else if (input$viridisColourFP %in% scicoColours) {
                fp2 <- fp2 + scale_fill_scico(palette = input$viridisColourFP, direction = viridisFlip)
              } else if (input$viridisColourFP %in% divergingColours) {
                fp2 <- fp2 + scale_fill_distiller(palette = input$viridisColourFP, direction = viridisFlip, type = "div")
              }
            }
          }
          fp2
        })
      } else if (input$featurePlotType == "Nebulosa Plot") {
        # req(any(nchar(fpFeaturesToPlot) >= 2))
        require("Nebulosa")
        combine = TRUE
        joint = input$fpNebulosaPlotTogether
        return_only_joint = input$fpNebulosaPlotTogetherOnly
        if (length(fpFeaturesToPlot) == 1) {
          combine = FALSE
          joint = FALSE
          return_only_joint = FALSE
        }
        fp <- do_NebulosaPlot(
          sample = scDataToFP,
          features = fpFeaturesToPlot,
          slot = input$fpAssayToPlot,
          reduction = input$fpDRToPlot,
          combine = combine,
          joint = joint,
          return_only_joint = return_only_joint
        )
        if (!combine | return_only_joint) {
          fp <- wrap_plots(fp)
        }
        fp <- lapply(seq_along(fp), function(i) {
          fpData <- fp[[i]][["data"]]
          colnames(fpData)[1:2] <- c("x", "y")
          fpData <- fpData %>%
            data.frame(check.names = F) %>%
            rownames_to_column("cell-id") %>%
            left_join((scDataToFP@meta.data %>% data.frame(check.names = F) %>% rownames_to_column("cell-id")), by = "cell-id")
          median <- data.table::rbindlist(lapply(input$plotByKeepBucket, function(x) {
            data.frame(
              "V1" = x,
              "x" = median(fpData[["x"]][fpData[[input$fpColumnToPlot]] == x]),
              "y" = median(fpData[["y"]][fpData[[input$fpColumnToPlot]] == x]),
              "median" = median(fpData[["feature"]][fpData[[input$fpColumnToPlot]] == x], na.rm = TRUE),
              "mean" = mean(fpData[["feature"]][fpData[[input$fpColumnToPlot]] == x], na.rm = TRUE),
              "max" = max(fpData[["feature"]][fpData[[input$fpColumnToPlot]] == x], na.rm = TRUE)
            )
          }))
          colnames(median)[1] <- input$fpColumnToPlot
          median <- as.data.frame(median)
          median <- median[order(median$median),]
          fp2 <- ggplot(fpData, aes_string(x = colnames(fpData)[2], y = colnames(fpData)[3]))
          if (!input$rasteriseFP) {
            if(!input$cellBordersFP | input$borderSizeFP == 0) {
              fp2 <- fp2 + eval(parse(text = paste0("geom_point(aes(colour = feature), size = ", input$pointSizeFP, ")")))
              if (input$viridisColourFP %in% viridisColours) {
                fp2 <- fp2 + scale_colour_viridis_c(option = input$viridisColourFP, direction = viridisFlip)
              } else if (input$viridisColourFP %in% scicoColours) {
                fp2 <- fp2 + scale_colour_scico(palette = input$viridisColourFP, direction = viridisFlip)
              } else if (input$viridisColourFP %in% divergingColours) {
                fp2 <- fp2 + scale_colour_distiller(palette = input$viridisColourFP, direction = viridisFlip, type = "div")
              }
            } else {
              fp2 <- fp2 + eval(parse(text = paste0("geom_point(aes(fill = feature), size = ", input$pointSizeFP, ", pch = 21, stroke = ", input$borderSizeFP/10, ")")))
              if (input$viridisColourFP %in% viridisColours) {
                fp2 <- fp2 + scale_fill_viridis_c(option = input$viridisColourFP, direction = viridisFlip)
              } else if (input$viridisColourFP %in% scicoColours) {
                fp2 <- fp2 + scale_fill_scico(palette = input$viridisColourFP, direction = viridisFlip)
              } else if (input$viridisColourFP %in% divergingColours) {
                fp2 <- fp2 + scale_fill_distiller(palette = input$viridisColourFP, direction = viridisFlip, type = "div")
              }
            }
          } else {
            # req(!is.null(input$rasterFP_DPI))
            fp2 <- fp2 + eval(parse(text = paste0("geom_scattermore(pointsize = ", (input$pointSizeFP*2)+0.6, ", pixels = c(", input$rasterFP_DPI, ",", input$rasterFP_DPI,"), aes(colour = feature))")))
            if (input$viridisColourFP %in% viridisColours) {
              fp2 <- fp2 + scale_colour_viridis_c(option = input$viridisColourFP, direction = viridisFlip)
            } else if (input$viridisColourFP %in% scicoColours) {
              fp2 <- fp2 + scale_colour_scico(palette = input$viridisColourFP, direction = viridisFlip)
            } else if (input$viridisColourFP %in% divergingColours) {
              fp2 <- fp2 + scale_colour_distiller(palette = input$viridisColourFP, direction = viridisFlip, type = "div")
            }
          }
          fp2 <- fp2 + theme_prism(base_size = input$textSizeFP) +
            theme(
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.line = element_blank(),
              legend.text = element_text(size = input$textSizeFP*0.8, face = "bold"),
              panel.border = element_rect(colour = "black", fill = NA, size = 1.5),
              legend.key.width = unit(0.4,"cm")
            ) +
            theme(legend.position = tolower(input$fpLegendPosition)) +
            ggtitle(fp[[i]]$labels$title)
          if (!input$fpShowAxes) {
            fp2 <- fp2 + theme(axis.title = element_blank(), element_rect(colour = "black", fill = NA, size = 0.5))
          }
          if (!is.null(fpColumnToSplit)) {
            levelsToSplit <- levels(as.factor(fpData[[fpColumnToSplit]]))
            labs <- unlist(setNames(lapply(seq_along(levelsToSplit), function(i) {
              paste0(levelsToSplit[[i]], "\n n = ", as.numeric(table(fpData[[fpColumnToSplit]])[[i]]))
            }), levelsToSplit))
            fp2 <- eval(parse(text = paste0("fp2 + facet_wrap(~", fpColumnToSplit, ", ncol = ", input$ncolFPSplit, ", labeller = labeller('", fpColumnToSplit,"' = labs))")))
          }
          if (input$fpShowLabels) {
            fp2 <- fp2 + new_scale_color() + new_scale_fill()
            if (input$fpLabelColour == "label") {
              fp2 <- fp2 +
                geom_label_repel(data = median, aes_string(label = input$fpColumnToPlot, x = "x", y = "y", fill = input$fpColumnToPlot), show.legend = FALSE, size = input$textSizeFP/4, max.overlaps = 100) +
                scale_fill_manual(values = catalystCols, breaks = levels(as.factor(fpData[[input$fpColumnToPlot]])))
            } else {
              fp2 <- fp2 +
                geom_label_repel(data = median, aes_string(label = input$fpColumnToPlot, x = "x", y = "y", fill = input$fpLabelColour), show.legend = FALSE, size = input$textSizeFP/4, max.overlaps = 100)
              if (input$viridisColourFP %in% viridisColours) {
                fp2 <- fp2 + scale_fill_viridis_c(option = input$viridisColourFP, direction = viridisFlip)
              } else if (input$viridisColourFP %in% scicoColours) {
                fp2 <- fp2 + scale_fill_scico(palette = input$viridisColourFP, direction = viridisFlip)
              } else if (input$viridisColourFP %in% divergingColours) {
                fp2 <- fp2 + scale_fill_distiller(palette = input$viridisColourFP, direction = viridisFlip, type = "div")
              }
            }
          }
          fp2
        })
      } else if (input$featurePlotType == "Violin Plot") {
        # req(any(nchar(fpFeaturesToPlot) >= 2))
        if (!is.null(fpColumnToSplit)) {
          colsToViolin <- fpColumnToSplit
        } else {
          colsToViolin <- fpColumnToPlot
        }
        fp <- do_ViolinPlot(
          sample = scDataToFP,
          slot = input$fpAssayToPlot,
          features = fpFeaturesToPlot,
          pt.size = input$pointSizeFP,
          group.by = fpColumnToPlot,
          split.by = fpColumnToSplit,
          plot_boxplot = F,
          ncol = input$ncolFP,
          font.size = input$textSizeFP,
          legend.position = "none"
        )
        if (length(fpFeaturesToPlot) == 1) {
          fp <- wrap_plots(fp)
        }
        for (i in seq_along(fp)) {
          fp[[i]] <- fp[[i]] + scale_fill_manual(values = inputDataReactive$Results$coloursList[[colsToViolin]])
        }
      } else if (input$featurePlotType == "Individual Heatmap") {
        Seurat::Idents(scDataToFP) <- fpColumnToPlot
        fp <- Seurat::DoHeatmap(
          object = scDataToFP,
          assay = "originalexp",
          slot = input$fpAssayToPlot,
          features = fpFeaturesToPlot,
          group.by = fpColumnToPlot,
          group.colors = inputDataReactive$Results$coloursList[[fpColumnToPlot]],
          size = input$textSizeFP/3,
          vjust = 0.1,
          group.bar = TRUE
        )
        if (input$viridisColourFP %in% scicoColours) {
          fp <- fp + scico::scale_fill_scico(palette = input$viridisColourFP, midpoint = 0, direction = viridisFlip)
        } else if (input$viridisColourFP %in% divergingColours) {
          fp <- fp + scale_fill_distiller(palette = input$viridisColourFP, direction = viridisFlip, type = "div")
        } else if (input$viridisColourFP %in% viridisColours) {
          fp <- fp + scale_fill_viridis_c(option = input$viridisColourFP, direction = viridisFlip)
        }
      } else if (input$featurePlotType == "Dot Plot") {
        fp <- do_DotPlot(
          sample = scDataToFP,
          features = fpFeaturesToPlot,
          group.by = fpColumnToPlot,
          use_viridis = use_viridis,
          viridis.direction = viridisFlip,
          viridis.palette = viridis.palette,
          legend.width = 1,
          legend.length = 9,
          font.size = input$textSizeFP,
          plot.grid = TRUE,
          dot.scale = input$pointSizeFP*5,
          flip = input$umapFeaturePlotDotplotFlip
        )
        if (input$viridisColourFP %in% scicoColours) {
          fp <- fp + scico::scale_fill_scico(palette = input$viridisColourFP, direction = viridisFlip)
        } else if (input$viridisColourFP %in% divergingColours) {
          fp <- fp + scale_fill_distiller(palette = input$viridisColourFP, direction = viridisFlip, type = "div")
        }
      } else if (input$featurePlotType == "Heatmap") {
        fp <- do_ExpressionHeatmap(
          sample = scDataToFP,
          features = fpFeaturesToPlot,
          group.by = fpColumnToPlot,
          slot = input$fpAssayToPlot,
          use_viridis = use_viridis,
          viridis.direction = viridisFlip,
          viridis.palette = viridis.palette,
          flip = input$umapFeaturePlotHeatmapFlip,
          cluster = input$umapFeaturePlotHeatmapCluster
        )
        if (input$viridisColourFP %in% scicoColours) {
          fp <- fp + scico::scale_fill_scico(palette = input$viridisColourFP, direction = viridisFlip)
        } else if (input$viridisColourFP %in% divergingColours) {
          fp <- fp + scale_fill_distiller(palette = input$viridisColourFP, direction = viridisFlip, type = "div")
        }
      } else if (input$featurePlotType == "Ridge Plot") {
        # req(any(nchar(fpFeaturesToPlot) >= 2))
        if (length(fpFeaturesToPlot) == 1) {
          fp <- do_RidgePlot(
            sample = scDataToFP,
            feature = fpFeaturesToPlot,
            group.by = fpColumnToPlot,
            split.by = fpColumnToSplit,
            slot = input$fpAssayToPlot,
            assay = "originalexp",
            font.size = input$textSizeFP,
            colors.use = inputDataReactive$Results$coloursList[[input$fpColumnToPlot]]
          )
        } else if (length(fpFeaturesToPlot) >= 2) {
          fp <- lapply(fpFeaturesToPlot, function(x) {
            do_RidgePlot(
              sample = scDataToFP,
              feature = x,
              group.by = fpColumnToPlot,
              split.by = fpColumnToSplit,
              slot = input$fpAssayToPlot,
              assay = "originalexp",
              font.size = input$textSizeFP,
              colors.use = inputDataReactive$Results$coloursList[[input$fpColumnToPlot]]
            )
          })
        }
      } else if (input$featurePlotType == "Barplot") {
        umapDF <- inputDataReactive$Results$umapDFList$All
        if (input$fpBarplotPercentage) {
          position <- "fill"
        } else {
          position <- "stack"
        }
        if (is.null(fpColumnToSplit)) {
          fp <- ggplot(umapDF, aes_string(x = fpColumnToPlot, fill = fpColumnToPlot))
        } else {
          fp <- ggplot(umapDF, aes_string(x = fpColumnToSplit, fill = fpColumnToPlot))
        }
        fp <- fp +
          geom_bar(stat = "count", position = position) +
          scale_fill_manual(values = inputDataReactive$Results$coloursList[[input$fpColumnToPlot]]) +
          theme_classic(base_size = input$textSizeFP) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
        if (input$fpBarplotShowNumbers) {
          fp <- fp + geom_text(stat='count', aes(label=..count..), vjust=-1)
        }
        if (is.null(fpColumnToSplit)) {
          dfX <- as.data.frame(table(umapDF[[fpColumnToPlot]]))
          colnames(dfX) <- c(fpColumnToPlot, "Count")
        } else {
          dfX <- as.data.frame(table(umapDF[[fpColumnToPlot]], umapDF[[fpColumnToSplit]]))
          dfX <- spread(dfX, key = Var2, value = Freq)
          colnames(dfX)[[1]] <- fpColumnToPlot
        }
        output$fpBarplotOutputUI2 <- renderUI({
            output$fpBarplotTable <- DT::renderDataTable(dfX, rownames = F)
            DT::dataTableOutput("fpBarplotTable", fill = FALSE)
        })
        output$dlBarplotCounts <- downloadHandler(
          filename = function() {
            paste(fpColumnToPlot, "_barplot_counts.xlsx")
          },
          content = function(file) {
            openxlsx::write.xlsx(x = as.data.frame(dfX), file = file)
          }
        )
      }
      featurePlotReactive$fp <- fp
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  })

output$featurePlotOutput <- renderPlot(
  {
    req(!is.null(input$fpFeatureToPlot), length(input$fpFeatureToPlot) > 0)
    if (input$featurePlotType %in% c("Feature Plot", "Nebulosa Plot", "Ridge Plot") && length(input$fpFeatureToPlot) >= 2) {
      gridExtra::grid.arrange(grobs = featurePlotReactive$fp, ncol = input$ncolFPGene)
    } else {
      featurePlotReactive$fp
    }
  },
  height = function(){input$figHeightFP},
  width = function(){input$figWidthFP}
)

output$dlFP <- downloadHandler(
  filename = function() {
    paste((input$featurePlotType %>% gsub(" ", "", .)), paste(input$fpFeatureToPlot, collapse = "_"), tolower(input$dlFormat), sep = ".")
  },
  content = function(file) {
    if (input$dlFormat == "PDF") {
      pdf(file = file, width = as.numeric(input$figWidthFP / 60), height = as.numeric(input$figHeightFP / 60))
    } else if (input$dlFormat == "SVG") {
      svg(file = file, width = as.numeric(input$figWidthFP / 60), height = as.numeric(input$figHeightFP / 60))
    } else if (input$dlFormat == "PNG") {
      png(filename = file, width = as.numeric(input$figWidthFP / 60), height = as.numeric(input$figHeightFP / 60), units = "in", res = as.numeric(input$pngRes))
    }
    if (input$featurePlotType %in% c("Feature Plot", "Nebulosa Plot", "Ridge Plot") && length(input$fpFeatureToPlot) >= 2) {
      gridExtra::grid.arrange(grobs = featurePlotReactive$fp, ncol = input$ncolFPGene)
    } else {
      print(featurePlotReactive$fp)
    }
    dev.off()
  }
)

## Download modified FCS files ----
output$downloadClusterCodes <- downloadHandler(
  filename = function(){
    "clusterCodes.xlsx"
  },
  content = function(file){
    # Get the full DR df as this has all we need
    umap_a <- inputDataReactive$Results$umapDFList$All
    # Code the cluster IDs to a numerical table, and save it for downloading later
    clusterCodes <- data.frame(
      "cluster_ids" = levels(umap_a$cluster_id),
      "cluster_id_codes" = 1:nlevels(umap_a$cluster_id)
    )
    # If there are new cluster IDs, append them
    if ("relabelled_clusters" %in% colnames(umap_a)) {
      clusterCodes$relabelled_clusters <- umap_a$relabelled_clusters[match(clusterCodes$cluster_ids, umap_a$cluster_id)]
      clusterCodes$new_cluster_codes <- as.numeric(factor(clusterCodes$relabelled_clusters))
    }
    writexl::write_xlsx(clusterCodes, path = file)
  }
)

output$downloadFCS <- downloadHandler(
  filename = function(){
    paste("modified_fcs_files_", Sys.Date(), ".zip", sep = "")
  },
  content = function(file){

    # Create the temp dir
    temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
    dir.create(temp_directory)

    # Get the full DR df as this has all we need
    umap_a <- inputDataReactive$Results$umapDFList$All
    # Code the cluster IDs to a numerical table, and save it for downloading later
    clusterCodes <- data.frame(
      "cluster_ids" = levels(umap_a$cluster_id),
      "cluster_id_codes" = 1:nlevels(umap_a$cluster_id)
    )
    # If there are new cluster IDs, append them
    if ("relabelled_clusters" %in% colnames(umap_a)) {
      clusterCodes$relabelled_clusters <- umap_a$relabelled_clusters[match(clusterCodes$cluster_ids, umap_a$cluster_id)]
      clusterCodes$new_cluster_codes <- as.numeric(factor(clusterCodes$relabelled_clusters))
    }

    # For every sample, add the DR coords
    fcsFilesList <- lapply(levels(inputDataReactive$Results$md$sample_id), function(s) {
      umap_xx <- umap_a[which(umap_a$sample_id == s),]
      apps <- data.frame(
        umap_x = umap_xx$x, umap_y = umap_xx$y, cluster_id_codes = clusterCodes$cluster_id_codes[match(umap_xx$cluster_id, clusterCodes$cluster_ids)]
      )
      if ("relabelled_clusters" %in% colnames(umap_xx)) {
        apps$new_cluster_codes <- clusterCodes$new_cluster_codes[match(umap_xx$cluster_id, clusterCodes$cluster_ids)]
      }
      fn1 <- inputDataReactive$Results$md$file_name[inputDataReactive$Results$md$sample_id == s]
      fn2 <- file.path(temp_directory, paste0(s, "_modified.fcs"))

      write.FCS(
        x = fr_append_cols(fr = inputDataReactive$Results$framesList$`All Cells`[[fn1]], cols = as.matrix(apps)),
        filename = fn2,
        delimiter="#"
      )
    }) %>% setNames(unique(inputDataReactive$Results$md$sample_id))

    zip::zip(
      zipfile = file,
      files = dir(temp_directory),
      root = temp_directory
    )

    unlink(temp_directory, recursive = TRUE)

  },
  contentType = "application/zip"

)
