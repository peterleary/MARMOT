req(exists("inputDataReactive"))
req(!is.null(inputDataReactive$Results[["sce"]]))

# Fireworks ----
fw <- Fireworks$new()
observe({
  fw$start()
  Sys.sleep(3)
  fw$stop(fadeOut = TRUE)
}) |> bindEvent(input$acceptCite)
observeEvent(input$acceptCite, {
  showNotification(ui = "Thanks for agreeing to cite us! You made the marmots very happy!", duration = 30)
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
      downloadButton("downloadInputsE", "Download settings (Excel)"), downloadButton("downloadInputsR", "Download settings (qs)")
    )
  })
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
output$metadataTable <- DT::renderDataTable(inputDataReactive$Results$md)
labelList <- setNames(lapply(inputDataReactive$Results$conditions, function(x) {
  levels(as.factor(inputDataReactive$Results$md[[x]]))
}), inputDataReactive$Results$conditions)
labelDf <- data.frame(
  "Factor" = unlist(lapply(seq_along(labelList), function(i) {rep(names(labelList)[[i]], lengths(labelList)[[i]])})),
  "Levels" = as.character((unlist(labelList)))
)
# Make a table to be able to edit factor levels for visualisation purposes
output$changeLabelTable <- DT::renderDataTable(DT::datatable(labelDf, class = "display", selection = 'none', editable = TRUE, rownames = F))

# posMarkers table ----
# If the posMarkers xlsx file was loaded, create the UI to display it
output$posMarkerUI <- renderUI({
  if ("topMarkerTable" %in% names(inputDataReactive$Results)) {
    req(!is.null(inputDataReactive$Results[["topMarkerTable"]]))
    DT::dataTableOutput(outputId = "posMarkerTable")
  } else {
    renderText("No Marker Gene table was loaded.")
  }
})
# If the posMarkers xlsx file was loaded, table the table in the table UI
if ("topMarkerTable" %in% names(inputDataReactive$Results)) {
  req(!is.null(inputDataReactive$Results[["topMarkerTable"]]))
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
  "new_clusters" = levels(inputDataReactive$Results[["sce"]]@colData$cluster_id),
  "colour" = inputDataReactive$Results$coloursList$cluster_id[match(levels(inputDataReactive$Results[["sce"]]@colData$cluster_id), names(inputDataReactive$Results$coloursList$cluster_id))]
)
rownames(clusterTableReactive$table) <- NULL
clusterTableReactive$table <- column_to_rownames(clusterTableReactive$table, "cluster_id")
observeEvent(clusterTableReactive$table, {
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
    )
  })
})
# If user edits cluster label table, add new column to cell meta data
observeEvent({
  input$clusterLabelTable_cell_edit
  }, ignoreNULL = FALSE, ignoreInit = TRUE, {
  clusterTableReactive$table <<- editData(clusterTableReactive$table, input$clusterLabelTable_cell_edit)
  
  # Update the Seurat object used for feature plot 
  inputDataReactive$Results$scData$new_clusters <- clusterTableReactive$table$new_clusters[match(inputDataReactive$Results$scData$cluster_id, rownames(clusterTableReactive$table))]
  inputDataReactive$Results$scData$new_clusters <- factor(
    x = inputDataReactive$Results$scData$new_clusters, 
    levels = mixedsort(unique(inputDataReactive$Results$scData$new_clusters))
  )
  
  # Update the main sce object 
  inputDataReactive$Results[["sce"]]@colData$new_clusters <- clusterTableReactive$table$new_clusters[match(inputDataReactive$Results[["sce"]]@colData$cluster_id, rownames(clusterTableReactive$table))]
  inputDataReactive$Results[["sce"]]@colData$new_clusters <- factor(
    x = inputDataReactive$Results[["sce"]]@colData$new_clusters, 
    levels = mixedsort(unique(inputDataReactive$Results[["sce"]]@colData$new_clusters))
    )
  
  # Update the DR data frames 
  for (tab in c("All", "Downsampled")) {
    if (tab %in% names(inputDataReactive$Results$umapDFList)) {
      inputDataReactive$Results$umapDFList[[tab]]$new_clusters <- clusterTableReactive$table$new_clusters[match(inputDataReactive$Results$umapDFList[[tab]]$cluster_id, rownames(clusterTableReactive$table))]
      inputDataReactive$Results$umapDFList[[tab]]$new_clusters <- factor(
        x = inputDataReactive$Results$umapDFList[[tab]]$new_clusters, 
        levels = mixedsort(unique(inputDataReactive$Results$umapDFList[[tab]]$new_clusters))
      )
    }
  }
  
  # Put the new colours in the colour list 
  inputDataReactive$Results$coloursList[["new_clusters"]] <- clusterTableReactive$table$colour
  names(inputDataReactive$Results$coloursList[["new_clusters"]]) <- clusterTableReactive$table$new_clusters
  allCols <- colnames(colData(inputDataReactive$Results$sce))
  colsThatCanBePlot <- unlist(lapply(seq_along(allCols), function(i) {
    if (length(unique(inputDataReactive$Results$sce[[allCols[i]]])) < 100) {
      allCols[i]
    }
  }))
  
  # Update the select inputs accordingly 
  updateSelectInput(session = session, inputId = "umapColumnToPlot", choices = colsThatCanBePlot, selected = "new_clusters")
  updateSelectInput(session = session, inputId = "fpColumnToPlot", choices = c("None", colsThatCanBePlot), selected = "new_clusters")
  updateSelectInput(session = session, inputId = "umapColumnToSplit", choices = c("None", colsThatCanBePlot), selected = input$umapColumnToSplit)
  updateSelectInput(session = session, inputId = "fpColumnToSplit", choices = c("None", colsThatCanBePlot), selected = input$fpColumnToSplit)
})
# Or if the user uploads a whole excel file 
observeEvent({
  clusterTableReactive$table
}, ignoreNULL = FALSE, ignoreInit = TRUE, {
  
  # Update the Seurat object used for feature plot 
  inputDataReactive$Results$scData$new_clusters <- clusterTableReactive$table$new_clusters[match(inputDataReactive$Results$scData$cluster_id, rownames(clusterTableReactive$table))]
  inputDataReactive$Results$scData$new_clusters <- factor(
    x = inputDataReactive$Results$scData$new_clusters, 
    levels = mixedsort(unique(inputDataReactive$Results$scData$new_clusters))
  )
  
  # Update the main sce object 
  inputDataReactive$Results[["sce"]]@colData$new_clusters <- clusterTableReactive$table$new_clusters[match(inputDataReactive$Results[["sce"]]@colData$cluster_id, rownames(clusterTableReactive$table))]
  inputDataReactive$Results[["sce"]]@colData$new_clusters <- factor(
    x = inputDataReactive$Results[["sce"]]@colData$new_clusters, 
    levels = mixedsort(unique(inputDataReactive$Results[["sce"]]@colData$new_clusters))
  )
  
  # Update the DR data frames 
  for (tab in c("All", "Downsampled")) {
    if (tab %in% names(inputDataReactive$Results$umapDFList)) {
      inputDataReactive$Results$umapDFList[[tab]]$new_clusters <- clusterTableReactive$table$new_clusters[match(inputDataReactive$Results$umapDFList[[tab]]$cluster_id, rownames(clusterTableReactive$table))]
      inputDataReactive$Results$umapDFList[[tab]]$new_clusters <- factor(
        x = inputDataReactive$Results$umapDFList[[tab]]$new_clusters, 
        levels = mixedsort(unique(inputDataReactive$Results$umapDFList[[tab]]$new_clusters))
      )
    }
  }
  
  # Put the new colours in the colour list 
  inputDataReactive$Results$coloursList[["new_clusters"]] <- clusterTableReactive$table$colour
  names(inputDataReactive$Results$coloursList[["new_clusters"]]) <- clusterTableReactive$table$new_clusters
  allCols <- colnames(colData(inputDataReactive$Results$sce))
  colsThatCanBePlot <- unlist(lapply(seq_along(allCols), function(i) {
    if (length(unique(inputDataReactive$Results$sce[[allCols[i]]])) < 100) {
      allCols[i]
    }
  }))
  
  # Update the select inputs accordingly 
  updateSelectInput(session = session, inputId = "umapColumnToPlot", choices = colsThatCanBePlot, selected = "new_clusters")
  updateSelectInput(session = session, inputId = "fpColumnToPlot", choices = c("None", colsThatCanBePlot), selected = "new_clusters")
  updateSelectInput(session = session, inputId = "umapColumnToSplit", choices = c("None", colsThatCanBePlot), selected = input$umapColumnToSplit)
  updateSelectInput(session = session, inputId = "fpColumnToSplit", choices = c("None", colsThatCanBePlot), selected = input$fpColumnToSplit)
})
# Download button for clusterLabels
output$saveClusterLabels <- downloadHandler(
  filename = function() { "clusterInfos.xlsx" },
  content = function(file) {
    openxlsx::write.xlsx((data.frame(clusterTableReactive$table, check.names = F) %>% rownames_to_column("original")), file = file)
  }
)
importedClusters <- reactiveValues(table = NULL)
observeEvent(input$importFile, {
  importedClusters$table <- openxlsx::read.xlsx(input$importFile[1, 'datapath'], colNames = T)
  importedClusters$table <- importedClusters$table %>% data.frame(check.names = F) %>% column_to_rownames("original")
  if (ncol(importedClusters$table) != 2) {
    shinyalert::shinyalert(title = "Marmot says no", text = "The file you tried to upload doesn't have the correct number of columns! Please make sure it has only three.", closeOnEsc = TRUE, closeOnClickOutside = TRUE, showCancelButton = TRUE)
  } else {
    colnames(importedClusters$table) <- c("new_clusters", "colour")
  }
  if(any(!inputDataReactive$Results[["sce"]]@colData$cluster_id %in% rownames(importedClusters$table))) {
    shinyalert::shinyalert(title = "Marmot says no", text = "You uploaded a file that has different original cluster IDs or different numbers of original clusters. Are you sure it's from this study?", closeOnEsc = TRUE, closeOnClickOutside = TRUE, showCancelButton = TRUE)
  } else {
    req(!is.null(clusterTableReactive$table))
    req(!is.null(importedClusters$table))
    clusterTableReactive$table <- importedClusters$table
  }
})

# Gene bucket ----
# Create a reactive list of genes from either the input select or marker table
genesReactive <- reactiveValues(genes = NULL)
observeEvent(
  {
    input$fpFeatureToPlot
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE,
  {
    genesList1 <- input$fpFeatureToPlot
    genesUnlist <- unique(unlist(c(genesList1)))
    genesUnlist <- genesUnlist[!genesUnlist == ""]
    genesReactive$genes <- genesUnlist
  }
)
# Empty the gene bucket if selected
observeEvent(input$resetGeneBucketFP, {
  genesReactive$genes <- NULL
  updateSelectizeInput(session = session, inputId = "fpFeatureToPlot", choices = names(inputDataReactive$Results$sce), selected = NULL, server = T)
  featurePlotReactive$fp <- NULL
}, ignoreNULL = T, ignoreInit = T)
# Create a bucket of genes selected by the user that will be feature plotted
observeEvent(
  {
    genesReactive$genes
  },
  ignoreNULL = FALSE,
  {
    output$geneBucket1 <- renderUI({
      bucket_list(
        header = "Drag and drop features in order to be plotted",
        group_name = "bucket_list_group",
        orientation = "horizontal",
        add_rank_list(
          text = "Include these features in this order",
          labels = genesReactive$genes,
          input_id = "keepBucketFP"
        ),
        add_rank_list(
          text = "Drag features into this bucket to remove them",
          labels = NULL,
          input_id = "excludeBucketFP"
        )
      )
    })
  }
)

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
    paste0("exploreFC_Data_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".qs")
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
    input$umapAssayToPlot
    input$umapColumnToPlot
    input$textSizeUMAP
    input$pointSizeUMAP
    input$umapShowContours
    input$umapShowLabels
    input$umapShowAxes
    input$umapLegendPosition
    input$umapColumnToSplit
    input$cellBordersFP
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
      req(nchar(input$umapColumnToPlot) >= 2)

      if (input$umapColumnToSplit == "None") {
        umapColumnToSplit <- NULL
      } else if (is.null(input$umapColumnToSplit)) {
        umapColumnToSplit <- NULL
      } else {
        umapColumnToSplit <- input$umapColumnToSplit
      }

      umapDF <- inputDataReactive$Results$umapDFList$Downsampled

      if (input$umapColumnToPlot == "new_clusters") {
        req("new_clusters" %in% colnames(inputDataReactive$Results[["sce"]]@colData))
        req(length(inputDataReactive$Results$coloursList[["new_clusters"]]) >= 2)
        # umapDF$new_clusters <- inputDataReactive$Results[["sce"]]@colData$new_clusters[match(inputDataReactive$Results[["sce"]]@colData$cluster_id, umapDF$cluster_id)]
      }

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
    req(nchar(input$featurePlotType) > 2)
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
      lapply(c(1:6, 10, 11), function(i) {
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
  if (input$featurePlotType %in% c("Heatmap", "Individual Heatmap", "Dot Plot")) {
    req(!is.null(input$fpHeatmapPlotAll))
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
fp <- NULL
featurePlotReactive <- reactiveValues(fp = NULL)
observeEvent(input$featurePlotType, {
  featurePlotReactive <- reactiveValues(fp = NULL)
})
observeEvent(
  {
    input$featurePlotType
    input$fpAssayToPlot
    input$keepBucketFP
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
    input$fpClonalSizeBarplotProptional
    input$fpClonalOverlapCall
    input$fpClonalOverlapChain
    input$fpClonalOverlapMethod
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
    lapply(names(colsList1), function(col) {
      lapply(names(colsList1[[col]]), function(lor) {
        input[[paste0("GroupColour", col, lor)]]
      })
    })
  },
  ignoreNULL = FALSE,
  {
    tryCatch({
      req(length(input$plotByKeepBucket) > 1)
      req(nchar(input$featurePlotType) >= 2)
      scDataToFP <- inputDataReactive$Results$scData

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

      fpFeaturesToPlot <- input$keepBucketFP %>% gsub("_", "-", .)

      # Make the plots!
      if (input$featurePlotType == "Feature Plot") {
        fp <- lapply(fpFeaturesToPlot, function(gene) {
          gtp <- gene %>% gsub("-", "_", .)
          gene <- gene %>% gsub("_", "-", .)
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
            reduction = inputDataReactive$Results$dimRedMethodToUse,
            sample = scDataToFP,
            slot = input$fpAssayToPlot,
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
              fp2 <- fp2 + eval(parse(text = paste0("geom_point(aes(colour = `", gene, "`), size = ", input$pointSizeFP, ")")))
              if (input$viridisColourFP %in% viridisColours) {
                fp2 <- fp2 + scale_colour_viridis_c(option = input$viridisColourFP, direction = viridisFlip)
              } else if (input$viridisColourFP %in% scicoColours) {
                fp2 <- fp2 + scale_colour_scico(palette = input$viridisColourFP, direction = viridisFlip)
              } else if (input$viridisColourFP %in% divergingColours) {
                fp2 <- fp2 + scale_colour_distiller(palette = input$viridisColourFP, direction = viridisFlip, type = "div")
              }
            } else {
              fp2 <- fp2 + eval(parse(text = paste0("geom_point(aes(fill = `", gene, "`), size = ", input$pointSizeFP, ", pch = 21, stroke = ", input$borderSizeFP/10, ")")))
              if (input$viridisColourFP %in% viridisColours) {
                fp2 <- fp2 + scale_fill_viridis_c(option = input$viridisColourFP, direction = viridisFlip)
              } else if (input$viridisColourFP %in% scicoColours) {
                fp2 <- fp2 + scale_fill_scico(palette = input$viridisColourFP, direction = viridisFlip)
              } else if (input$viridisColourFP %in% divergingColours) {
                fp2 <- fp2 + scale_fill_distiller(palette = input$viridisColourFP, direction = viridisFlip, type = "div")
              }
            }
          } else {
            req(!is.null(input$rasterFP_DPI))
            fp2 <- fp2 + eval(parse(text = paste0("geom_scattermore(pointsize = ", (input$pointSizeFP*2)+0.6, ", pixels = c(", input$rasterFP_DPI, ",", input$rasterFP_DPI,"), aes(colour = `", gene, "`))")))
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
        req(any(nchar(fpFeaturesToPlot) >= 2))
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
          slot = input$fpLayerToPlot,
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
            req(!is.null(input$rasterFP_DPI))
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
      # } else if (input$featurePlotType == "Boxplot") {
      #   assay <-  Seurat::GetAssayData(object = scDataToFP, assay = "originalexp", layer = input$fpAssayToPlot) %>% 
      #     t %>%
      #     data.frame(check.names = F) %>%
      #     rownames_to_column("cell-id") %>% 
      #     left_join((scDataToFP@meta.data %>% data.frame(check.names = F) %>% rownames_to_column("cell-id")), by = "cell-id")
      #   fp <- lapply(fpFeaturesToPlot, function(gene) {
      #     gtp <- gene %>% gsub("-", "_", .)
      #     gene <- gene %>% gsub("_", "-", .)
      #     ggplot(assay, aes(x = .data[[fpColumnToPlot]], y = .data[[gene]])) +
      #       # geom_boxplot(outlier.shape = NA, aes(fill = .data[[fpColumnToPlot]])) +
      #       geom_violin(outlier.shape = NA, aes(fill = .data[[fpColumnToPlot]])) +
      #       scale_fill_manual(values = inputDataReactive$Results$coloursList[[fpColumnToPlot]]) +
      #       theme_prism()
      #   })
      } else if (input$featurePlotType == "Violin Plot") {
        req(any(nchar(fpFeaturesToPlot) >= 2))
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
        req(any(nchar(fpFeaturesToPlot) >= 2))
        req(!is.null(input$umapFeaturePlotDotplotFlip))
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
        req(any(nchar(fpFeaturesToPlot) >= 2))
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
        req(any(nchar(fpFeaturesToPlot) >= 2))
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
    if (input$featurePlotType %in% c("Feature Plot", "Nebulosa Plot", "Ridge Plot") && length(input$keepBucketFP) >= 2) {
      gridExtra::grid.arrange(grobs = featurePlotReactive$fp, ncol = input$ncolFPGene)
    } else {
      featurePlotReactive$fp
    }
  },
  height = function(){input$figHeightFP},
  width = function(){input$figWidthFP}
)
if (input$featurePlotType == "Ridge Plot" && length(input$keepBucketFP) >= 2 || input$featurePlotType %in% c("Feature Plot", "Nebulosa Plot")) {
  output$dlFP <- downloadHandler(
    filename = function() {
      paste((input$featurePlotType %>% gsub(" ", "", .)), paste(input$keepBucketFP, collapse = "_"), tolower(input$dlFormat), sep = ".")
    },
    content = function(file) {
      if (input$dlFormat == "PDF") {
        pdf(file = file, width = as.numeric(input$figWidthFP / 60), height = as.numeric(input$figHeightFP / 60))
      } else if (input$dlFormat == "SVG") {
        svg(file = file, width = as.numeric(input$figWidthFP / 60), height = as.numeric(input$figHeightFP / 60))
      } else if (input$dlFormat == "PNG") {
        png(filename = file, width = as.numeric(input$figWidthFP / 60), height = as.numeric(input$figHeightFP / 60), units = "in", res = as.numeric(input$pngRes))
      }
      gridExtra::grid.arrange(grobs = featurePlotReactive$fp, ncol = input$ncolFPGene)
      dev.off()
    }
  )
} else {
  output$dlFP <- downloadHandler(
    filename = function() {
      paste((input$featurePlotType %>% gsub(" ", "", .)), input$keepBucketFP, tolower(input$dlFormat), sep = ".")
    },
    content = function(file) {
      if (input$dlFormat == "PDF") {
        pdf(file = file, width = as.numeric(input$figWidthFP / 60), height = as.numeric(input$figHeightFP / 60))
      } else if (input$dlFormat == "SVG") {
        svg(file = file, width = as.numeric(input$figWidthFP / 60), height = as.numeric(input$figHeightFP / 60))
      } else if (input$dlFormat == "PNG") {
        png(filename = file, width = as.numeric(input$figWidthFP / 60), height = as.numeric(input$figHeightFP / 60), units = "in", res = as.numeric(input$pngRes))
      }
      print(featurePlotReactive$fp)
      dev.off()
    }
  )
}
