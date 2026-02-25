# server-relabel.R
# Cluster relabeling table and observers

# Clusters table ----
clusterTableReactive <- reactiveValues(table = NULL)
clusterTableReactive$table <- data.frame(
  "cluster_id" = levels(inputDataReactive$Results[["sce"]]@colData$cluster_id),
  "relabelled_clusters" = levels(inputDataReactive$Results[["sce"]]@colData$cluster_id),
  "colours" = inputDataReactive$Results$coloursList$cluster_id[match(
    levels(inputDataReactive$Results[["sce"]]@colData$cluster_id),
    names(inputDataReactive$Results$coloursList$cluster_id)
  )]
)
rownames(clusterTableReactive$table) <- NULL
clusterTableReactive$table <- tibble::column_to_rownames(clusterTableReactive$table, "cluster_id")

# Download button for cluster labels
output$saveClusterLabels <- downloadHandler(
  filename = function() "clusterInfos.xlsx",
  content = function(file) {
    openxlsx::write.xlsx(
      data.frame(clusterTableReactive$table) |>
        tibble::rownames_to_column("original"),
      file = file
    )
  }
)

# Show the cluster table
output$clusterLabelTable <- DT::renderDataTable({
  DT::datatable(
    data = clusterTableReactive$table,
    class = "display",
    selection = "none",
    editable = TRUE,
    options = list(dom = "ft", pageLength = 10000)
  ) |>
    DT::formatStyle(
      columns = "colours",
      backgroundColor = DT::styleEqual(
        clusterTableReactive$table$colours,
        clusterTableReactive$table$colours
      )
    )
})

inputDataReactive$Results$coloursList[["relabelled_clusters"]] <- inputDataReactive$Results$coloursList$cluster_id

# Helper: apply relabelling to all relevant data objects
apply_relabelling <- function() {
  result <- apply_relabelling_pure(
    sce = inputDataReactive$Results[["sce"]],
    umapDFList = inputDataReactive$Results$umapDFList,
    coloursList = inputDataReactive$Results$coloursList,
    cluster_table = clusterTableReactive$table
  )
  inputDataReactive$Results[["sce"]] <- result$sce
  inputDataReactive$Results$umapDFList <- result$umapDFList
  inputDataReactive$Results$coloursList <- result$coloursList

  # Update plottable columns
  colsThatCanBePlot <- get_plottable_columns(inputDataReactive$Results$sce)
  updateSelectInput(
    session = session, inputId = "umapColumnToPlot",
    choices = colsThatCanBePlot, selected = "relabelled_clusters"
  )
  updateSelectInput(
    session = session, inputId = "fpColumnToPlot",
    choices = c("None", colsThatCanBePlot),
    selected = "relabelled_clusters"
  )
  updateSelectInput(
    session = session, inputId = "umapColumnToSplit",
    choices = c("None", colsThatCanBePlot),
    selected = input$umapColumnToSplit
  )
  updateSelectInput(
    session = session, inputId = "fpColumnToSplit",
    choices = c("None", colsThatCanBePlot),
    selected = input$fpColumnToSplit
  )
}

# If user edits cluster label table, add new column to cell metadata
observeEvent({
  input$clusterLabelTable_cell_edit
}, ignoreNULL = FALSE, ignoreInit = TRUE, {
  clusterTableReactive$table <<- editData(
    clusterTableReactive$table, input$clusterLabelTable_cell_edit
  )
  apply_relabelling()
})

# Import cluster labels from file
observeEvent(input$importFile, {
  importedDf <- openxlsx::read.xlsx(
    input$importFile[1, "datapath"], colNames = TRUE
  )
  importedDf <- importedDf |>
    data.frame(check.names = FALSE) |>
    tibble::column_to_rownames("original")
  sce_clusters <- inputDataReactive$Results[["sce"]]@colData$cluster_id
  if (any(!sce_clusters %in% rownames(importedDf))) {
    shinyalert::shinyalert(
      title = "The marmots say no",
      text = paste(
        "You uploaded a file that has different original cluster IDs",
        "or different numbers of original clusters.",
        "Are you sure it's from this study?"
      ),
      closeOnEsc = TRUE, closeOnClickOutside = TRUE,
      showCancelButton = TRUE,
      imageUrl = "./Resetti_CF.webp.png"
    )
  } else {
    importedDf$relabelled_clusters <- factor(
      importedDf$relabelled_clusters,
      levels = unique(gtools::mixedsort(
        as.character(importedDf$relabelled_clusters)
      ))
    )
    clusterTableReactive$table <- importedDf
    colourPaletteList$relabelled_clusters <- clusterTableReactive$table$colours
    apply_relabelling()
  }
})
