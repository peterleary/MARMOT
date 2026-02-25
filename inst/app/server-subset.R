# server-subset.R
# Cell subsetting UI and logic

# Subsetting UI and logic ----
observeEvent({
  input$fpSubsetCells
}, ignoreNULL = FALSE, ignoreInit = TRUE, {
  req(inputDataReactive$Results[["sce"]])
  sce <- inputDataReactive$Results$sce
  n_cells <- ncol(sce)

  if (input$fpSubsetCells) {
    output$fpSubsetCellsByColumnUI1 <- renderUI({
      selectInput(
        inputId = "fpColumnToSubset",
        label = "Subset cells proportionally by",
        choices = get_plottable_columns(sce),
        selected = "condition"
      )
    })
    output$fpSubsetCellsByColumnUI2 <- renderUI({
      numericInput(
        inputId = "fpSubsetToGlobal",
        label = "Subset proportionally to",
        value = isolate({
          if (!is.null(input$fpSubsetToGlobal)) input$fpSubsetToGlobal else n_cells
        }),
        min = 1,
        max = n_cells,
        step = 1
      )
    })
  } else {
    output$fpSubsetCellsByColumnUI1 <- NULL
    output$fpSubsetCellsByColumnUI2 <- NULL
    output$fpSubsetCellsTableUI <- NULL
    cellsToKeepReactive$sc2 <- NULL
  }
})

# Reactive values for storing subset information
cellsToKeepReactive <- reactiveValues(sc2 = NULL)

# Observer for column and global subset changes
observeEvent({
  input$fpColumnToSubset
  input$fpSubsetToGlobal
}, ignoreNULL = FALSE, ignoreInit = TRUE, {
  req(ncol(inputDataReactive$Results$sce) > 10)
  if (isTRUE(input$fpSubsetCells)) {
    req(input$fpColumnToSubset, input$fpSubsetToGlobal)
    cd <- as.data.frame(SummarizedExperiment::colData(inputDataReactive$Results$sce))
    sc2 <- calculate_proportional_subset(cd, input$fpColumnToSubset, input$fpSubsetToGlobal)
    cell_counts <- table(cd[[input$fpColumnToSubset]])

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
    cellsToKeepReactive$sc2 <- sc2
  } else {
    output$fpSubsetCellsTableUI <- NULL
    cellsToKeepReactive$sc2 <- NULL
  }
})

# Observer for individual group input changes
observeEvent({
  if (isTRUE(input$fpSubsetCells) && !is.null(input$fpColumnToSubset)) {
    cd <- as.data.frame(SummarizedExperiment::colData(inputDataReactive$Results$sce))
    group_levels <- names(table(cd[[input$fpColumnToSubset]]))
    lapply(group_levels, function(x) {
      input[[paste0("fpSubset", x, "ToThis")]]
    })
  }
}, ignoreNULL = TRUE, ignoreInit = TRUE, {
  req(ncol(inputDataReactive$Results$sce) > 10)
  if (isTRUE(input$fpSubsetCells) && !is.null(input$fpColumnToSubset)) {
    cd <- as.data.frame(SummarizedExperiment::colData(inputDataReactive$Results$sce))
    group_levels <- names(table(cd[[input$fpColumnToSubset]]))
    for (x in group_levels) {
      input_id <- paste0("fpSubset", x, "ToThis")
      if (!is.null(input[[input_id]]) && !is.null(cellsToKeepReactive$sc2)) {
        cellsToKeepReactive$sc2[[x]] <- as.numeric(input[[input_id]])
      }
    }
  }
})

# Observer for final subsetting — stores subset cell IDs
observeEvent(cellsToKeepReactive$sc2, ignoreNULL = TRUE, ignoreInit = TRUE, {
  req(ncol(inputDataReactive$Results$sce) > 10)
  req(input$fpColumnToSubset, cellsToKeepReactive$sc2)

  cd <- as.data.frame(SummarizedExperiment::colData(inputDataReactive$Results$sce))
  cellsToKeep <- sample_cells_by_group(cd, input$fpColumnToSubset, cellsToKeepReactive$sc2)

  inputDataReactive$Results[["subsetCellIds"]] <- cellsToKeep
})
