# server-subset.R
# Cell subsetting UI and logic — supports None, Absolute, and Proportional modes

# Reactive values for storing subset information
cellsToKeepReactive <- reactiveValues(sc2 = NULL)

# Observer: show/hide column + global controls based on subset mode ----
observeEvent({
  input$fpSubsetMode
}, ignoreNULL = FALSE, ignoreInit = TRUE, {
  req(inputDataReactive$Results[["sce"]])
  sce <- inputDataReactive$Results$sce
  n_cells <- ncol(sce)

  if (!is.null(input$fpSubsetMode) && input$fpSubsetMode != "None") {
    output$fpSubsetCellsByColumnUI1 <- renderUI({
      selectInput(
        inputId = "fpColumnToSubset",
        label = "Subset cells by",
        choices = get_plottable_columns(sce),
        selected = "condition"
      )
    })
    if (input$fpSubsetMode == "Proportional") {
      output$fpSubsetCellsByColumnUI2 <- renderUI({
        numericInput(
          inputId = "fpSubsetToGlobal",
          label = "Total cells (proportional target)",
          value = isolate({
            if (!is.null(input$fpSubsetToGlobal)) input$fpSubsetToGlobal else n_cells
          }),
          min = 1,
          max = n_cells,
          step = 1
        )
      })
    } else {
      # Absolute mode: no global target needed (sliders shown per group)
      output$fpSubsetCellsByColumnUI2 <- renderUI(NULL)
    }
  } else {
    output$fpSubsetCellsByColumnUI1 <- renderUI(NULL)
    output$fpSubsetCellsByColumnUI2 <- renderUI(NULL)
    output$fpSubsetCellsTableUI <- renderUI(NULL)
    cellsToKeepReactive$sc2 <- NULL
    inputDataReactive$Results[["subsetCellIds"]] <- NULL
  }
})

# Observer for column / global subset changes — builds per-group sliders ----
observeEvent({
  input$fpColumnToSubset
  input$fpSubsetToGlobal
  input$fpSubsetMode
}, ignoreNULL = FALSE, ignoreInit = TRUE, {
  req(ncol(inputDataReactive$Results$sce) > 10)
  mode <- input$fpSubsetMode %||% "None"
  if (mode == "None") return(invisible(NULL))
  req(input$fpColumnToSubset)

  cd <- as.data.frame(SummarizedExperiment::colData(inputDataReactive$Results$sce))
  cell_counts <- table(cd[[input$fpColumnToSubset]])
  group_levels <- names(cell_counts)

  if (mode == "Proportional") {
    req(input$fpSubsetToGlobal)
    sc2 <- calculate_proportional_subset(cd, input$fpColumnToSubset, input$fpSubsetToGlobal)
    output$fpSubsetCellsTableUI <- renderUI({
      lapply(group_levels, function(x) {
        numericInput(
          inputId = paste0("fpSubset", x, "ToThis"),
          label = paste("Cells for", x, "(max", as.numeric(cell_counts[[x]]), ")"),
          value = sc2[[x]],
          min = 0L,
          max = as.numeric(cell_counts[[x]]),
          step = 1L
        )
      })
    })
    cellsToKeepReactive$sc2 <- sc2

  } else if (mode == "Absolute") {
    # Default: keep all cells per group
    sc2 <- setNames(as.numeric(cell_counts), group_levels)
    output$fpSubsetCellsTableUI <- renderUI({
      lapply(group_levels, function(x) {
        numericInput(
          inputId = paste0("fpSubset", x, "ToThis"),
          label = paste("Cells for", x, "(max", as.numeric(cell_counts[[x]]), ")"),
          value = sc2[[x]],
          min = 0L,
          max = as.numeric(cell_counts[[x]]),
          step = 1L
        )
      })
    })
    cellsToKeepReactive$sc2 <- sc2
  }
})

# Observer for individual group input changes ----
observeEvent({
  if (!is.null(input$fpSubsetMode) && input$fpSubsetMode != "None" &&
        !is.null(input$fpColumnToSubset)) {
    cd <- as.data.frame(SummarizedExperiment::colData(inputDataReactive$Results$sce))
    group_levels <- names(table(cd[[input$fpColumnToSubset]]))
    lapply(group_levels, function(x) {
      input[[paste0("fpSubset", x, "ToThis")]]
    })
  }
}, ignoreNULL = TRUE, ignoreInit = TRUE, {
  req(ncol(inputDataReactive$Results$sce) > 10)
  mode <- input$fpSubsetMode %||% "None"
  if (mode == "None") return(invisible(NULL))
  req(input$fpColumnToSubset)

  cd <- as.data.frame(SummarizedExperiment::colData(inputDataReactive$Results$sce))
  group_levels <- names(table(cd[[input$fpColumnToSubset]]))
  for (x in group_levels) {
    input_id <- paste0("fpSubset", x, "ToThis")
    if (!is.null(input[[input_id]]) && !is.null(cellsToKeepReactive$sc2)) {
      cellsToKeepReactive$sc2[[x]] <- as.numeric(input[[input_id]])
    }
  }
})

# Observer for final subsetting — stores subset cell IDs ----
observeEvent(cellsToKeepReactive$sc2, ignoreNULL = TRUE, ignoreInit = TRUE, {
  req(ncol(inputDataReactive$Results$sce) > 10)
  req(input$fpColumnToSubset, cellsToKeepReactive$sc2)

  cd <- as.data.frame(SummarizedExperiment::colData(inputDataReactive$Results$sce))
  cellsToKeep <- sample_cells_by_group(cd, input$fpColumnToSubset, cellsToKeepReactive$sc2)

  inputDataReactive$Results[["subsetCellIds"]] <- cellsToKeep
})
