# ── server-subset.R ──────────────────────────────────────────────────────────
# Cell subsetting module for Shiny MARMOT v2.
# Implements 3-mode subsetting (None / Absolute / Proportional) using
# sample_cells_by_group() and calculate_proportional_subset() from
# data_helpers.R.
#
# Data model:
#   inputDataReactive$Results$sce           — full SCE (never mutated here)
#   inputDataReactive$Results$subsetCellIds — NULL or character vector of IDs
#   cellsToKeepReactive$sc2                 — named numeric: cells per group
# ─────────────────────────────────────────────────────────────────────────────

cellsToKeepReactive <- reactiveValues(sc2 = NULL)

# ── 1. Mode observer: show/hide controls based on fpSubsetMode ──────────────
observeEvent(input$fpSubsetMode, ignoreNULL = FALSE, ignoreInit = TRUE, {
  req(inputDataReactive$Results[["sce"]])
  sce     <- inputDataReactive$Results$sce
  n_cells <- ncol(sce)
  mode    <- input$fpSubsetMode %||% "None"

  if (mode != "None") {
    # Column selector (shared by both modes)
    output$fpSubsetCellsByColumnUI1 <- renderUI({
      selectInput(
        inputId  = "fpColumnToSubset",
        label    = "Subset cells by",
        choices  = get_plottable_columns(sce),
        selected = "cluster_id"
      )
    })

    # Global target input (Proportional only)
    if (mode == "Proportional") {
      output$fpSubsetCellsByColumnUI2 <- renderUI({
        numericInput(
          inputId = "fpSubsetToGlobal",
          label   = "Total cells (proportional target)",
          value   = isolate({
            if (!is.null(input$fpSubsetToGlobal)) input$fpSubsetToGlobal
            else min(50000L, n_cells)
          }),
          min  = 1L,
          max  = n_cells,
          step = 1L
        )
      })
    } else {
      # Absolute: no global target (per-group inputs handle it)
      output$fpSubsetCellsByColumnUI2 <- renderUI(NULL)
    }

    # Summary text placeholder
    output$fpSubsetCellsByColumnUI3 <- renderUI({
      helpText(
        style = "color: #71717a; font-size: 0.85rem;",
        paste0("Dataset: ", format(n_cells, big.mark = ","), " cells. ",
               "Adjust per-group targets below.")
      )
    })

  } else {
    # None: clear everything
    output$fpSubsetCellsByColumnUI1 <- renderUI(NULL)
    output$fpSubsetCellsByColumnUI2 <- renderUI(NULL)
    output$fpSubsetCellsByColumnUI3 <- renderUI(NULL)
    output$fpSubsetCellsTableUI     <- renderUI(NULL)
    cellsToKeepReactive$sc2                    <- NULL
    inputDataReactive$Results[["subsetCellIds"]] <- NULL
  }
})

# ── 2. Build per-group sliders when column / global target / mode changes ───
observeEvent({
  input$fpColumnToSubset
  input$fpSubsetToGlobal
  input$fpSubsetMode
}, ignoreNULL = FALSE, ignoreInit = TRUE, {
  req(inputDataReactive$Results[["sce"]])
  req(ncol(inputDataReactive$Results$sce) > 10)
  mode <- input$fpSubsetMode %||% "None"
  if (mode == "None") return(invisible(NULL))
  req(input$fpColumnToSubset)

  cd          <- as.data.frame(SummarizedExperiment::colData(inputDataReactive$Results$sce))
  cell_counts <- table(cd[[input$fpColumnToSubset]])
  group_levels <- names(cell_counts)

  if (mode == "Proportional") {
    req(input$fpSubsetToGlobal)
    sc2 <- calculate_proportional_subset(cd, input$fpColumnToSubset, input$fpSubsetToGlobal)

    output$fpSubsetCellsTableUI <- renderUI({
      lapply(group_levels, function(x) {
        numericInput(
          inputId = paste0("fpSubset", x, "ToThis"),
          label   = paste0(x, " (max ", format(as.numeric(cell_counts[[x]]), big.mark = ","), ")"),
          value   = sc2[[x]],
          min     = 0L,
          max     = as.numeric(cell_counts[[x]]),
          step    = 1L
        )
      })
    })
    cellsToKeepReactive$sc2 <- sc2

  } else if (mode == "Absolute") {
    sc2 <- setNames(as.numeric(cell_counts), group_levels)

    output$fpSubsetCellsTableUI <- renderUI({
      lapply(group_levels, function(x) {
        numericInput(
          inputId = paste0("fpSubset", x, "ToThis"),
          label   = paste0(x, " (max ", format(as.numeric(cell_counts[[x]]), big.mark = ","), ")"),
          value   = sc2[[x]],
          min     = 0L,
          max     = as.numeric(cell_counts[[x]]),
          step    = 1L
        )
      })
    })
    cellsToKeepReactive$sc2 <- sc2
  }
})

# ── 3. Individual group input changes → update cellsToKeepReactive$sc2 ─────
observeEvent({
  if (!is.null(input$fpSubsetMode) && input$fpSubsetMode != "None" &&
      !is.null(input$fpColumnToSubset)) {
    cd           <- as.data.frame(SummarizedExperiment::colData(inputDataReactive$Results$sce))
    group_levels <- names(table(cd[[input$fpColumnToSubset]]))
    lapply(group_levels, function(x) input[[paste0("fpSubset", x, "ToThis")]])
  }
}, ignoreNULL = TRUE, ignoreInit = TRUE, {
  req(inputDataReactive$Results[["sce"]])
  req(ncol(inputDataReactive$Results$sce) > 10)
  mode <- input$fpSubsetMode %||% "None"
  if (mode == "None") return(invisible(NULL))
  req(input$fpColumnToSubset)

  cd           <- as.data.frame(SummarizedExperiment::colData(inputDataReactive$Results$sce))
  group_levels <- names(table(cd[[input$fpColumnToSubset]]))

  for (x in group_levels) {
    input_id <- paste0("fpSubset", x, "ToThis")
    if (!is.null(input[[input_id]]) && !is.null(cellsToKeepReactive$sc2)) {
      cellsToKeepReactive$sc2[[x]] <- as.numeric(input[[input_id]])
    }
  }
})

# ── 4. Final subsetting: sample cell IDs and store ──────────────────────────
observeEvent(cellsToKeepReactive$sc2, ignoreNULL = TRUE, ignoreInit = TRUE, {
  req(inputDataReactive$Results[["sce"]])
  req(ncol(inputDataReactive$Results$sce) > 10)
  req(input$fpColumnToSubset, cellsToKeepReactive$sc2)

  cd         <- as.data.frame(SummarizedExperiment::colData(inputDataReactive$Results$sce))
  cellsToKeep <- sample_cells_by_group(cd, input$fpColumnToSubset, cellsToKeepReactive$sc2)

  inputDataReactive$Results[["subsetCellIds"]] <- cellsToKeep
})
