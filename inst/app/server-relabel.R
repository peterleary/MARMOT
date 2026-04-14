# ── server-relabel.R ──────────────────────────────────────────────────────────
# Cluster relabelling module for MARMOT Shiny app.
# Adapted from exploreSingleCell's relabelling system; works with SCE +
# MARMOT's apply_relabelling_pure() helper (data_helpers.R).
# ─────────────────────────────────────────────────────────────────────────────

# ── Local helpers ────────────────────────────────────────────────────────────

initialise_relabel_table <- function(sce, column_name, coloursList, default_palette) {
  unique_vals <- gtools::mixedsort(
    unique(as.character(SummarizedExperiment::colData(sce)[[column_name]]))
  )
  n_vals <- length(unique_vals)
  # Pull existing colours from coloursList (matches eSC pattern); fall back to palette
  if (!is.null(coloursList[[column_name]])) {
    existing <- coloursList[[column_name]]
    colours  <- existing[match(unique_vals, names(existing))]
    missing  <- is.na(colours)
    if (any(missing)) colours[missing] <- rep_len(default_palette, sum(missing))
  } else {
    colours <- rep_len(default_palette, n_vals)
  }
  df <- data.frame(
    relabelled_clusters = unique_vals,
    colours             = as.character(colours[seq_len(n_vals)]),
    stringsAsFactors    = FALSE
  )
  rownames(df) <- unique_vals
  df
}

extract_colour_mapping <- function(relabel_table) {
  colour_vec <- relabel_table$colours
  names(colour_vec) <- relabel_table$relabelled_clusters
  colour_vec[!duplicated(names(colour_vec))]
}

# ── Reactive state ───────────────────────────────────────────────────────────

clusterTableReactive <- reactiveValues(table = NULL)
relabelStorageReactive <- reactiveValues(
  tables       = list(),
  colours      = list(),
  activeColumn = NULL
)

# ── Initialise from loaded data ──────────────────────────────────────────────

observeEvent(inputDataReactive$Results, once = TRUE, {
  req(inputDataReactive$Results)
  sce <- inputDataReactive$Results$sce

  # Find a valid default column — prefer cluster_id, fall back to first colData column
  cd_names <- colnames(SummarizedExperiment::colData(sce))
  default_column <- if ("cluster_id" %in% cd_names) {
    "cluster_id"
  } else {
    cd_names[1]
  }
  req(default_column)

  tryCatch({
    coloursList <- inputDataReactive$Results$coloursList
    tbl <- initialise_relabel_table(sce, default_column, coloursList, catalystCols)

    clusterTableReactive$table                        <- tbl
    relabelStorageReactive$activeColumn                <- default_column
    relabelStorageReactive$tables[[default_column]]    <- tbl
    relabelStorageReactive$colours[[default_column]]   <- extract_colour_mapping(tbl)

    # Seed relabelled_clusters colour entry for downstream plots (matches eSC)
    coloursList[["relabelled_clusters"]] <- coloursList[[default_column]]
    inputDataReactive$Results$coloursList <- coloursList
  }, error = function(e) {
    showNotification(
      paste("Relabel init error:", e$message),
      type = "warning", duration = 8
    )
  })
})

# ── Relabel column indicator ─────────────────────────────────────────────────

output$relabelColumnIndicator <- renderUI({
  col <- relabelStorageReactive$activeColumn
  req(col)
  tags$p(
    style = "font-weight: 600; margin-top: 6px;",
    "Relabelling column: ",
    tags$span(style = "color: #dc2626;", col)
  )
})

# ── Download handler ─────────────────────────────────────────────────────────

output$saveClusterLabels <- downloadHandler(
  filename = function() {
    paste0("clusterLabels_", relabelStorageReactive$activeColumn, ".xlsx")
  },
  content = function(file) {
    openxlsx::write.xlsx(
      data.frame(clusterTableReactive$table) |>
        tibble::rownames_to_column("original"),
      file = file
    )
  }
)

# ── Editable DataTable ───────────────────────────────────────────────────────

output$clusterLabelTable <- DT::renderDataTable({
  req(clusterTableReactive$table)
  DT::datatable(
    data      = clusterTableReactive$table,
    class     = "display compact",
    selection = "none",
    editable  = TRUE,
    options   = list(dom = "ft", pageLength = 10000, scrollY = "400px")
  ) |>
    DT::formatStyle(
      columns         = "colours",
      backgroundColor = DT::styleEqual(
        clusterTableReactive$table$colours,
        clusterTableReactive$table$colours
      )
    )
})

# ── Cell edit observer ───────────────────────────────────────────────────────

observeEvent(input$clusterLabelTable_cell_edit, ignoreNULL = TRUE, ignoreInit = TRUE, {
  clusterTableReactive$table <- DT::editData(
    clusterTableReactive$table, input$clusterLabelTable_cell_edit
  )
  # Persist edit into storage for this column
  col <- relabelStorageReactive$activeColumn
  if (!is.null(col)) {
    relabelStorageReactive$tables[[col]]  <- clusterTableReactive$table
    relabelStorageReactive$colours[[col]] <- extract_colour_mapping(clusterTableReactive$table)
  }
})

# ── Apply Relabelling ────────────────────────────────────────────────────────

observeEvent(input$applyRelabelling, {
  req(clusterTableReactive$table, inputDataReactive$Results)

  active_col <- relabelStorageReactive$activeColumn %||% "cluster_id"

  result <- apply_relabelling_pure(
    sce           = inputDataReactive$Results$sce,
    umapDFList    = inputDataReactive$Results$umapDFList,
    coloursList   = inputDataReactive$Results$coloursList,
    cluster_table = clusterTableReactive$table,
    source_column = active_col
  )
  # Atomic update: single invalidation instead of three separate sub-field writes
  res <- inputDataReactive$Results
  res$sce         <- result$sce
  res$umapDFList  <- result$umapDFList
  res$coloursList <- result$coloursList
  inputDataReactive$Results <- res

  # Bump data version to force DR plot redraw even if UI inputs haven't changed
  drDataVersion(isolate(drDataVersion()) + 1L)

  # Register relabelled palette so it appears in palette selector
  new_colours <- result$coloursList[["relabelled_clusters"]]
  colourPaletteList[["relabelled_clusters"]] <- new_colours

  # Freeze inputs to prevent intermediate invalidations during batch update
  freezeReactiveValue(input, "umapColourPalette")
  freezeReactiveValue(input, "umapColumnToPlot")
  freezeReactiveValue(input, "fpColumnToPlot")
  freezeReactiveValue(input, "umapColumnToSplit")
  freezeReactiveValue(input, "fpColumnToSplit")

  # Update palette selector to include the new palette
  updateSelectInput(session, "umapColourPalette",
    choices = names(reactiveValuesToList(colourPaletteList)),
    selected = "relabelled_clusters")

  # Refresh all column-based selectInputs
  colsThatCanBePlot <- get_plottable_columns(result$sce)
  updateSelectInput(session, "umapColumnToPlot",
    choices = colsThatCanBePlot, selected = "relabelled_clusters")
  updateSelectInput(session, "fpColumnToPlot",
    choices = c("None", colsThatCanBePlot), selected = "relabelled_clusters")
  updateSelectInput(session, "umapColumnToSplit",
    choices = c("None", colsThatCanBePlot), selected = input$umapColumnToSplit)
  updateSelectInput(session, "fpColumnToSplit",
    choices = c("None", colsThatCanBePlot), selected = input$fpColumnToSplit)

  showNotification("Relabelling applied successfully.", type = "message", duration = 4)
})

# ── Column change observer ───────────────────────────────────────────────────

observeEvent(input$umapColumnToPlot, ignoreInit = TRUE, {
  new_col <- input$umapColumnToPlot
  req(new_col, inputDataReactive$Results)

  # Skip relabelled columns — they are derived, not directly relabellable
  if (new_col == "relabelled_clusters" || grepl("_relabelled$", new_col)) return(invisible())

  old_col <- relabelStorageReactive$activeColumn

  # Save current column's state before switching
  if (!is.null(old_col) && !is.null(clusterTableReactive$table)) {
    relabelStorageReactive$tables[[old_col]]  <- clusterTableReactive$table
    relabelStorageReactive$colours[[old_col]] <- extract_colour_mapping(clusterTableReactive$table)
  }

  # Load or initialise the new column's table
  if (!is.null(relabelStorageReactive$tables[[new_col]])) {
    clusterTableReactive$table <- relabelStorageReactive$tables[[new_col]]
  } else {
    sce <- inputDataReactive$Results$sce
    tbl <- initialise_relabel_table(sce, new_col,
             inputDataReactive$Results$coloursList, catalystCols)
    clusterTableReactive$table                    <- tbl
    relabelStorageReactive$tables[[new_col]]       <- tbl
    relabelStorageReactive$colours[[new_col]]      <- extract_colour_mapping(tbl)
  }

  relabelStorageReactive$activeColumn <- new_col
})

# ── Import from Excel ────────────────────────────────────────────────────────

observeEvent(input$importFile, {
  req(input$importFile, inputDataReactive$Results)

  tryCatch({
    imported_df <- openxlsx::read.xlsx(
      input$importFile$datapath, colNames = TRUE
    )
    imported_df <- imported_df |>
      data.frame(check.names = FALSE) |>
      tibble::column_to_rownames("original")

    # Validate: every cluster in the active column must be in the imported file
    active_col <- relabelStorageReactive$activeColumn %||% "cluster_id"
    sce_clusters <- unique(as.character(
      SummarizedExperiment::colData(inputDataReactive$Results$sce)[[active_col]]
    ))
    missing_in_import <- setdiff(sce_clusters, rownames(imported_df))
    if (length(missing_in_import) > 0) {
      shinyalert::shinyalert(
        title               = "The marmots say no",
        text                = paste0(
          "The uploaded file is missing clusters present in '", active_col,
          "': ", paste(missing_in_import, collapse = ", "),
          ". Are you sure it belongs to this study?"
        ),
        closeOnEsc          = TRUE,
        closeOnClickOutside = TRUE,
        showCancelButton    = TRUE,
        imageUrl            = ""
      )
      return(invisible())
    }

    imported_df$relabelled_clusters <- factor(
      imported_df$relabelled_clusters,
      levels = unique(gtools::mixedsort(as.character(imported_df$relabelled_clusters)))
    )
    clusterTableReactive$table <- imported_df

    # Persist into storage
    col <- relabelStorageReactive$activeColumn
    if (!is.null(col)) {
      relabelStorageReactive$tables[[col]]  <- imported_df
      relabelStorageReactive$colours[[col]] <- extract_colour_mapping(imported_df)
    }

    showNotification("Labels imported successfully. Applying...", type = "message", duration = 3)

    # Auto-apply
    shinyjs::click("applyRelabelling")

  }, error = function(e) {
    showNotification(paste("Failed to read file:", e$message), type = "error")
  })
})
