# ── server-dads.R ──────────────────────────────────────────────────────────────
# DA (Differential Abundance) and DS (Differential State) results display.
# Renders interactive DT tables and CATALYST::plotDiffHeatmap heatmaps
# for each contrast, matching the pipeline QMD output.
# ──────────────────────────────────────────────────────────────────────────────

# ── Populate contrast dropdown once data loads ────────────────────────────────
observeEvent(inputDataReactive$Results, once = TRUE, {
  res <- inputDataReactive$Results
  req(res)

  da_names <- if (!is.null(res$daList)) names(res$daList) else character(0)
  ds_names <- if (!is.null(res$dsList)) names(res$dsList) else character(0)
  all_contrasts <- unique(c(da_names, ds_names))

  if (length(all_contrasts) == 0) all_contrasts <- "None"
  updateSelectInput(session, "dadsContrast",
    choices = all_contrasts, selected = all_contrasts[1])
})

# ── Dynamic output container (table or plot) ─────────────────────────────────
output$dadsOutputUI <- renderUI({
  result_type <- input$dadsResultType %||% "DA Table"

  if (grepl("Table", result_type)) {
    DT::dataTableOutput(outputId = "dadsTable")
  } else {
    shinycssloaders::withSpinner(
      plotOutput(outputId = "dadsPlot", inline = TRUE),
      type = 6, color = "#dc2626"
    )
  }
})

# ── DA/DS Table ──────────────────────────────────────────────────────────────
output$dadsTable <- DT::renderDataTable({
  req(inputDataReactive$Results)
  req(input$dadsResultType, input$dadsContrast)
  req(input$dadsContrast != "None")

  res         <- inputDataReactive$Results
  contrast    <- input$dadsContrast
  result_type <- input$dadsResultType

  df <- NULL

  if (result_type == "DA Table" && !is.null(res$daList[[contrast]])) {
    df <- as.data.frame(res$daList[[contrast]])
    # Keep key columns; preserve any extras diffcyt added
    key_cols <- intersect(c("cluster_id", "logFC", "logCPM", "LR", "p_val", "p_adj"), colnames(df))
    df <- df[, key_cols, drop = FALSE]
    df <- df[order(df$p_adj), ]

  } else if (result_type == "DS Table" && !is.null(res$dsList[[contrast]])) {
    df <- as.data.frame(res$dsList[[contrast]])
    key_cols <- intersect(c("cluster_id", "marker_id", "logFC", "p_val", "p_adj"), colnames(df))
    df <- df[, key_cols, drop = FALSE]
    df <- df[order(df$p_val), ]
  }

  req(!is.null(df), nrow(df) > 0)

  # Natural sort cluster_id
  if ("cluster_id" %in% colnames(df)) {
    df$cluster_id <- factor(df$cluster_id,
      levels = gtools::mixedsort(unique(as.character(df$cluster_id))))
  }

  # Numeric columns to style
  num_cols <- intersect(c("logFC", "logCPM", "LR", "p_val", "p_adj"), colnames(df))
  pval_cols <- intersect(c("p_val", "p_adj"), colnames(df))

  dt <- DT::datatable(
    data      = df,
    rownames  = FALSE,
    class     = "display compact",
    selection = "none",
    options   = list(
      dom        = "frtip",
      pageLength = 25,
      scrollX    = TRUE
    )
  )

  if (length(num_cols) > 0) {
    dt <- DT::formatSignif(dt, columns = num_cols, digits = 3)
  }
  if ("logFC" %in% colnames(df)) {
    dt <- DT::formatStyle(dt,
      columns    = "logFC",
      color      = DT::styleInterval(cuts = 0, values = c("blue", "darkorange")),
      fontWeight = "bold"
    )
  }
  if (length(pval_cols) > 0) {
    dt <- DT::formatStyle(dt,
      columns    = pval_cols,
      color      = DT::styleInterval(cuts = 0.05, values = c("green", "black")),
      fontWeight = "bold"
    )
  }

  dt
})

# ── DA/DS Heatmap ────────────────────────────────────────────────────────────
output$dadsPlot <- renderPlot({
  req(inputDataReactive$Results)
  req(input$dadsResultType, input$dadsContrast)
  req(input$dadsContrast != "None")
  req(grepl("Heatmap", input$dadsResultType))

  if (!requireNamespace("CATALYST", quietly = TRUE)) {
    showNotification("CATALYST package is not installed.", type = "error")
    return(NULL)
  }

  res         <- inputDataReactive$Results
  sce         <- res$sce
  contrast    <- input$dadsContrast
  result_type <- input$dadsResultType
  top_n       <- as.integer(input$dadsTopN %||% 20)
  conditions  <- res$conditions
  if (is.null(conditions) || length(conditions) == 0) conditions <- "condition"
  # Only use conditions that exist in colData
  valid_conds <- intersect(conditions,
    colnames(SummarizedExperiment::colData(sce)))

  tryCatch({
    hm <- NULL

    if (result_type == "DA Heatmap" && !is.null(res$daList[[contrast]])) {
      hm <- CATALYST::plotDiffHeatmap(sce, res$daList[[contrast]],
        normalize = TRUE, all = TRUE, top_n = top_n,
        col_anno = if (length(valid_conds) > 0) valid_conds else TRUE)

    } else if (result_type == "DS Heatmap" && !is.null(res$dsList[[contrast]])) {
      fdr_thresh <- as.numeric(input$dadsFDR %||% 0.05)
      hm <- CATALYST::plotDiffHeatmap(sce, res$dsList[[contrast]],
        normalize = TRUE, all = TRUE, fdr = fdr_thresh,
        col_anno = if (length(valid_conds) > 0) valid_conds else TRUE)
    }

    req(!is.null(hm))

    # Apply MARMOT condition colours to the heatmap annotations
    coloursList <- res$coloursList
    if (!is.null(coloursList) && inherits(hm, "Heatmap") || inherits(hm, "HeatmapList")) {
      for (con in valid_conds) {
        tryCatch({
          anno <- hm@top_annotation@anno_list[[con]]
          if (!is.null(anno) && !is.null(coloursList[[con]])) {
            matched <- coloursList[[con]][match(
              names(anno@color_mapping@colors),
              names(coloursList[[con]]))]
            anno@color_mapping@colors   <- matched
            anno@color_mapping@full_col <- matched
            anno@fun@var_env[["color_mapping"]]@colors   <- matched
            anno@fun@var_env[["color_mapping"]]@full_col <- matched
            hm@top_annotation@anno_list[[con]] <- anno
          }
        }, error = function(e) NULL)  # Skip colour patching on error
      }
    }

    ComplexHeatmap::draw(hm)
  }, error = function(e) {
    showNotification(
      paste("DA/DS Heatmap error:", e$message),
      type = "error", duration = 8
    )
    NULL
  })
},
height = function() { as.integer(input$dadsFigHeight %||% 700) },
width  = function() { as.integer(input$dadsFigWidth  %||% 900) })
