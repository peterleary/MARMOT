# ── server-download.R ──────────────────────────────────────────────────────────
# Download handlers for Shiny MARMOT v2.
# Merges exploreSingleCell settings-tracking pattern with MARMOT-specific
# exports (FCS files with DR coords + cluster codes).
# ──────────────────────────────────────────────────────────────────────────────

# ── Settings tracking (exploreSingleCell pattern) ────────────────────────────
# Debounced to avoid rebuilding on every slider drag
saved_inputs_raw <- reactive({
  data.frame(
    Input = names(input),
    Value = sapply(names(input), function(x) paste(input[[x]], collapse = " ")),
    stringsAsFactors = FALSE
  )
})
saved_inputs_debounced <- saved_inputs_raw |> debounce(2000)
saved_inputs <- reactiveVal(
  data.frame(Input = character(), Value = character(), stringsAsFactors = FALSE)
)
observe({ saved_inputs(saved_inputs_debounced()) })

# ── DR plot download ─────────────────────────────────────────────────────────
output$dlUMAP <- downloadHandler(
  filename = function() {
    dr  <- input$umapDRToPlot %||% "DR"
    col <- input$umapColumnToPlot %||% "plot"
    fmt <- tolower(input$dlFormat %||% "pdf")
    paste(dr, col, fmt, sep = ".")
  },
  content = function(file) {
    req(!is.null(umapReactive()$umapStatic))
    w <- as.numeric(input$figWidthUMAP) / 60
    h <- as.numeric(input$figHeightUMAP) / 60

    if (input$dlFormat == "PDF") {
      pdf(file = file, width = w, height = h)
    } else if (input$dlFormat == "SVG") {
      svg(file = file, width = w, height = h)
    } else if (input$dlFormat == "PNG") {
      png(filename = file, width = w, height = h,
          units = "in", res = as.numeric(input$pngRes))
    }
    plot(umapReactive()$umapStatic)
    dev.off()
  }
)

# ── Feature plot download ────────────────────────────────────────────────────
output$dlFP <- downloadHandler(
  filename = function() {
    plottype <- gsub(" ", "", input$featurePlotType %||% "FeaturePlot")
    genes    <- paste(input$fpFeatureToPlot, collapse = "_")
    if (!nzchar(genes)) genes <- "markers"
    fmt      <- tolower(input$dlFormat %||% "pdf")
    paste(plottype, genes, fmt, sep = ".")
  },
  content = function(file) {
    req(!is.null(featurePlotReactive$fp))
    fpw <- as.numeric(input$figWidthFP %||% 0)
    if (fpw <= 0) fpw <- session$clientData$output_umapFeaturePlotOutput_width %||% 1000   # "fit": the box's width
    w  <- fpw / 60
    h  <- as.numeric(input$figHeightFP) / 60
    fp <- featurePlotReactive$fp

    is_heatmap <- inherits(fp, "Heatmap") || inherits(fp, "HeatmapList")
    needs_arrange <- isTRUE(featurePlotReactive$needs_arrange) &&
      is.list(fp) && length(fp) >= 2

    if (is_heatmap) {
      # ComplexHeatmap requires explicit device open/draw/close
      if (input$dlFormat == "PDF") {
        pdf(file = file, width = w, height = h)
      } else if (input$dlFormat == "SVG") {
        svg(file = file, width = w, height = h)
      } else if (input$dlFormat == "PNG") {
        png(filename = file, width = w, height = h,
            units = "in", res = as.numeric(input$pngRes))
      }
      ComplexHeatmap::draw(fp)
      dev.off()

    } else if (needs_arrange) {
      # Patchwork assembly for multi-panel feature/violin/ridge/nebulosa plots
      ncol_fp <- fp_columns(input$ncolFPGene, length(fp))
      combined <- patchwork::wrap_plots(fp, ncol = as.integer(ncol_fp))
      ggplot2::ggsave(
        filename = file, plot = combined,
        width = w, height = h, units = "in",
        dpi = if (input$dlFormat == "PNG") as.numeric(input$pngRes) else 300,
        device = tolower(input$dlFormat)
      )

    } else {
      # Single ggplot
      ggplot2::ggsave(
        filename = file, plot = fp,
        width = w, height = h, units = "in",
        dpi = if (input$dlFormat == "PNG") as.numeric(input$pngRes) else 300,
        device = tolower(input$dlFormat)
      )
    }
  }
)

# ── Settings download (Excel) ────────────────────────────────────────────────
output$downloadInputsE <- downloadHandler(
  filename = function() "MARMOT_Settings.xlsx",
  content = function(file) {
    writexl::write_xlsx(saved_inputs(), path = file)
  }
)

# ── Cluster codes download (Excel) ──────────────────────────────────────────
output$downloadClusterCodes <- downloadHandler(
  filename = function() "cluster_codes.xlsx",
  content = function(file) {
    umap_a <- inputDataReactive$Results$umapDFList[["All"]]
    req(umap_a)
    codes <- build_cluster_codes(umap_a)
    writexl::write_xlsx(codes, path = file)
  }
)

# ── FCS export (zip of modified .fcs files) ──────────────────────────────────
output$downloadFCS <- downloadHandler(
  filename = function() {
    paste0("MARMOT_FCS_export_", Sys.Date(), ".zip")
  },
  content = function(file) {
    res <- inputDataReactive$Results
    md  <- res$md

    # Event numbers tie each cell back to its row in the original FCS file
    event_nums <- MARMOT::event_numbers(res$sce)
    if (is.null(event_nums)) {
      showNotification(
        paste("These results cannot export FCS files: they do not record which event of the",
              "original FCS file each cell came from. Re-run the pipeline with the current MARMOT."),
        type = "error", duration = 12
      )
      return(invisible())
    }

    req(!is.null(md$sample_id))

    # Build cluster code lookup from the full DR data frame
    umap_a <- res$umapDFList[["All"]]
    codes  <- build_cluster_codes(umap_a)

    # One row per cell in the report, with the columns to append
    cells <- data.frame(
      file_name        = md$file_name[match(umap_a$sample_id, md$sample_id)],
      sample_id        = as.character(umap_a$sample_id),
      event_number     = event_nums[umap_a$sce_idx],
      umap_x           = umap_a$x,
      umap_y           = umap_a$y,
      cluster_id_codes = codes$cluster_id_codes[match(umap_a$cluster_id, codes$cluster_ids)],
      stringsAsFactors = FALSE
    )
    # If relabelled clusters exist, include those codes too
    if ("relabelled_clusters" %in% colnames(umap_a)) {
      cells$new_cluster_codes <- codes$new_cluster_codes[match(umap_a$cluster_id, codes$cluster_ids)]
    }

    # The original FCS files normally sit above the results folder
    fcs_dir <- if (nzchar(trimws(input$fcsFolder %||% ""))) {
      trimws(input$fcsFolder)
    } else {
      dirname(dirname(res$dataDir))
    }

    temp_dir <- file.path(tempdir(), as.integer(Sys.time()))
    dir.create(temp_dir, recursive = TRUE)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    # Write one .fcs per sample with appended DR coords + cluster codes
    failed <- FALSE
    tryCatch(
      MARMOT::write_annotated_fcs(cells, fcs_dir = fcs_dir, out_dir = temp_dir,
                                  source_files = res$sourceFiles),
      marmot_fcs_not_found = function(e) {
        failed <<- TRUE
        showNotification(
          paste0("Can't find the original FCS files in ", fcs_dir,
                 ". If they have moved, type their folder into the box under the FCS files button."),
          type = "error", duration = 12
        )
      },
      marmot_fcs_changed = function(e) {
        failed <<- TRUE
        showNotification(conditionMessage(e), type = "error", duration = 12)
      }
    )
    if (failed) return(invisible())

    # Bundle cluster codes Excel into the zip as well
    codes_path <- file.path(temp_dir, "cluster_codes.xlsx")
    writexl::write_xlsx(codes, path = codes_path)

    zip::zip(zipfile = file, files = dir(temp_dir), root = temp_dir)
  },
  contentType = "application/zip"
)
