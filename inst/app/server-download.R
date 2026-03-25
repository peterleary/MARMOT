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
    w  <- as.numeric(input$figWidthFP) / 60
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
      ncol_fp <- input$ncolFPGene %||% 2
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
    flist <- inputDataReactive$Results$framesList
    md    <- inputDataReactive$Results$md

    if (is.null(flist)) {
      showNotification(
        "No FCS frames available for export. The pipeline was run without FCS sidecar data.",
        type = "error", duration = 8
      )
      return(invisible())
    }

    req(!is.null(md$sample_id))

    # Build cluster code lookup from the full DR data frame
    umap_a <- inputDataReactive$Results$umapDFList[["All"]]
    codes  <- build_cluster_codes(umap_a)

    temp_dir <- file.path(tempdir(), as.integer(Sys.time()))
    dir.create(temp_dir, recursive = TRUE)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    # Write one .fcs per sample with appended DR coords + cluster codes
    lapply(levels(md$sample_id), function(s) {
      umap_s <- umap_a[umap_a$sample_id == s, , drop = FALSE]

      # Columns to append: DR coordinates + numeric cluster code
      appended <- data.frame(
        umap_x          = umap_s$x,
        umap_y          = umap_s$y,
        cluster_id_codes = codes$cluster_id_codes[
          match(umap_s$cluster_id, codes$cluster_ids)
        ]
      )

      # If relabelled clusters exist, include those codes too
      if ("relabelled_clusters" %in% colnames(umap_s)) {
        appended$new_cluster_codes <- codes$new_cluster_codes[
          match(umap_s$cluster_id, codes$cluster_ids)
        ]
      }

      # Resolve original filename and write modified frame
      fn_orig <- md$file_name[md$sample_id == s][[1]]
      fn_out  <- file.path(temp_dir, paste0(s, "_modified.fcs"))

      flowCore::write.FCS(
        x = flowCore::fr_append_cols(
          fr   = flist$Untransformed[[fn_orig]],
          cols = as.matrix(appended)
        ),
        filename  = fn_out,
        delimiter = "#"
      )
    })

    # Bundle cluster codes Excel into the zip as well
    codes_path <- file.path(temp_dir, "cluster_codes.xlsx")
    writexl::write_xlsx(codes, path = codes_path)

    zip::zip(zipfile = file, files = dir(temp_dir), root = temp_dir)
  },
  contentType = "application/zip"
)
