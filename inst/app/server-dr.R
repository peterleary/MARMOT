# ── server-dr.R ──────────────────────────────────────────────────────────────
# Dimensionality reduction plot module for MARMOT Shiny app.
# Renders the main DR scatter plot (static ggplot + interactive plotly)
# from pre-computed umapDFList data frames.
# ─────────────────────────────────────────────────────────────────────────────

# ── Debounced input grouping ─────────────────────────────────────────────────
# All DR-related inputs in a single reactive, debounced adaptively.
# debounce_umap_ms is set in server-colours.R (500/600/700 by cell count).
umap_inputs_raw <- reactive({
  border_type <- input$umapBorderType %||% "None"

  # Border settings conditional on type (avoids redraws when irrelevant sliders move)
  border_settings <- switch(border_type,
    "Density borders" = list(input$scpubrBorderSize,
      input$scpubrBorderColour, input$scpubrBorderDensity),
    "Per-cell borders" = list(input$borderSizeUMAP, input$umapBorderColour),
    NULL
  )

  c(list(
    umapDRToPlot      = input$umapDRToPlot,
    umapColumnToPlot  = input$umapColumnToPlot,
    umapColumnToSplit = input$umapColumnToSplit,
    umapContrast      = input$umapContrast,
    umapDAFilter      = input$umapDAFilter,
    umapColourPalette = input$umapColourPalette,
    textSizeUMAP      = input$textSizeUMAP,
    pointSizeUMAP     = input$pointSizeUMAP,
    pointAlphaUMAP    = input$pointAlphaUMAP,
    umapShowLabels    = input$umapShowLabels,
    umapShowAxes      = input$umapShowAxes,
    umapBorderType    = border_type,
    umapMainNcol      = input$umapMainNcol,
    labelSizeUMAP     = input$labelSizeUMAP,
    labelAlphaUMAP    = input$labelAlphaUMAP,
    labelShiftUMAP    = input$labelShiftUMAP,
    rasteriseUMAP     = input$rasteriseUMAP,
    rasterUMAP_DPI    = input$rasterUMAP_DPI,
    figWidthUMAP      = input$figWidthUMAP,
    figHeightUMAP     = input$figHeightUMAP
  ), border_settings)
})

umap_debounce_ms <- reactive({
  ncell <- inputDataReactive$Results$ncell %||% 0L
  if (ncell > 200000L) 700L else if (ncell > 50000L) 600L else 500L
})

umap_inputs <- umap_inputs_raw |> debounce(umap_debounce_ms)


# ── Main DR reactive ────────────────────────────────────────────────────────
umapReactive <- eventReactive(
  umap_inputs(),
  ignoreNULL = FALSE,
  {
    req(input$umapDRToPlot, input$umapColumnToPlot)
    tryCatch({
      inputs <- umap_inputs()
      res    <- inputDataReactive$Results
      req(res)

      # ── Resolve DR data frame ───────────────────────────────────────────
      # UI choices come from reducedDimNames(sce) (e.g. "UMAP", "TSNE"),
      # but umapDFList keys may be "Downsampled.UMAP" etc.
      dr_key <- inputs$umapDRToPlot
      umapDF <- res$umapDFList[[dr_key]]
      if (is.null(umapDF)) {
        umapDF <- res$umapDFList[[paste0("Downsampled.", dr_key)]]
      }
      if (is.null(umapDF)) {
        # Fall back to first available entry
        umapDF <- res$umapDFList[[1]]
      }
      req(!is.null(umapDF))

      # ── DA filtering ────────────────────────────────────────────────────
      da_mode   <- inputs$umapDAFilter %||% "None"
      contrast  <- inputs$umapContrast %||% "None"

      if (da_mode != "None" && contrast != "None" &&
          !is.null(res$selectedClustersList)) {
        # Pipeline names: "Contrast 1: Treatment Up" / "Contrast 1: Control Up"
        # Match by prefix: keys that start with the contrast name
        scl_names <- names(res$selectedClustersList)
        matched   <- scl_names[startsWith(scl_names, contrast)]
        # Identify up vs down by position (odd = up, even = down in pipeline)
        # or by suffix if available
        up_idx   <- grep("Up$|up$|\\.up$", matched)
        down_idx <- grep("Down$|down$|\\.down$", matched)
        # Fallback: if no suffix match, use positional (first = up, second = down)
        if (length(up_idx) == 0 && length(down_idx) == 0 && length(matched) >= 2) {
          up_idx <- 1; down_idx <- 2
        }
        clusters_to_plot <- list(
          if (length(up_idx) > 0) res$selectedClustersList[[matched[up_idx[1]]]] else character(0),
          if (length(down_idx) > 0) res$selectedClustersList[[matched[down_idx[1]]]] else character(0)
        )
        da_result <- filter_da_clusters(umapDF, clusters_to_plot, mode = da_mode)
        umapDF <- da_result$umap_df
        if (!is.null(da_result$warning)) {
          showNotification(da_result$warning, type = "warning")
        }
        # Ensure "Other" has a grey colour
        if (!is.null(res$coloursList$cluster_id)) {
          res$coloursList$cluster_id[["Other"]] <- "grey80"
        }
      } else {
        umapDF$cluster_id <- factor(
          umapDF$cluster_id,
          levels = gtools::mixedsort(unique(as.character(umapDF$cluster_id)))
        )
      }

      # Shuffle for fair cluster overdraw (matches SCpubr shuffle=TRUE default)
      umapDF <- umapDF[sample(nrow(umapDF)), ]

      # ── Resolve colour vector ───────────────────────────────────────────
      plot_column  <- inputs$umapColumnToPlot
      palette_name <- inputs$umapColourPalette %||% "Catalyst"
      req(plot_column %in% colnames(umapDF))

      present_levels <- if (is.factor(umapDF[[plot_column]])) {
        levels(umapDF[[plot_column]])
      } else {
        gtools::mixedsort(unique(as.character(umapDF[[plot_column]])))
      }

      if (plot_column %in% names(res$coloursList)) {
        colours <- res$coloursList[[plot_column]]
        colours <- colours[names(colours) %in% present_levels]
        missing <- setdiff(present_levels, names(colours))
        if (length(missing) > 0) {
          pal   <- reactiveValuesToList(colourPaletteList)[[palette_name]]
          extra <- rep_len(pal, length(missing))
          names(extra) <- missing
          colours <- c(colours, extra)
        }
      } else {
        pal <- reactiveValuesToList(colourPaletteList)[[palette_name]]
        if (is.null(pal)) {
          pal <- colorspace::qualitative_hcl(length(present_levels), palette = "Dark 3")
        }
        pal <- rep_len(pal, length(present_levels))
        colours <- setNames(pal[seq_along(present_levels)], present_levels)
      }

      # ── Split column ────────────────────────────────────────────────────
      split_col <- if (!is.null(inputs$umapColumnToSplit) &&
                       inputs$umapColumnToSplit != "None") {
        inputs$umapColumnToSplit
      } else {
        NULL
      }

      # ================================================================
      # Interactive plotly plot
      # ================================================================
      colour_formula <- as.formula(paste0("~`", plot_column, "`"))

      if (is.null(split_col)) {
        umapInteractive <- plot_ly(
          data   = umapDF,
          x      = ~x, y = ~y,
          type   = "scattergl",
          mode   = "markers",
          color  = colour_formula,
          colors = colours,
          text   = colour_formula,
          hovertemplate = paste0(plot_column, ": %{text}<br><extra></extra>"),
          marker = list(
            size = inputs$pointSizeUMAP * 10,
            line = list(color = "black", width = 0)
          )
        )
      } else {
        # Faceted subplots with deduplicated legends
        split_levels <- unique(umapDF[[split_col]])
        nrows <- ceiling(length(split_levels) / max(1L, inputs$umapMainNcol))

        plots <- umapDF |>
          split(umapDF[[split_col]]) |>
          purrr::imap(function(df_sub, lvl) {
            plot_ly(
              data   = df_sub,
              x      = ~x, y = ~y,
              type   = "scattergl",
              mode   = "markers",
              color  = colour_formula,
              colors = colours,
              text   = colour_formula,
              hovertemplate = paste0(
                plot_column, ": %{text}<br>",
                split_col, ": ", lvl, "<br><extra></extra>"
              ),
              marker = list(
                size = inputs$pointSizeUMAP * 10,
                line = list(color = "black", width = 0)
              )
            ) |> layout(title = NULL)
          })

        umapInteractive <- subplot(
          plots,
          nrows   = nrows,
          shareX  = TRUE,
          shareY  = TRUE,
          titleX  = TRUE,
          titleY  = TRUE,
          margin  = 0.05
        ) |>
          layout(showlegend = TRUE) |>
          plotly_build()

        # Deduplicate legend entries across subplots
        trace_names <- purrr::map_chr(
          umapInteractive$x$data, ~ .x$name %||% ""
        )
        unique_names     <- unique(trace_names)
        first_occurrence <- match(unique_names, trace_names)
        for (i in seq_along(umapInteractive$x$data)) {
          trace_name <- umapInteractive$x$data[[i]]$name %||% ""
          umapInteractive$x$data[[i]]$legendgroup <- trace_name
          umapInteractive$x$data[[i]]$showlegend  <- (i %in% first_occurrence)
        }
      }

      # ================================================================
      # Static ggplot plot
      # ================================================================
      border_type <- inputs$umapBorderType %||% "Density borders"

      # Rasterisation only compatible with plain "None" borders
      use_raster <- isTRUE(res$rasterise_auto) && border_type == "None"

      umapStatic <- ggplot(umapDF, aes(x = .data[["x"]], y = .data[["y"]]))

      # ── Rendering path ──────────────────────────────────────────────
      if (use_raster) {
        # Raster: scattermore for large datasets, no border support
        umapStatic <- umapStatic +
          scattermore::geom_scattermore(
            pointsize = (inputs$pointSizeUMAP * 2) + 0.6,
            pixels    = c(1000L, 1000L),
            alpha     = inputs$pointAlphaUMAP,
            aes(colour = .data[[plot_column]])
          ) +
          guides(colour = guide_legend(
            override.aes = list(size = 3, alpha = 1), ncol = 2
          ))

      } else if (border_type == "Per-cell borders") {
        # pch=21 with stroke for per-cell borders
        # Read border inputs directly (conditionally excluded from debounce list)
        umapStatic <- umapStatic +
          geom_point(
            pch    = 21,
            alpha  = inputs$pointAlphaUMAP,
            size   = inputs$pointSizeUMAP,
            stroke = input$borderSizeUMAP %||% 0.5,
            colour = input$umapBorderColour %||% "black",
            aes(fill = .data[[plot_column]])
          ) +
          guides(fill = guide_legend(
            override.aes = list(shape = 21, size = 3, alpha = 1, stroke = 0.2),
            ncol = 2
          ))

      } else if (border_type == "Density borders") {
        # SCpubr-style 3-layer sandwich: border → grey75 base → coloured foreground
        # Read border inputs directly (conditionally excluded from debounce list)
        border_size    <- input$scpubrBorderSize    %||% 3
        border_density <- input$scpubrBorderDensity %||% 1
        border_colour  <- input$scpubrBorderColour  %||% "black"
        eff_size       <- inputs$pointSizeUMAP / 2   # SCpubr halves pt.size

        kde <- MASS::kde2d(umapDF[["x"]], umapDF[["y"]], n = 100L)
        ix  <- pmax(1L, pmin(findInterval(umapDF[["x"]], kde$x), length(kde$x)))
        iy  <- pmax(1L, pmin(findInterval(umapDF[["y"]], kde$y), length(kde$y)))
        cell_density <- kde$z[cbind(ix, iy)]
        border_df    <- umapDF[cell_density <= quantile(cell_density, border_density), ]

        umapStatic <- umapStatic +
          # Layer 1: border (edge cells)
          geom_point(
            data = border_df,
            size = eff_size * border_size,
            colour = border_colour, show.legend = FALSE,
            na.rm = TRUE
          ) +
          # Layer 2: grey base (all cells)
          geom_point(
            colour = "grey75", size = eff_size,
            show.legend = FALSE, na.rm = TRUE
          ) +
          # Layer 3: coloured foreground (all cells)
          geom_point(
            aes(colour = .data[[plot_column]]),
            size = eff_size,
            na.rm = TRUE
          ) +
          guides(colour = guide_legend(
            override.aes = list(size = 4, colour = "black",
                                fill = colours, shape = 21),
            ncol = 2
          ))

      } else {
        # "None": plain geom_point
        umapStatic <- umapStatic +
          geom_point(
            aes(colour = .data[[plot_column]]),
            alpha = inputs$pointAlphaUMAP,
            size  = inputs$pointSizeUMAP,
            pch   = 20
          ) +
          guides(colour = guide_legend(
            override.aes = list(size = 3, alpha = 1), ncol = 2
          ))
      }

      # ── Theme + coord ───────────────────────────────────────────────
      umapStatic <- umapStatic +
        marmot_dr_theme(
          base_size       = inputs$textSizeUMAP,
          show_axes       = inputs$umapShowAxes,
          legend_position = "right"
        )

      # ── Faceting with cell counts ───────────────────────────────────
      if (!is.null(split_col)) {
        umapStatic <- add_facet_with_counts(
          umapStatic, umapDF, split_col, inputs$umapMainNcol
        )
      }

      # ── Colour scale ────────────────────────────────────────────────
      if (border_type == "Per-cell borders") {
        umapStatic <- umapStatic +
          scale_fill_manual(values = colours, na.value = "grey75")
      } else {
        umapStatic <- umapStatic +
          scale_colour_manual(values = colours, na.value = "grey75")
      }

      # ── Cluster labels (geom_label_repel + ggnewscale) ─────────────
      if (isTRUE(inputs$umapShowLabels)) {
        median_pos <- compute_label_positions(umapDF, plot_column)

        # new_scale_fill() only needed when main scatter uses fill (Per-cell borders);
        # for colour-based paths it breaks override.aes fill in the legend
        if (border_type == "Per-cell borders") {
          umapStatic <- umapStatic + ggnewscale::new_scale_fill()
        }

        umapStatic <- umapStatic +
          ggrepel::geom_label_repel(
            data = median_pos,
            aes(
              label = .data[[plot_column]],
              x     = .data[["x"]],
              y     = .data[["y"]],
              fill  = .data[[plot_column]]
            ),
            inherit.aes  = FALSE,
            colour       = "white",
            fontface     = "bold",
            show.legend  = FALSE,
            alpha        = inputs$labelAlphaUMAP %||% 0.9,
            size         = inputs$labelSizeUMAP,
            max.overlaps = 100,
            nudge_y      = inputs$labelShiftUMAP / 5,
            nudge_x      = inputs$labelShiftUMAP / 5
          ) +
          scale_fill_manual(values = colours)
      }

      # ── Return both plots ───────────────────────────────────────────
      list(
        umapInteractive = umapInteractive,
        umapStatic      = umapStatic
      )

    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error")) {
        showNotification(conditionMessage(e), type = "error")
      }
      NULL
    })
  }
)


# ── Outputs ──────────────────────────────────────────────────────────────────

output$umapInteractive <- renderPlotly({
  req(!is.null(umapReactive()))
  umapReactive()$umapInteractive |>
    layout(
      width  = input$figWidthUMAP,
      height = input$figHeightUMAP,
      legend = list(
        font  = list(family = "Arial", size = input$textSizeUMAP),
        title = list(font = list(family = "Arial", size = input$textSizeUMAP + 2))
      )
    )
})

output$umapStatic <- renderPlot(
  {
    req(!is.null(umapReactive()))
    umapReactive()$umapStatic
  },
  height = function() input$figHeightUMAP,
  width  = function() input$figWidthUMAP
)
