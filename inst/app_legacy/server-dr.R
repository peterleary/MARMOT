# server-dr.R
# Dimensionality reduction plot (static + interactive)

# Adaptive debounce timing based on dataset size ----
umap_debounce_ms <- reactive({
  ncell <- inputDataReactive$Results$ncell %||% 0L
  if (ncell > 200000L) 700L else if (ncell > 50000L) 600L else 500L
})

# Capture all DR inputs into a single reactive for debouncing
umap_inputs_raw <- reactive({
  list(
    input$umapDRToPlot,
    input$umapColumnToPlot,
    input$textSizeUMAP,
    input$pointSizeUMAP,
    input$umapShowLabels,
    input$umapShowAxes,
    input$umapLegendPosition,
    input$umapColumnToSplit,
    input$umapBorderType,
    input$borderSizeUMAP,
    input$umapBorderColour,
    input$densityLineWidth,
    input$densityThreshold,
    input$densityLineColour,
    input$umapMainNcol,
    input$pointAlphaUMAP,
    input$labelSizeUMAP,
    input$labelShiftUMAP,
    input$umapShowDAClusters,
    input$umapContrastToUse,
    clusterTableReactive$table,
    lapply(names(colsList1), function(col) {
      lapply(names(colsList1[[col]]), function(lor) {
        input[[paste0("GroupColour", col, lor)]]
      })
    })
  )
})

umap_inputs <- umap_inputs_raw |> debounce(umap_debounce_ms)

# DR Plot ----
umapReactive <- eventReactive(
  umap_inputs(),
  ignoreNULL = FALSE,
  {
    req(input$umapDRToPlot, input$umapColumnToPlot)
    tryCatch({
      umapColumnToSplit <- if (input$umapColumnToSplit == "None" ||
                               is.null(input$umapColumnToSplit)) {
        NULL
      } else {
        input$umapColumnToSplit
      }

      contrasts_vec <- inputDataReactive$Results$smd$`Conditions To Test`
      contrasts_vec <- contrasts_vec[!is.na(contrasts_vec)]
      n_contrasts <- length(contrasts_vec)
      contrastToUse <- which(contrasts_vec == input$umapContrastToUse)
      if (length(contrastToUse) == 0) contrastToUse <- 1L
      contrastIndexes <- seq(1, max(1, 2 * n_contrasts - 1), by = 2)[contrastToUse]
      clustersToPlot <- inputDataReactive$Results$selectedClustersList[c(contrastIndexes, contrastIndexes + 1)]

      umapDF <- inputDataReactive$Results$umapDFList[[paste0("Downsampled.", input$umapDRToPlot)]]
      req(!is.null(umapDF))

      # DA cluster subsetting (pure helper)
      da_result <- filter_da_clusters(umapDF, clustersToPlot, mode = input$umapShowDAClusters)
      umapDF <- da_result$umap_df
      if (!is.null(da_result$warning)) {
        showNotification(da_result$warning, type = "error")
      }

      if (input$umapShowDAClusters != "None") {
        inputDataReactive$Results$coloursList$cluster_id[["Other"]] <- "grey80"
      }

      # Interactive plotly plot ----
      col <- input$umapColumnToPlot
      req(col %in% colnames(umapDF))
      colour_column <- as.formula(paste0("~`", col, "`"))
      umapInteractive <- plot_ly(
        data = umapDF,
        x = ~x,
        y = ~y,
        type = "scattergl",
        mode = "markers",
        color = colour_column,
        colors = inputDataReactive$Results$coloursList[[input$umapColumnToPlot]],
        text = colour_column,
        hovertemplate = paste(input$umapColumnToPlot,
                            ": %{text}<br>", "<extra></extra>"),
        marker = list(
          size = input$pointSizeUMAP * 10,
          color = "fill_colour",
          line = list(color = "black", width = input$borderSizeUMAP %||% 0)
        )
      )

      # With facet
      if (!is.null(umapColumnToSplit) && input$umapColumnToSplit != "None") {
        split_col <- input$umapColumnToSplit
        split_levels <- unique(umapDF[[split_col]])
        umapMainNcol <- floor(length(split_levels) / input$umapMainNcol)
        plots <- umapDF |>
          split(umapDF[[input$umapColumnToSplit]]) |>
          purrr::imap(function(df_sub, i) {
            plot_ly(
              data = df_sub,
              x = ~x, y = ~y,
              type = "scattergl", mode = "markers",
              color = colour_column,
              colors = inputDataReactive$Results$coloursList[[input$umapColumnToPlot]],
              text = colour_column,
              hovertemplate = paste0(
                input$umapColumnToPlot, ": %{text}<br>",
                split_col, ": ", i, "<br>", "<extra></extra>"
              ),
              marker = list(
                size = input$pointSizeUMAP * 10,
                color = "fill_colour",
                line = list(color = "black", width = input$borderSizeUMAP %||% 0)
              )
            ) |> layout(title = NULL)
          })
        umapInteractive <- subplot(
          plots,
          nrows = umapMainNcol,
          shareX = TRUE, shareY = TRUE,
          titleX = TRUE, titleY = TRUE,
          margin = 0.05
        ) |>
          layout(showlegend = TRUE) |>
          plotly_build()

        # Deduplicate legend entries
        trace_names <- purrr::map_chr(
          umapInteractive$x$data, ~.x$name %||% ""
        )
        unique_names <- unique(trace_names)
        first_occurrences <- match(unique_names, trace_names)
        for (i in seq_along(umapInteractive$x$data)) {
          trace_name <- umapInteractive$x$data[[i]]$name %||% ""
          umapInteractive$x$data[[i]]$legendgroup <- trace_name
          umapInteractive$x$data[[i]]$showlegend <- (
            i %in% first_occurrences
          )
        }
      }

      # Static DR plot ----
      border_type <- input$umapBorderType %||% "Density borders"

      # Rasterisation only compatible with "None" (scattermore has no stroke/overlay support)
      use_raster <- isTRUE(inputDataReactive$Results$rasterise_auto) &&
                    border_type == "None"

      umapStatic <- ggplot(umapDF, aes(x = .data[["x"]], y = .data[["y"]]))

      if (use_raster) {
        # scattermore: rasterised for large datasets
        umapStatic <- umapStatic +
          scattermore::geom_scattermore(
            pointsize = (input$pointSizeUMAP * 2) + 0.6,
            pixels    = c(1000L, 1000L),
            alpha     = input$pointAlphaUMAP,
            aes(colour = .data[[input$umapColumnToPlot]])
          ) +
          guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1), ncol = 2))

      } else if (border_type == "Per-cell") {
        umapStatic <- umapStatic +
          geom_point(pch = 21, alpha = input$pointAlphaUMAP, size = input$pointSizeUMAP,
                     stroke = input$borderSizeUMAP %||% 0.5,
                     colour = input$umapBorderColour %||% "black",
                     aes(fill = .data[[input$umapColumnToPlot]])) +
          guides(fill = guide_legend(override.aes = list(shape = 21, size = 3, alpha = 1, stroke = 0.2), ncol = 2))

      } else if (border_type == "Density borders") {
        # 3-layer sandwich replicating SCpubr plot_cell_borders (no Seurat needed):
        # 1. MASS::kde2d estimates 2D density over all cell positions on a 100x100 grid
        # 2. Each cell is mapped to its local density via findInterval() + matrix lookup
        # 3. Cells below the border_density quantile (peripheral/edge cells) are "border cells"
        # Layer order: large dark border cells → grey base → coloured foreground
        border_size    <- input$densityLineWidth  %||% 3
        border_density <- input$densityThreshold  %||% 1
        border_colour  <- input$densityLineColour %||% "black"

        kde <- MASS::kde2d(umapDF[["x"]], umapDF[["y"]], n = 100L)
        ix  <- pmax(1L, pmin(findInterval(umapDF[["x"]], kde$x), length(kde$x)))
        iy  <- pmax(1L, pmin(findInterval(umapDF[["y"]], kde$y), length(kde$y)))
        cell_density <- kde$z[cbind(ix, iy)]
        border_df    <- umapDF[cell_density < quantile(cell_density, border_density), ]

        umapStatic <- umapStatic +
          geom_point(data = border_df,
                     size   = input$pointSizeUMAP * border_size,
                     colour = border_colour, show.legend = FALSE) +
          geom_point(colour = "grey75", size = input$pointSizeUMAP,
                     show.legend = FALSE) +
          geom_point(aes(colour = .data[[input$umapColumnToPlot]]),
                     size = input$pointSizeUMAP) +
          guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1), ncol = 2))

      } else {
        # "None"
        umapStatic <- umapStatic +
          geom_point(aes(colour = .data[[input$umapColumnToPlot]]),
                     alpha = input$pointAlphaUMAP, size = input$pointSizeUMAP) +
          guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1), ncol = 2))
      }

      umapStatic <- umapStatic +
        marmot_dr_theme(
          base_size = input$textSizeUMAP,
          show_axes = input$umapShowAxes,
          legend_position = tolower(input$umapLegendPosition %||% "right")
        ) +
        coord_fixed()

      if (!is.null(umapColumnToSplit)) {
        umapStatic <- add_facet_with_counts(umapStatic, umapDF, umapColumnToSplit, input$umapMainNcol)
      }

      # Use stored colours; fall back to a generated palette for unlisted columns
      col_vals <- inputDataReactive$Results$coloursList[[input$umapColumnToPlot]]
      if (is.null(col_vals)) {
        lvls <- unique(na.omit(umapDF[[input$umapColumnToPlot]]))
        col_vals <- setNames(
          colorspace::qualitative_hcl(length(lvls), palette = "Dark 3"),
          lvls
        )
      }
      # Apply scale matching the aesthetic used by the current border type
      if (border_type == "Per-cell") {
        umapStatic <- umapStatic +
          scale_fill_manual(values = col_vals, na.value = "grey80")
      } else {
        umapStatic <- umapStatic +
          scale_colour_manual(values = col_vals, na.value = "grey80")
      }

      if (input$umapShowLabels) {
        median_pos <- compute_label_positions(umapDF, input$umapColumnToPlot)
        umapStatic <- umapStatic +
          ggnewscale::new_scale_fill() +
          ggrepel::geom_label_repel(
            data = median_pos,
            aes(label = .data[[input$umapColumnToPlot]],
                x = .data[["x"]], y = .data[["y"]],
                fill = .data[[input$umapColumnToPlot]]),
            inherit.aes = FALSE,
            colour = "white", fontface = "bold",
            show.legend = FALSE,
            size = input$labelSizeUMAP,
            max.overlaps = 100,
            nudge_y = input$labelShiftUMAP / 5,
            nudge_x = input$labelShiftUMAP / 5
          ) +
          scale_fill_manual(values = col_vals)
      }

      return(list(
        "umapInteractive" = umapInteractive,
        "umapStatic" = umapStatic
      ))
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error")) {
        showNotification(conditionMessage(e), type = "error")
      }
      NULL
    })
  }
)

output$umapInteractive <- renderPlotly({
  req(!is.null(umapReactive()))
  umapReactive()$umapInteractive |>
    layout(
      width = input$figWidthUMAP,
      height = input$figHeightUMAP,
      legend = list(
        font = list(family = "Arial", size = input$textSizeUMAP),
        title = list(
          font = list(family = "Arial", size = input$textSizeUMAP + 2)
        )
      )
    )
})

output$umapStatic <- renderPlot(
  { req(!is.null(umapReactive())); umapReactive()$umapStatic },
  height = function() input$figHeightUMAP,
  width = function() input$figWidthUMAP
)
