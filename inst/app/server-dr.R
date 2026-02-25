# server-dr.R
# Dimensionality reduction plot (static + interactive)

# DR Plot ----
umapReactive <- eventReactive(
  {
    input$umapDRToPlot
    input$umapColumnToPlot
    input$textSizeUMAP
    input$pointSizeUMAP
    input$umapShowLabels
    input$umapShowAxes
    input$umapLegendPosition
    input$umapColumnToSplit
    input$pointBorderUMAP
    input$borderSizeUMAP
    input$umapMainNcol
    input$pointAlphaUMAP
    input$umapBorderColour
    input$labelSizeUMAP
    input$labelShiftUMAP
    input$umapShowDAClusters
    input$umapContrastToUse
    clusterTableReactive$table
    lapply(names(colsList1), function(col) {
      lapply(names(colsList1[[col]]), function(lor) {
        input[[paste0("GroupColour", col, lor)]]
      })
    })
  },
  ignoreNULL = FALSE,
  {
    tryCatch({
      umapColumnToSplit <- if (input$umapColumnToSplit == "None" ||
                               is.null(input$umapColumnToSplit)) {
        NULL
      } else {
        input$umapColumnToSplit
      }

      contrastToUse <- grep(input$umapContrastToUse, inputDataReactive$Results$smd$`Conditions To Test`)
      contrastIndexes <- seq(1, 11, by = 2)[contrastToUse]
      clustersToPlot <- inputDataReactive$Results$selectedClustersList[c(contrastIndexes, contrastIndexes + 1)]

      umapDF <- inputDataReactive$Results$umapDFList[[paste0("Downsampled.", input$umapDRToPlot)]]

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
      colour_column <- as.formula(paste0("~", input$umapColumnToPlot))
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
          size = input$pointSizeUMAP * 2,
          color = "fill_colour",
          line = list(color = "black", width = input$borderSizeUMAP)
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
                size = input$pointSizeUMAP * 3,
                color = "fill_colour",
                line = list(color = "black", width = input$borderSizeUMAP)
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
      umapStatic <- ggplot(umapDF, aes(x = .data[["x"]], y = .data[["y"]]))
      if (input$borderSizeUMAP > 0) {
        umapStatic <- umapStatic +
          geom_point(pch = 21, alpha = input$pointAlphaUMAP, size = input$pointSizeUMAP,
                     stroke = input$borderSizeUMAP, colour = input$umapBorderColour,
                     aes(fill = .data[[input$umapColumnToPlot]])) +
          guides(fill = guide_legend(override.aes = list(shape = 21, size = 5, stroke = 0.2)))
      } else {
        umapStatic <- umapStatic +
          geom_point(pch = 20, alpha = input$pointAlphaUMAP, size = input$pointSizeUMAP,
                     aes(colour = .data[[input$umapColumnToPlot]])) +
          guides(colour = guide_legend(override.aes = list(shape = 20, size = 6, stroke = 0.2)))
      }

      umapStatic <- umapStatic +
        marmot_dr_theme(base_size = input$textSizeUMAP, show_axes = input$umapShowAxes)

      if (!is.null(umapColumnToSplit)) {
        umapStatic <- add_facet_with_counts(umapStatic, umapDF, umapColumnToSplit, input$umapMainNcol)
      }

      if (input$umapShowLabels) {
        median_pos <- compute_label_positions(umapDF, input$umapColumnToPlot)
        umapStatic <- umapStatic +
          ggrepel::geom_label_repel(
            data = median_pos,
            aes(label = .data[[input$umapColumnToPlot]], x = .data[["x"]], y = .data[["y"]],
                fill = .data[[input$umapColumnToPlot]]),
            show.legend = FALSE, size = input$labelSizeUMAP,
            nudge_y = input$labelShiftUMAP / 5, nudge_x = input$labelShiftUMAP / 5
          )
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
      umapStatic <- umapStatic +
        scale_fill_manual(values = col_vals, na.value = "grey80") +
        scale_colour_manual(values = col_vals, na.value = "grey80")

      return(list(
        "umapInteractive" = umapInteractive,
        "umapStatic" = umapStatic
      ))
    }, error = function(e) {
      cat("ERROR :", conditionMessage(e), "\n")
    })
  }
)

output$umapInteractive <- renderPlotly({
  umapReactive()$umapInteractive |>
    layout(legend = list(
      font = list(family = "Arial", size = input$textSizeUMAP),
      title = list(
        font = list(family = "Arial", size = input$textSizeUMAP + 2)
      )
    ))
})

output$umapStatic <- renderPlot(
  { umapReactive()$umapStatic },
  height = function() input$figHeightUMAP,
  width = function() input$figWidthUMAP
)
