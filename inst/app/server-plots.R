# server-plots.R
# Feature plot rendering and dynamic settings UI (SCE-native, no Seurat)

# Feature Plot Inputs ----
observeEvent({
  input$fpColumnToPlot
}, {
  fpColumnToPlot <- if (input$fpColumnToPlot == "None" ||
                        is.null(input$fpColumnToPlot)) NULL else input$fpColumnToPlot

  output$plotByBucket <- renderUI({
    sortable::bucket_list(
      header = "Drag and drop groups in order to be plotted",
      group_name = "bucket_list_group1",
      orientation = "horizontal",
      sortable::add_rank_list(
        text = "Include these groups",
        labels = as.list(levels(as.factor(inputDataReactive$Results[["sce"]][[fpColumnToPlot]]))),
        input_id = "plotByKeepBucket"
      ),
      sortable::add_rank_list(
        text = "Exclude these groups",
        labels = NULL,
        input_id = "plotByExcludeBucket"
      )
    )
  })
  outputOptions(output, "plotByBucket", suspendWhenHidden = FALSE)
}, suspended = FALSE)

observeEvent({
  input$fpColumnToSplit
}, {
  fpColumnToSplit <- if (input$fpColumnToSplit == "None" ||
                         is.null(input$fpColumnToSplit)) NULL else input$fpColumnToSplit

  if (!is.null(fpColumnToSplit)) {
    output$splitByBucket <- renderUI({
      sortable::bucket_list(
        header = "Drag and drop groups in order to be plotted",
        group_name = "bucket_list_group2",
        orientation = "horizontal",
        sortable::add_rank_list(
          text = "Include these groups",
          labels = as.list(levels(as.factor(inputDataReactive$Results[["sce"]][[fpColumnToSplit]]))),
          input_id = "splitByKeepBucket"
        ),
        sortable::add_rank_list(
          text = "Exclude these groups",
          labels = NULL,
          input_id = "splitByExcludeBucket"
        )
      )
    })
    outputOptions(output, "splitByBucket", suspendWhenHidden = FALSE)
  } else {
    output$splitByBucket <- renderText({
      "Select a variable to split the plots by first!"
    })
  }
}, suspended = FALSE)

# Dynamic settings per plot type ----
observeEvent(
  { input$featurePlotType },
  ignoreNULL = FALSE,
  {
    # Feature Plot / Nebulosa: DR settings
    if (input$featurePlotType %in% c("Feature Plot", "Nebulosa Plot")) {
      output$umapFeaturePlotSettingsUI0 <- renderUI({
        dr_names <- names(inputDataReactive$Results$umapDFList)
        selectInput(
          inputId = "fpDRToPlot", label = "DR to plot",
          choices = dr_names,
          selected = grep("Downsampled", dr_names, value = TRUE)[1],
          multiple = FALSE, width = "85%"
        )
      })
      output$umapFeaturePlotSettingsUI1 <- renderUI({
        checkboxInput(inputId = "fpShowAxes", label = "Show plot axes?", value = FALSE)
      })
      output$umapFeaturePlotSettingsUI2 <- renderUI({
        checkboxInput(inputId = "fpShowLabels", label = "Show cluster labels?", value = FALSE)
      })
      output$umapFeaturePlotSettingsUI3 <- renderUI({
        checkboxInput(inputId = "cellBordersFP", label = "Show cell border?", value = TRUE)
      })
      splitLayout(
        output$umapFeaturePlotSettingsUI4 <- renderUI({
          sliderInput(
            inputId = "pointSizeFP", label = "Dot Size",
            min = 0.1, max = 4, value = 0.8, step = 0.1,
            width = "85%", ticks = FALSE
          )
        }),
        output$umapFeaturePlotSettingsUI5 <- renderUI({
          sliderInput(
            inputId = "borderSizeFP", label = "Dot border size",
            min = 0, max = 10, value = 0, step = 1,
            width = "85%", ticks = FALSE
          )
        })
      )
      output$umapFeaturePlotSettingsUI6 <- renderUI({
        radioButtons(
          inputId = "fpLabelColour", label = "Colour cluster labels by:",
          choiceNames = c("Label colour", "Gene median", "Gene mean"),
          choiceValues = c("label", "median", "mean")
        )
      })
      output$umapFeaturePlotSettingsUI10 <- renderUI({
        checkboxInput(inputId = "rasteriseFP", label = "Rasterise?", value = FALSE)
      })
      output$umapFeaturePlotSettingsUI11 <- renderUI({
        numericInput(
          inputId = "rasterFP_DPI", label = "Raster DPI",
          value = 1024, min = 0, max = 2000, step = 5, width = "85%"
        )
      })
    } else {
      lapply(c(0, 1:6, 10, 11), function(i) {
        output[[paste0("umapFeaturePlotSettingsUI", i)]] <- renderUI(NULL)
      })
    }

    # Feature Plot specific: custom min/max
    if (input$featurePlotType == "Feature Plot") {
      output$umapFeaturePlotSettingsUI7 <- renderUI({
        checkboxInput(inputId = "fpDRCustomMinMax", label = "Use custom min/max values?", value = FALSE)
      })
      splitLayout(
        output$umapFeaturePlotSettingsUI8 <- renderUI({
          numericInput(
            inputId = "fpDRCustomMin", label = "Min",
            value = 0, min = -Inf, max = Inf, step = 0.5, width = "66%"
          )
        }),
        output$umapFeaturePlotSettingsUI9 <- renderUI({
          numericInput(
            inputId = "fpDRCustomMax", label = "Max",
            value = 6, min = -Inf, max = Inf, step = 0.5, width = "66%"
          )
        })
      )
    } else {
      lapply(7:9, function(i) {
        output[[paste0("umapFeaturePlotSettingsUI", i)]] <- renderUI(NULL)
      })
    }

    # Nebulosa: joint plot
    if (input$featurePlotType == "Nebulosa Plot") {
      output$fpNebulosaOutputUI1 <- renderUI({
        checkboxInput(inputId = "fpNebulosaPlotTogether", label = "Show joint plot?", value = TRUE)
      })
      output$fpNebulosaOutputUI2 <- renderUI({
        checkboxInput(inputId = "fpNebulosaPlotTogetherOnly", label = "Show only joint plot?", value = FALSE)
      })
    } else {
      output$fpNebulosaOutputUI1 <- NULL
      output$fpNebulosaOutputUI2 <- NULL
    }

    # Dot Plot: flip
    if (input$featurePlotType == "Dot Plot") {
      output$umapFeaturePlotDotPlotUI2 <- renderUI({
        checkboxInput(inputId = "umapFeaturePlotDotplotFlip", label = "Flip dot plot?", value = TRUE)
      })
      outputOptions(output, "umapFeaturePlotDotPlotUI2", suspendWhenHidden = FALSE)
    } else {
      output$umapFeaturePlotDotPlotUI1 <- renderUI(NULL)
      output$umapFeaturePlotDotPlotUI2 <- renderUI(NULL)
    }

    # Heatmap settings
    if (input$featurePlotType == "Heatmap") {
      output$umapFeaturePlotHeatmapUI1 <- renderUI({
        checkboxInput(inputId = "umapFeaturePlotHeatmapCluster", label = "Cluster heatmap?", value = TRUE)
      })
      output$umapFeaturePlotHeatmapUI2 <- renderUI({
        checkboxInput(inputId = "umapFeaturePlotHeatmapFlip", label = "Flip heatmap?", value = TRUE)
      })
      output$umapFeaturePlotWarningUI <- renderUI(NULL)
    } else {
      output$umapFeaturePlotHeatmapUI1 <- renderUI(NULL)
      output$umapFeaturePlotHeatmapUI2 <- renderUI(NULL)
    }

    # Heatmap / Individual Heatmap / Dot Plot: "Plot all features"
    if (input$featurePlotType %in% c("Heatmap", "Individual Heatmap", "Dot Plot")) {
      output$fpHeatmapOutputUI1 <- renderUI({
        checkboxInput(inputId = "fpHeatmapPlotAll", label = "Plot all available features?", value = FALSE)
      })
      outputOptions(output, "fpHeatmapOutputUI1", suspendWhenHidden = FALSE)
    } else {
      output$fpHeatmapOutputUI1 <- renderUI(NULL)
    }

    # Barplot settings
    if (input$featurePlotType == "Barplot") {
      output$fpBarplotOptionsUI1 <- renderUI({
        checkboxInput(
          inputId = "fpBarplotPercentage",
          label = "Make barplot fractional?", value = FALSE
        )
      })
      output$fpBarplotOptionsUI2 <- renderUI({
        checkboxInput(
          inputId = "fpBarplotShowNumbers",
          label = "Show numbers?", value = FALSE
        )
      })
      output$fpBarplotOutputUI3 <- renderUI({
        downloadButton(
          outputId = "dlBarplotCounts",
          label = "Download Barplot Counts"
        )
      })
    } else {
      output$fpBarplotOutputUI1 <- NULL
      output$fpBarplotOutputUI2 <- NULL
      output$fpBarplotOptionsUI1 <- NULL
      output$fpBarplotOptionsUI2 <- NULL
      output$fpBarplotOutputUI3 <- NULL
    }
  }
)

# "Plot all features" toggle logic
previousFeatureSelection <- reactiveVal(NULL)
observeEvent({
  input$fpHeatmapPlotAll
  input$featurePlotType
}, ignoreNULL = TRUE, {
  req(!is.null(input$featurePlotType))
  req(!is.null(input$fpHeatmapPlotAll))
  if (input$featurePlotType %in% c("Heatmap", "Individual Heatmap", "Dot Plot")) {
    if (input$fpHeatmapPlotAll) {
      previousFeatureSelection(input$fpFeatureToPlot)
      updateSelectInput(
        session = session, inputId = "fpFeatureToPlot",
        selected = names(inputDataReactive$Results$sce)
      )
    } else {
      updateSelectInput(
        session = session, inputId = "fpFeatureToPlot",
        selected = previousFeatureSelection()
      )
    }
  } else {
    updateSelectInput(
      session = session, inputId = "fpFeatureToPlot",
      selected = previousFeatureSelection()
    )
  }
})

# Feature plots ---- (SCE-native, no Seurat)
featurePlotReactive <- reactiveValues(fp = NULL)
observeEvent(input$featurePlotType, {
  featurePlotReactive <- reactiveValues(fp = NULL)
})

observeEvent(
  {
    input$fpDRToPlot
    input$fpFeatureToPlot
    input$featurePlotType
    input$fpAssayToPlot
    input$fpColumnToPlot
    input$fpColumnToSplit
    input$pointSizeFP
    input$textSizeFP
    input$ncolFPGene
    input$ncolFPSplit
    input$fpShowLabels
    input$viridisColourFP
    input$flipViridisFP
    input$umapFeaturePlotHeatmapCluster
    input$umapFeaturePlotHeatmapFlip
    input$fpLegendPosition
    input$cellBordersFP
    input$borderSizeFP
    input$fpShowAxes
    input$umapFeaturePlotDotplotFlip
    input$fpBarplotPercentage
    input$fpBarplotShowNumbers
    input$plotByKeepBucket
    input$splitByKeepBucket
    input$fpLabelColour
    input$fpNebulosaPlotTogether
    input$fpNebulosaPlotTogetherOnly
    input$rasteriseFP
    input$rasterFP_DPI
    input$fpContrastToUse
    input$fpShowDAClusters
    input$fpHeatmapPlotAll
    input$fpSubsetCells
    input$fpSubsetToGlobal
    lapply(names(colsList1), function(col) {
      lapply(names(colsList1[[col]]), function(lor) {
        input[[paste0("GroupColour", col, lor)]]
      })
    })
  },
  ignoreNULL = FALSE,
  {
    tryCatch({
      sce <- inputDataReactive$Results$sce

      # Determine assay to use for expression data
      assayMap <- c(
        "data" = "exprsQuantNorm",
        "counts" = "exprsTransformed",
        "scale.data" = "exprsScaled"
      )
      assayToUse <- assayMap[[input$fpAssayToPlot]]
      if (is.null(assayToUse)) assayToUse <- "exprsQuantNorm"

      viridisFlip <- if (isTRUE(input$flipViridisFP)) -1 else 1

      fpColumnToPlot <- if (input$fpColumnToPlot == "None" ||
                            is.null(input$fpColumnToPlot)) NULL else input$fpColumnToPlot
      fpColumnToSplit <- if (input$fpColumnToSplit == "None" ||
                             is.null(input$fpColumnToSplit)) NULL else input$fpColumnToSplit

      # Get the working umapDF for feature scatter and barplot
      drName <- if (!is.null(input$fpDRToPlot)) input$fpDRToPlot else "Downsampled"
      umapDF <- inputDataReactive$Results$umapDFList[[drName]]
      if (is.null(umapDF)) umapDF <- inputDataReactive$Results$umapDFList$Downsampled

      # Apply subset if active
      if (isTRUE(input$fpSubsetCells) &&
            !is.null(inputDataReactive$Results[["subsetCellIds"]])) {
        subsetIds <- inputDataReactive$Results[["subsetCellIds"]]
        # Subset the SCE
        if (!is.null(sce) && length(subsetIds) > 0) {
          sce <- sce[, colnames(sce) %in% subsetIds]
        }
      }

      # Apply bucket filtering
      if (!is.null(fpColumnToPlot) && !is.null(input$plotByKeepBucket)) {
        if (!is.null(umapDF)) {
          umapDF <- umapDF[umapDF[[fpColumnToPlot]] %in% input$plotByKeepBucket, ]
          umapDF[[fpColumnToPlot]] <- factor(umapDF[[fpColumnToPlot]], levels = input$plotByKeepBucket)
        }
        if (!is.null(sce)) {
          keep_cells <- colnames(sce)[sce[[fpColumnToPlot]] %in% input$plotByKeepBucket]
          if (length(keep_cells) > 0) sce <- sce[, keep_cells]
          sce[[fpColumnToPlot]] <- factor(sce[[fpColumnToPlot]], levels = input$plotByKeepBucket)
        }
      }
      if (!is.null(fpColumnToSplit) && !is.null(input$splitByKeepBucket)) {
        if (!is.null(umapDF)) {
          umapDF <- umapDF[umapDF[[fpColumnToSplit]] %in% input$splitByKeepBucket, ]
          umapDF[[fpColumnToSplit]] <- factor(umapDF[[fpColumnToSplit]], levels = input$splitByKeepBucket)
        }
        if (!is.null(sce)) {
          keep_cells <- colnames(sce)[sce[[fpColumnToSplit]] %in% input$splitByKeepBucket]
          if (length(keep_cells) > 0) sce <- sce[, keep_cells]
        }
      }

      # DA cluster filtering
      contrastToUse <- grep(
        input$fpContrastToUse,
        inputDataReactive$Results$smd$`Conditions To Test`
      )
      contrastIndexes <- seq(1, 11, by = 2)[contrastToUse]
      clustersToPlot <- inputDataReactive$Results$selectedClustersList[
        c(contrastIndexes, contrastIndexes + 1)
      ]
      if (input$fpShowDAClusters != "None" && !is.null(sce)) {
        da_clusters <- switch(input$fpShowDAClusters,
          "All" = as.character(unlist(clustersToPlot)),
          "Up only" = clustersToPlot[[1]],
          "Down only" = clustersToPlot[[2]]
        )
        if (length(da_clusters) > 1) {
          keep_cells <- colnames(sce)[
            as.character(sce$cluster_id) %in% da_clusters
          ]
          if (length(keep_cells) > 1) sce <- sce[, keep_cells]
        } else {
          showNotification("There are no DA clusters in this contrast!", type = "error")
        }
      }

      fpFeaturesToPlot <- gsub("_", "-", input$fpFeatureToPlot)

      # ====== FEATURE PLOT ======
      if (input$featurePlotType == "Feature Plot") {
        fp <- lapply(fpFeaturesToPlot, function(marker) {
          df <- umapDF[order(umapDF[[marker]], decreasing = FALSE), ]

          median_pos <- compute_label_positions(df, input$fpColumnToPlot, marker)

          fp2 <- make_feature_scatter(
            df = df, marker = marker,
            palette = input$viridisColourFP, direction = viridisFlip,
            point_size = input$pointSizeFP,
            rasterise = isTRUE(input$rasteriseFP), raster_dpi = input$rasterFP_DPI %||% 1024,
            border = isTRUE(input$cellBordersFP), border_size = input$borderSizeFP %||% 0,
            base_size = input$textSizeFP, show_axes = isTRUE(input$fpShowAxes),
            legend_position = tolower(input$fpLegendPosition)
          )

          if (!is.null(fpColumnToSplit)) {
            fp2 <- add_facet_with_counts(fp2, df, fpColumnToSplit, input$ncolFPSplit)
          }

          if (isTRUE(input$fpShowLabels)) {
            fp2 <- fp2 + ggnewscale::new_scale_color() + ggnewscale::new_scale_fill()
            if (input$fpLabelColour == "label") {
              fp2 <- fp2 +
                ggrepel::geom_label_repel(
                  data = median_pos,
                  aes(
                    label = .data[[input$fpColumnToPlot]],
                    x = .data[["x"]], y = .data[["y"]],
                    fill = .data[[input$fpColumnToPlot]]
                  ),
                  show.legend = FALSE,
                  size = input$textSizeFP / 4,
                  max.overlaps = 100
                ) +
                scale_fill_manual(
                  values = inputDataReactive$Results$coloursList[[input$fpColumnToPlot]]
                )
            } else {
              fp2 <- fp2 +
                ggrepel::geom_label_repel(
                  data = median_pos,
                  aes(
                    label = .data[[input$fpColumnToPlot]],
                    x = .data[["x"]], y = .data[["y"]],
                    fill = .data[[input$fpLabelColour]]
                  ),
                  show.legend = FALSE,
                  size = input$textSizeFP / 4,
                  max.overlaps = 100
                )
              fp2 <- apply_continuous_scale(fp2, input$viridisColourFP, viridisFlip, "fill")
            }
          }
          fp2
        })

      # ====== NEBULOSA PLOT ======
      } else if (input$featurePlotType == "Nebulosa Plot") {
        if (!requireNamespace("Nebulosa", quietly = TRUE)) {
          showNotification("Nebulosa is not installed.", type = "error")
          return(NULL)
        }
        # Nebulosa works directly with SCE
        nebFeatures <- fpFeaturesToPlot
        joint <- isTRUE(input$fpNebulosaPlotTogether)
        return_only_joint <- isTRUE(input$fpNebulosaPlotTogetherOnly)
        combine <- length(nebFeatures) > 1
        if (length(nebFeatures) == 1) {
          joint <- FALSE
          return_only_joint <- FALSE
          combine <- FALSE
        }

        dr_choice <- if (!is.null(input$fpDRToPlot) &&
          grepl("UMAP|TSNE|PCA|pacmap", input$fpDRToPlot, ignore.case = TRUE)) {
          input$fpDRToPlot
        } else {
          "UMAP"
        }

        fp_raw <- Nebulosa::plot_density(
          sce, features = nebFeatures,
          slot = assayToUse,
          reduction = dr_choice,
          joint = joint, combine = combine
        )

        # Wrap into list of ggplot objects
        if (!is.list(fp_raw)) fp_raw <- list(fp_raw)

        # Apply MARMOT theme to each panel
        fp <- lapply(fp_raw, function(p) {
          p + marmot_dr_theme(
            base_size = input$textSizeFP,
            show_axes = isTRUE(input$fpShowAxes),
            legend_position = tolower(input$fpLegendPosition)
          )
        })

        if (return_only_joint && length(fp) > 1) {
          fp <- list(fp[[length(fp)]])  # Last panel is the joint plot
        }

      # ====== VIOLIN PLOT ======
      } else if (input$featurePlotType == "Violin Plot") {
        # Extract expression from SCE
        colsToViolin <- if (!is.null(fpColumnToSplit)) fpColumnToSplit else fpColumnToPlot

        fp <- lapply(fpFeaturesToPlot, function(marker) {
          # Get expression for this marker
          marker_idx <- match(marker, rownames(sce))
          if (is.na(marker_idx)) return(NULL)

          expr_vals <- SummarizedExperiment::assay(sce, assayToUse)[marker_idx, ]
          cd <- as.data.frame(SummarizedExperiment::colData(sce))
          cd[[marker]] <- as.numeric(expr_vals)

          make_violin_plot(
            df = cd, marker = marker, group_col = fpColumnToPlot,
            split_col = fpColumnToSplit,
            colours = inputDataReactive$Results$coloursList[[colsToViolin]],
            point_size = input$pointSizeFP %||% 0, base_size = input$textSizeFP
          )
        })
        fp <- Filter(Negate(is.null), fp)

      # ====== INDIVIDUAL HEATMAP (per-cell) ======
      } else if (input$featurePlotType == "Individual Heatmap") {
        # Build expression matrix from SCE
        marker_idx <- match(fpFeaturesToPlot, rownames(sce))
        marker_idx <- marker_idx[!is.na(marker_idx)]
        if (length(marker_idx) > 0) {
          expr_mat <- as.matrix(SummarizedExperiment::assay(sce, assayToUse)[marker_idx, , drop = FALSE])
          group_ids <- factor(sce[[fpColumnToPlot]])

          fp <- make_percell_heatmap(
            expr_mat = expr_mat, group_ids = group_ids,
            group_colours = inputDataReactive$Results$coloursList[[fpColumnToPlot]],
            palette = input$viridisColourFP, direction = viridisFlip
          )
        } else {
          fp <- NULL
        }

      # ====== DOT PLOT ======
      } else if (input$featurePlotType == "Dot Plot") {
        # Aggregate expression per group
        marker_idx <- match(fpFeaturesToPlot, rownames(sce))
        marker_idx <- marker_idx[!is.na(marker_idx)]
        if (length(marker_idx) > 0) {
          expr_mat <- as.matrix(SummarizedExperiment::assay(sce, assayToUse)[marker_idx, , drop = FALSE])
          cd <- as.data.frame(SummarizedExperiment::colData(sce))

          # Build per-cell df
          expr_df <- as.data.frame(t(expr_mat))
          expr_df[[fpColumnToPlot]] <- cd[[fpColumnToPlot]]

          markers_in_df <- fpFeaturesToPlot[
            fpFeaturesToPlot %in% colnames(expr_df)
          ]
          agg <- aggregate_expression(
            expr_df, markers_in_df, fpColumnToPlot
          )

          fp <- make_dot_plot(
            avg_expr = agg$avg_expr, pct_expr = agg$pct_expr,
            palette = input$viridisColourFP, direction = viridisFlip,
            dot_scale = (input$pointSizeFP %||% 1) * 5,
            flip = isTRUE(input$umapFeaturePlotDotplotFlip),
            base_size = input$textSizeFP
          )
        } else {
          fp <- NULL
        }

      # ====== EXPRESSION HEATMAP (aggregated) ======
      } else if (input$featurePlotType == "Heatmap") {
        # Use MARMOT's SCE-native plotExprHeatmap
        fp <- tryCatch({
          MARMOT::plotExprHeatmap(
            x = sce,
            features = fpFeaturesToPlot,
            by = "cluster_id",
            assay = switch(input$fpAssayToPlot,
              "data" = "exprs",
              "counts" = "counts",
              "scale.data" = "exprs"
            ),
            scale = if (input$fpAssayToPlot == "scale.data") "last" else "never",
            row_clust = isTRUE(input$umapFeaturePlotHeatmapCluster),
            col_clust = isTRUE(input$umapFeaturePlotHeatmapCluster)
          )
        }, error = function(e) {
          cat("Heatmap error:", e$message, "\n")
          NULL
        })

      # ====== RIDGE PLOT ======
      } else if (input$featurePlotType == "Ridge Plot") {
        fp <- lapply(fpFeaturesToPlot, function(marker) {
          marker_idx <- match(marker, rownames(sce))
          if (is.na(marker_idx)) return(NULL)

          expr_vals <- SummarizedExperiment::assay(sce, assayToUse)[marker_idx, ]
          cd <- as.data.frame(SummarizedExperiment::colData(sce))
          cd[[marker]] <- as.numeric(expr_vals)

          make_ridge_plot(
            df = cd, marker = marker, group_col = fpColumnToPlot,
            colours = inputDataReactive$Results$coloursList[[input$fpColumnToPlot]],
            base_size = input$textSizeFP
          )
        })
        fp <- Filter(Negate(is.null), fp)

      # ====== BARPLOT ======
      } else if (input$featurePlotType == "Barplot") {
        umapDFAll <- inputDataReactive$Results$umapDFList$All
        x_col <- if (is.null(fpColumnToSplit)) fpColumnToPlot else fpColumnToSplit

        fp <- make_barplot(
          df = umapDFAll, x_col = x_col, fill_col = fpColumnToPlot,
          colours = inputDataReactive$Results$coloursList[[input$fpColumnToPlot]],
          fractional = isTRUE(input$fpBarplotPercentage),
          show_numbers = isTRUE(input$fpBarplotShowNumbers),
          base_size = input$textSizeFP
        )

        # Create counts table
        if (is.null(fpColumnToSplit)) {
          dfX <- as.data.frame(table(umapDFAll[[fpColumnToPlot]]))
          colnames(dfX) <- c(fpColumnToPlot, "Count")
        } else {
          dfX <- as.data.frame(table(umapDFAll[[fpColumnToPlot]], umapDFAll[[fpColumnToSplit]]))
          dfX <- tidyr::spread(dfX, key = Var2, value = Freq)
          colnames(dfX)[[1]] <- fpColumnToPlot
        }
        output$fpBarplotOutputUI2 <- renderUI({
          output$fpBarplotTable <- DT::renderDataTable(dfX, rownames = FALSE)
          DT::dataTableOutput("fpBarplotTable", fill = FALSE)
        })
        output$dlBarplotCounts <- downloadHandler(
          filename = function() {
            paste(fpColumnToPlot, "_barplot_counts.xlsx")
          },
          content = function(file) {
            openxlsx::write.xlsx(x = as.data.frame(dfX), file = file)
          }
        )
      }

      featurePlotReactive$fp <- fp
    }, error = function(e) {
      cat("ERROR :", conditionMessage(e), "\n")
    })
  }
)

output$featurePlotOutput <- renderPlot(
  {
    req(!is.null(input$fpFeatureToPlot), length(input$fpFeatureToPlot) > 0)
    fp <- featurePlotReactive$fp
    if (is.null(fp)) return(NULL)

    # ComplexHeatmap objects need draw()
    if (inherits(fp, "Heatmap") || inherits(fp, "HeatmapList")) {
      ComplexHeatmap::draw(fp)
    } else if (input$featurePlotType %in%
               c("Feature Plot", "Nebulosa Plot", "Ridge Plot", "Violin Plot") &&
               is.list(fp) && length(fp) >= 2) {
      gridExtra::grid.arrange(grobs = fp, ncol = input$ncolFPGene)
    } else if (is.list(fp) && length(fp) == 1) {
      fp[[1]]
    } else {
      fp
    }
  },
  height = function() input$figHeightFP,
  width = function() input$figWidthFP
)
