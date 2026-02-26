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
        # Default DR priority: PaCMAP > UMAP > TSNE > first available
        fp_dr_default <- dr_names[1]
        for (pref in c("TSNE", "UMAP", "PaCMAP")) {
          hit <- grep(pref, dr_names, ignore.case = TRUE, value = TRUE)
          # Prefer Downsampled variants
          ds_hit <- grep("Downsampled", hit, value = TRUE)
          if (length(ds_hit) > 0) hit <- ds_hit
          if (length(hit) > 0) fp_dr_default <- hit[1]
        }
        selectInput(
          inputId = "fpDRToPlot", label = "DR to plot",
          choices = dr_names,
          selected = fp_dr_default,
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
            min = 0.1, max = 4, value = 0.2, step = 0.1,
            width = "85%", ticks = FALSE
          )
        }),
        output$umapFeaturePlotSettingsUI5 <- renderUI({
          sliderInput(
            inputId = "borderSizeFP", label = "Border size",
            min = 1, max = 5, value = 3, step = 0.1,
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
      # Auto-enable rasterisation for large datasets
      default_rasterise <- isTRUE(inputDataReactive$Results$rasterise_auto)
      output$umapFeaturePlotSettingsUI10 <- renderUI({
        checkboxInput(inputId = "rasteriseFP", label = "Rasterise?", value = default_rasterise)
      })
      output$umapFeaturePlotSettingsUI11 <- renderUI({
        numericInput(
          inputId = "rasterFP_DPI", label = "Raster DPI",
          value = 1024, min = 0, max = 2000, step = 5, width = "85%"
        )
      })
      output$umapFeaturePlotSettingsUI12 <- renderUI({
        sliderInput(
          inputId = "borderDensityFP", label = "Border density",
          min = 0.05, max = 2, value = 1, step = 0.05,
          width = "85%", ticks = FALSE
        )
      })
    } else {
      lapply(c(0, 1:6, 10, 11, 12), function(i) {
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

# Adaptive debounce timing for feature plots ----
feature_debounce_ms <- reactive({
  ncell <- inputDataReactive$Results$ncell %||% 0L
  if (ncell > 200000L) 400L else if (ncell > 50000L) 200L else 100L
})

# Capture all feature plot inputs into a single reactive for debouncing
fp_inputs_raw <- reactive({
  list(
    input$fpDRToPlot,
    input$fpFeatureToPlot,
    input$featurePlotType,
    input$fpAssayToPlot,
    input$fpColumnToPlot,
    input$fpColumnToSplit,
    input$pointSizeFP,
    input$textSizeFP,
    input$ncolFPGene,
    input$ncolFPSplit,
    input$fpShowLabels,
    input$viridisColourFP,
    input$flipViridisFP,
    input$umapFeaturePlotHeatmapCluster,
    input$umapFeaturePlotHeatmapFlip,
    input$fpLegendPosition,
    input$cellBordersFP,
    input$borderSizeFP,
    input$borderDensityFP,
    input$fpShowAxes,
    input$umapFeaturePlotDotplotFlip,
    input$fpBarplotPercentage,
    input$fpBarplotShowNumbers,
    input$plotByKeepBucket,
    input$splitByKeepBucket,
    input$fpLabelColour,
    input$fpNebulosaPlotTogether,
    input$fpNebulosaPlotTogetherOnly,
    input$rasteriseFP,
    input$rasterFP_DPI,
    input$fpContrastToUse,
    input$fpShowDAClusters,
    input$fpHeatmapPlotAll,
    input$fpSubsetMode,
    input$fpSubsetToGlobal,
    clusterTableReactive$table,
    lapply(names(colsList1), function(col) {
      lapply(names(colsList1[[col]]), function(lor) {
        input[[paste0("GroupColour", col, lor)]]
      })
    })
  )
})

fp_inputs <- fp_inputs_raw |> debounce(feature_debounce_ms)

# Feature plots ---- (SCE-native, no Seurat)
featurePlotReactive <- reactiveValues(fp = NULL)
observeEvent(input$featurePlotType, {
  featurePlotReactive$fp <- NULL
})

observeEvent(
  fp_inputs(),
  ignoreNULL = FALSE,
  {
    tryCatch({
      req(
        !is.null(inputDataReactive$Results$sce),
        !is.null(inputDataReactive$Results$umapDFList),
        length(input$fpFeatureToPlot) > 0 ||
          input$featurePlotType %in% c("Barplot", "Heatmap")
      )
      sce <- inputDataReactive$Results$sce

      # Determine assay to use for expression data
      assayMap <- c(
        "data" = "exprsQuantNorm",
        "counts" = "exprsTransformed",
        "scale.data" = "norm"
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
      req(!is.null(umapDF))

      # Apply subset if active
      if (!is.null(input$fpSubsetMode) && input$fpSubsetMode != "None" &&
            !is.null(inputDataReactive$Results[["subsetCellIds"]])) {
        subsetIds <- inputDataReactive$Results[["subsetCellIds"]]
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
      contrasts_vec <- inputDataReactive$Results$smd$`Conditions To Test`
      contrasts_vec <- contrasts_vec[!is.na(contrasts_vec)]
      n_contrasts <- length(contrasts_vec)
      contrastToUse <- grep(input$fpContrastToUse, contrasts_vec)
      if (length(contrastToUse) == 0) contrastToUse <- 1L
      contrastIndexes <- seq(1, max(1, 2 * n_contrasts - 1), by = 2)[contrastToUse]
      clustersToPlot <- inputDataReactive$Results$selectedClustersList[
        c(contrastIndexes, contrastIndexes + 1)
      ]
      if (input$fpShowDAClusters != "None" && !is.null(sce)) {
        da_clusters <- switch(input$fpShowDAClusters,
          "All" = as.character(unlist(clustersToPlot)),
          "Up only" = clustersToPlot[[1]],
          "Down only" = clustersToPlot[[2]]
        )
        if (length(da_clusters) > 0) {
          keep_cells <- colnames(sce)[
            as.character(sce$cluster_id) %in% da_clusters
          ]
          if (length(keep_cells) > 0) sce <- sce[, keep_cells]
        } else {
          showNotification("There are no DA clusters in this contrast!", type = "error")
        }
      }

      fpFeaturesToPlot <- gsub("_", "-", input$fpFeatureToPlot)

      # ── Shared batch extraction: pull all expression at once (used by Feature Plot + Nebulosa) ──
      orig_sce <- inputDataReactive$Results$sce
      expr_batch <- NULL

      if (input$featurePlotType %in% c("Feature Plot", "Nebulosa Plot") &&
            length(fpFeaturesToPlot) > 0) {
        if (assayToUse %in% SummarizedExperiment::assayNames(orig_sce)) {
          marker_idx <- match(fpFeaturesToPlot, rownames(orig_sce))
          na_pos <- is.na(marker_idx)
          if (any(na_pos)) {
            marker_idx[na_pos] <- match(gsub("-", "_", fpFeaturesToPlot[na_pos]), rownames(orig_sce))
          }
          valid_idx <- which(!is.na(marker_idx))
          valid_markers <- fpFeaturesToPlot[valid_idx]
          valid_row_idx <- marker_idx[valid_idx]
        } else {
          valid_markers <- character(0)
          valid_row_idx <- integer(0)
        }

        # Pre-extract all expression rows at once (one matrix read, not N reads)
        if (length(valid_row_idx) > 0 && "sce_idx" %in% colnames(umapDF)) {
          expr_batch <- SummarizedExperiment::assay(
            orig_sce, assayToUse)[valid_row_idx, umapDF$sce_idx, drop = FALSE]
          rownames(expr_batch) <- valid_markers
        } else if (length(valid_row_idx) > 0) {
          n_common <- min(ncol(sce), nrow(umapDF))
          if (n_common == nrow(umapDF)) {
            expr_batch <- SummarizedExperiment::assay(
              sce, assayToUse)[valid_row_idx, seq_len(n_common), drop = FALSE]
            rownames(expr_batch) <- valid_markers
          }
        }
      }

      # ====== FEATURE PLOT — factory pattern (batch extraction) ======
      if (input$featurePlotType == "Feature Plot") {
        # ── Loop only over ggplot construction (fast path) ──
        fp <- lapply(fpFeaturesToPlot, function(marker) {
          df <- umapDF

          # Attach expression from pre-extracted batch matrix
          if (!is.null(expr_batch) && marker %in% rownames(expr_batch)) {
            df[[marker]] <- as.numeric(expr_batch[marker, ])
          }
          if (!marker %in% colnames(df)) return(NULL)
          df <- df[order(df[[marker]], decreasing = FALSE), ]

          median_pos <- compute_label_positions(df, input$fpColumnToPlot, marker)

          fp2 <- make_feature_scatter(
            df = df, marker = marker,
            palette = input$viridisColourFP, direction = viridisFlip,
            point_size = input$pointSizeFP, alpha = 0.6,
            rasterise = isTRUE(input$rasteriseFP), raster_dpi = input$rasterFP_DPI %||% 1024,
            border = isTRUE(input$cellBordersFP),
            border_size    = input$borderSizeFP    %||% 2.0,
            border_density = input$borderDensityFP %||% 1,
            border_colour  = "black",
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

      # ====== NEBULOSA PLOT (ks::kde weighted scatter) ======
      } else if (input$featurePlotType == "Nebulosa Plot") {
        if (!requireNamespace("ks", quietly = TRUE)) {
          showNotification("ks package is not installed. Install with install.packages('ks').",
                           type = "error")
          return(NULL)
        }

        # DR coordinates from umapDF (already have x, y columns)
        emb_mat <- as.matrix(umapDF[, c("x", "y")])

        joint <- isTRUE(input$fpNebulosaPlotTogether) && length(fpFeaturesToPlot) > 1
        return_only_joint <- isTRUE(input$fpNebulosaPlotTogetherOnly)

        # Per-gene weighted density scatter
        fp <- lapply(fpFeaturesToPlot, function(marker) {
          df <- umapDF

          # Attach expression from shared batch extraction
          if (!is.null(expr_batch) && marker %in% rownames(expr_batch)) {
            df[[marker]] <- as.numeric(expr_batch[marker, ])
          }
          if (!marker %in% colnames(df)) return(NULL)

          w <- df[[marker]]
          w[is.na(w)] <- 0
          if (sum(w) == 0) {
            df$density <- 0
          } else {
            w_norm <- w / sum(w) * length(w)
            dens <- ks::kde(emb_mat, w = w_norm, eval.points = emb_mat)
            df$density <- dens$estimate
          }
          df <- df[order(df$density), ]  # high density on top

          fp2 <- make_feature_scatter(
            df = df, marker = "density",
            palette = input$viridisColourFP, direction = viridisFlip,
            point_size = input$pointSizeFP, alpha = 0.7,
            rasterise = isTRUE(input$rasteriseFP), raster_dpi = input$rasterFP_DPI %||% 1024,
            border = isTRUE(input$cellBordersFP),
            border_size    = input$borderSizeFP    %||% 2.0,
            border_density = input$borderDensityFP %||% 1,
            border_colour  = "black",
            base_size = input$textSizeFP, show_axes = isTRUE(input$fpShowAxes),
            legend_position = tolower(input$fpLegendPosition)
          ) + ggtitle(paste0(marker, " density")) + labs(colour = "Density")

          # Cluster labels
          if (isTRUE(input$fpShowLabels) && !is.null(fpColumnToPlot)) {
            median_pos <- compute_label_positions(df, fpColumnToPlot)
            fp2 <- fp2 + ggnewscale::new_scale_color() + ggnewscale::new_scale_fill()
            if (input$fpLabelColour == "label") {
              fp2 <- fp2 +
                ggrepel::geom_label_repel(
                  data = median_pos,
                  aes(label = .data[[fpColumnToPlot]],
                      x = .data[["x"]], y = .data[["y"]],
                      fill = .data[[fpColumnToPlot]]),
                  show.legend = FALSE, size = input$textSizeFP / 4,
                  max.overlaps = 100
                ) +
                scale_fill_manual(
                  values = inputDataReactive$Results$coloursList[[fpColumnToPlot]]
                )
            } else {
              median_pos <- compute_label_positions(df, fpColumnToPlot, marker)
              fp2 <- fp2 +
                ggrepel::geom_label_repel(
                  data = median_pos,
                  aes(label = .data[[fpColumnToPlot]],
                      x = .data[["x"]], y = .data[["y"]],
                      fill = .data[[input$fpLabelColour]]),
                  show.legend = FALSE, size = input$textSizeFP / 4,
                  max.overlaps = 100
                )
              fp2 <- apply_continuous_scale(fp2, input$viridisColourFP, viridisFlip, "fill")
            }
          }
          fp2
        })
        fp <- Filter(Negate(is.null), fp)

        # Joint density (sum expression weights across genes, single KDE)
        if (joint && length(fpFeaturesToPlot) > 1) {
          df_joint <- umapDF
          w_joint <- rep(0, nrow(df_joint))
          for (marker in fpFeaturesToPlot) {
            if (!is.null(expr_batch) && marker %in% rownames(expr_batch)) {
              v <- as.numeric(expr_batch[marker, ])
              v[is.na(v)] <- 0
              w_joint <- w_joint + v
            }
          }
          if (sum(w_joint) > 0) {
            w_norm <- w_joint / sum(w_joint) * length(w_joint)
            dens <- ks::kde(emb_mat, w = w_norm, eval.points = emb_mat)
            df_joint$density <- dens$estimate
          } else {
            df_joint$density <- 0
          }
          df_joint <- df_joint[order(df_joint$density), ]

          joint_plot <- make_feature_scatter(
            df = df_joint, marker = "density",
            palette = input$viridisColourFP, direction = viridisFlip,
            point_size = input$pointSizeFP, alpha = 0.7,
            rasterise = isTRUE(input$rasteriseFP), raster_dpi = input$rasterFP_DPI %||% 1024,
            border = isTRUE(input$cellBordersFP),
            border_size    = input$borderSizeFP    %||% 2.0,
            border_density = input$borderDensityFP %||% 1,
            border_colour  = "black",
            base_size = input$textSizeFP, show_axes = isTRUE(input$fpShowAxes),
            legend_position = tolower(input$fpLegendPosition)
          ) + ggtitle(paste0("Joint: ", paste(fpFeaturesToPlot, collapse = " + "))) +
            labs(colour = "Density")

          # Cluster labels on joint plot
          if (isTRUE(input$fpShowLabels) && !is.null(fpColumnToPlot)) {
            median_pos <- compute_label_positions(df_joint, fpColumnToPlot)
            joint_plot <- joint_plot + ggnewscale::new_scale_color() + ggnewscale::new_scale_fill() +
              ggrepel::geom_label_repel(
                data = median_pos,
                aes(label = .data[[fpColumnToPlot]],
                    x = .data[["x"]], y = .data[["y"]],
                    fill = .data[[fpColumnToPlot]]),
                show.legend = FALSE, size = input$textSizeFP / 4,
                max.overlaps = 100
              ) +
              scale_fill_manual(
                values = inputDataReactive$Results$coloursList[[fpColumnToPlot]]
              )
          }

          fp <- c(fp, list(joint_plot))
        }

        if (return_only_joint && length(fp) > 1) {
          fp <- list(fp[[length(fp)]])
        }

      # ====== VIOLIN PLOT ======
      } else if (input$featurePlotType == "Violin Plot") {
        colsToViolin <- if (!is.null(fpColumnToSplit)) fpColumnToSplit else fpColumnToPlot

        fp <- lapply(fpFeaturesToPlot, function(marker) {
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
        # Batch-extract expression matrix for all selected markers at once
        expr_mat <- extract_expr_matrix(sce, assayToUse, fpFeaturesToPlot)
        if (is.null(fpColumnToPlot) || ncol(expr_mat) == 0 || nrow(expr_mat) == 0) {
          fp <- NULL
        } else {
          group_ids <- factor(sce[[fpColumnToPlot]])
          fp <- make_percell_heatmap(
            expr_mat      = expr_mat,
            group_ids     = group_ids,
            group_colours = inputDataReactive$Results$coloursList[[fpColumnToPlot]],
            palette       = input$viridisColourFP,
            direction     = viridisFlip
          )
        }

      # ====== DOT PLOT ======
      } else if (input$featurePlotType == "Dot Plot") {
        # Batch-extract expression matrix for all selected markers at once
        expr_mat <- extract_expr_matrix(sce, assayToUse, fpFeaturesToPlot)
        if (nrow(expr_mat) > 0 && !is.null(fpColumnToPlot)) {
          cd <- as.data.frame(SummarizedExperiment::colData(sce))
          expr_df <- as.data.frame(t(expr_mat))
          expr_df[[fpColumnToPlot]] <- cd[[fpColumnToPlot]]
          markers_in_df <- rownames(expr_mat)[rownames(expr_mat) %in% colnames(expr_df)]
          agg <- aggregate_expression(expr_df, markers_in_df, fpColumnToPlot)

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
        fp <- tryCatch({
          # Ensure cluster_codes exists (missing after Parquet round-trip)
          if (is.null(S4Vectors::metadata(sce)$cluster_codes)) {
            lvls <- levels(factor(sce$cluster_id))
            S4Vectors::metadata(sce)$cluster_codes <- data.frame(
              cluster_id = factor(lvls, levels = lvls),
              row.names = lvls
            )
          }
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
          showNotification(paste("Heatmap error:", e$message), type = "error")
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
      showNotification(conditionMessage(e), type = "error")
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
      # patchwork assembly: honours per-marker ncol setting
      fp_clean <- Filter(Negate(is.null), fp)
      patchwork::wrap_plots(fp_clean, ncol = input$ncolFPGene)
    } else if (is.list(fp) && length(fp) == 1) {
      fp[[1]]
    } else {
      fp
    }
  },
  height = function() input$figHeightFP,
  width = function() input$figWidthFP
)
