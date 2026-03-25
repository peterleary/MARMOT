# ── server-plots.R ──────────────────────────────────────────────────────────
# Feature plot module for Shiny MARMOT v2.
# Handles all 8 plot types using SCE data (NOT Seurat).
# Combines exploreSingleCell's advanced settings patterns with MARMOT's
# existing plot helper functions from helpers/plot_helpers.R.
#
# Data model:
#   inputDataReactive$Results$sce           — SingleCellExperiment
#   inputDataReactive$Results$umapDFList    — named list of DR data frames
#   inputDataReactive$Results$coloursList   — named list of colour vectors
#   inputDataReactive$Results$subsetCellIds — NULL or character vector
#   inputDataReactive$Results$smd           — study metadata data frame
#   inputDataReactive$Results$selectedClustersList — DA cluster lists
#   featurePlotReactive$fp                  — the plot output (list or ggplot)
#   featurePlotReactive$needs_arrange       — TRUE if patchwork assembly needed
#   featurePlotReactive$ncol                — ncol for patchwork
#   genesReactive$genes                     — validated marker names (from server-colours.R)
#   clusterTableReactive$table              — relabel table (from server-relabel.R)
# ─────────────────────────────────────────────────────────────────────────────

# ── UI element management helpers ────────────────────────────────────────────
# exploreSingleCell pattern: create_ui_element / clear_ui_elements
# These render dynamic UI into the numbered output slots declared in
# ui-tab-umap.R's "Advanced" tab.

create_ui_element <- function(output_id, ui_element) {
  output[[output_id]] <- renderUI(ui_element)
}

clear_ui_elements <- function(ids) {
  for (id in ids) {
    output[[id]] <- renderUI(NULL)
  }
}

# ── 1. Plot-By Bucket List ──────────────────────────────────────────────────
observeEvent(input$fpColumnToPlot, {
  plot_column <- input$fpColumnToPlot
  if (is.null(plot_column) || plot_column == "None") return()
  req(inputDataReactive$Results$sce)

  cd <- as.data.frame(SummarizedExperiment::colData(inputDataReactive$Results$sce))
  plot_data <- cd[[plot_column]]
  plot_levels <- if (is.factor(plot_data)) {
    levels(plot_data)
  } else {
    gtools::mixedsort(unique(as.character(plot_data)))
  }

  output$plotByBucket <- renderUI({
    sortable::bucket_list(
      header = "Drag groups to reorder or exclude",
      group_name = "bucket_list_group1",
      orientation = "horizontal",
      sortable::add_rank_list(
        text = "Include these groups",
        labels = as.list(plot_levels),
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


# ── 2. Split-By Bucket List ─────────────────────────────────────────────────
observeEvent(input$fpColumnToSplit, {
  split_column <- input$fpColumnToSplit
  if (is.null(split_column) || split_column == "None") {
    output$splitByBucket <- renderText("Select a variable to split by first!")
    return()
  }
  req(inputDataReactive$Results$sce)

  cd <- as.data.frame(SummarizedExperiment::colData(inputDataReactive$Results$sce))
  split_data <- cd[[split_column]]
  split_levels <- if (is.factor(split_data)) {
    levels(split_data)
  } else {
    gtools::mixedsort(unique(as.character(split_data)))
  }

  output$splitByBucket <- renderUI({
    sortable::bucket_list(
      header = "Drag groups to reorder or exclude",
      group_name = "bucket_list_group2",
      orientation = "horizontal",
      sortable::add_rank_list(
        text = "Include these groups",
        labels = as.list(split_levels),
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
}, suspended = FALSE)


# ── 3. Dynamic Advanced Settings per Plot Type ───────────────────────────────
# All UI slot IDs declared in ui-tab-umap.R's "Advanced" tab:
#   umapFeaturePlotSettingsUI0..10  (DR, raster, border, label, custom min/max)
#   umapFeaturePlotDotPlotUI1..8   (dot plot settings)
#   umapFeaturePlotViolinUI1..6    (violin settings)
#   umapFeaturePlotHeatmapUI1..6   (heatmap settings)
#   fpRidgePlotUI1..2              (ridge settings)
#   fpNebulosaOutputUI1..2         (nebulosa settings)
#   fpBarplotOptionsUI1..3         (barplot settings)
#   fpBarplotOutputUI2             (barplot counts table)
#   umapFeaturePlotWarningUI       (warning slot)

# Slot ID lists for batch clearing
all_dr_ids      <- paste0("umapFeaturePlotSettingsUI", c(0:10))
all_dotplot_ids <- paste0("umapFeaturePlotDotPlotUI", 1:8)
all_violin_ids  <- paste0("umapFeaturePlotViolinUI", 1:6)
all_heatmap_ids <- paste0("umapFeaturePlotHeatmapUI", 1:6)
all_ridge_ids   <- paste0("fpRidgePlotUI", 1:2)
all_nebulosa_ids <- paste0("fpNebulosaOutputUI", 1:2)
all_barplot_ids <- c(paste0("fpBarplotOptionsUI", 1:3), "fpBarplotOutputUI2")

observeEvent(
  { input$featurePlotType },
  ignoreNULL = FALSE,
  {
    plot_type <- input$featurePlotType %||% "Feature Plot"

    # ── Clear all dynamic settings first ─────────────────────────────────
    clear_ui_elements(c(
      all_dr_ids, all_dotplot_ids, all_violin_ids,
      all_heatmap_ids, all_ridge_ids, all_nebulosa_ids,
      all_barplot_ids, "umapFeaturePlotWarningUI"
    ))

    # ── Feature Plot / Nebulosa Plot: DR + raster + border settings ──────
    if (plot_type %in% c("Feature Plot", "Nebulosa Plot")) {

      # DR selector
      create_ui_element("umapFeaturePlotSettingsUI0", {
        dr_names <- names(inputDataReactive$Results$umapDFList)
        fp_dr_default <- dr_names[1]
        for (pref in c("TSNE", "UMAP", "PaCMAP")) {
          hit <- grep(pref, dr_names, ignore.case = TRUE, value = TRUE)
          ds_hit <- grep("Downsampled", hit, value = TRUE)
          if (length(ds_hit) > 0) hit <- ds_hit
          if (length(hit) > 0) fp_dr_default <- hit[1]
        }
        selectInput(
          inputId = "fpDRToPlot", label = "DR to plot",
          choices = dr_names, selected = fp_dr_default,
          multiple = FALSE, width = "85%"
        )
      })

      # Dot settings
      create_ui_element("umapFeaturePlotSettingsUI1", {
        sliderInput("pointSizeFP", "Dot size",
          min = 0.1, max = 4, value = 1, step = 0.1,
          width = "85%", ticks = FALSE)
      })
      create_ui_element("umapFeaturePlotSettingsUI2", {
        splitLayout(
          checkboxInput("rasteriseFP", "Rasterise?", value = FALSE),
          numericInput("rasterFP_DPI", "Raster DPI",
            value = 1024, min = 0, max = 2000, step = 5, width = "85%")
        )
      })

      # Cell borders — grouped together
      create_ui_element("umapFeaturePlotSettingsUI3", {
        checkboxInput("cellBordersFP", "Show cell borders?", value = TRUE)
      })
      create_ui_element("umapFeaturePlotSettingsUI4", {
        splitLayout(
          sliderInput("borderSizeFP", "Border size",
            min = 1, max = 5, value = 2, step = 0.1,
            width = "85%", ticks = FALSE),
          sliderInput("borderDensityFP", "Border density",
            min = 0.05, max = 1, value = 1, step = 0.05,
            width = "85%", ticks = FALSE)
        )
      })
      create_ui_element("umapFeaturePlotSettingsUI5", {
        colourpicker::colourInput("borderColourFP", "Border colour",
          value = "black", showColour = "both", width = "66%")
      })

      # Axes and labels
      create_ui_element("umapFeaturePlotSettingsUI6", {
        checkboxInput("fpShowAxes", "Show plot axes?", value = FALSE)
      })
      create_ui_element("umapFeaturePlotSettingsUI7", {
        checkboxInput("fpShowLabels", "Show cluster labels?", value = FALSE)
      })
      create_ui_element("umapFeaturePlotSettingsUI8", {
        radioButtons("fpLabelColour", "Colour cluster labels by:",
          choiceNames = c("Label colour", "Gene median", "Gene mean"),
          choiceValues = c("label", "median", "mean"),
          selected = "mean")
      })
    }

    # ── Feature Plot specific: custom min/max ────────────────────────────
    if (plot_type == "Feature Plot") {
      create_ui_element("umapFeaturePlotSettingsUI9", {
        tagList(
          checkboxInput("fpDRCustomMinMax", "Use custom min/max values?", value = FALSE),
          conditionalPanel(
            condition = "input.fpDRCustomMinMax == true",
            splitLayout(
              numericInput("fpDRCustomMin", "Min",
                value = 0, min = -Inf, max = Inf, step = 0.5, width = "66%"),
              numericInput("fpDRCustomMax", "Max",
                value = 6, min = -Inf, max = Inf, step = 0.5, width = "66%")
            )
          )
        )
      })
    }

    # ── Nebulosa: joint plot options ─────────────────────────────────────
    if (plot_type == "Nebulosa Plot") {
      create_ui_element("fpNebulosaOutputUI1", {
        checkboxInput("fpNebulosaPlotTogether", "Show joint plot?", value = TRUE)
      })
      create_ui_element("fpNebulosaOutputUI2", {
        checkboxInput("fpNebulosaPlotTogetherOnly", "Show only joint plot?", value = FALSE)
      })
    }

    # ── Dot Plot settings ────────────────────────────────────────────────
    if (plot_type == "Dot Plot") {
      create_ui_element("umapFeaturePlotDotPlotUI1", {
        sliderInput("fpDotPlotDotScale", "Dot scale",
          min = 1, max = 20, value = 10, step = 0.5,
          width = "85%", ticks = FALSE)
      })
      create_ui_element("umapFeaturePlotDotPlotUI2", {
        checkboxInput("umapFeaturePlotDotplotFlip", "Flip dot plot?", value = TRUE)
      })
      create_ui_element("umapFeaturePlotDotPlotUI3", {
        checkboxInput("fpDotPlotHideBorder", "Hide dot border?", value = FALSE)
      })
      create_ui_element("umapFeaturePlotDotPlotUI4", {
        checkboxInput("fpDotPlotHideLegend", "Hide legend?", value = FALSE)
      })
      create_ui_element("umapFeaturePlotDotPlotUI5", {
        selectInput("fpDotPlotLegendPosition", "Legend position",
          choices = c("Right" = "right", "Bottom" = "bottom",
                      "Left" = "left", "Top" = "top"),
          selected = "right")
      })
      create_ui_element("umapFeaturePlotDotPlotUI6", {
        selectInput("fpDotPlotScaling", "Expression scaling",
          choices = c("None", "Z-score", "Quantile"),
          selected = "None")
      })
      create_ui_element("umapFeaturePlotDotPlotUI7", {
        checkboxInput("fpDotPlotUniformSize", "Uniform dot size?", value = FALSE)
      })
      create_ui_element("umapFeaturePlotDotPlotUI8", {
        selectInput("fpDotPlotScaleBasis", "Scaling basis",
          choices = c("Cell-level" = "cell", "Group-level" = "group"),
          selected = "cell")
      })
      # "Plot all features" checkbox
      create_ui_element("umapFeaturePlotHeatmapUI1", {
        checkboxInput("fpHeatmapPlotAll", "Plot all available features?", value = FALSE)
      })
    }

    # ── Violin Plot settings ─────────────────────────────────────────────
    if (plot_type == "Violin Plot") {
      create_ui_element("umapFeaturePlotViolinUI1", {
        checkboxInput("fpViolinShowBoxplot", "Show boxplot overlay?", value = FALSE)
      })
      create_ui_element("umapFeaturePlotViolinUI2", {
        sliderInput("fpViolinAxisAngle", "X-axis label angle",
          min = 0, max = 90, value = 45, step = 5,
          width = "85%", ticks = FALSE)
      })
      create_ui_element("umapFeaturePlotViolinUI3", {
        checkboxInput("fpViolinTrim", "Trim violin tails?", value = TRUE)
      })
      create_ui_element("umapFeaturePlotViolinUI4", {
        tagList(
          checkboxInput("fpViolinShowMedian", "Show median line?", value = FALSE),
          checkboxInput("fpViolinShowQuartiles", "Show quartile lines?", value = FALSE)
        )
      })
      create_ui_element("umapFeaturePlotViolinUI5", {
        splitLayout(
          sliderInput("fpViolinWidth", "Violin width",
            min = 0.2, max = 2, value = 0.9, step = 0.1,
            width = "85%", ticks = FALSE),
          sliderInput("fpViolinBarWidth", "Boxplot width",
            min = 0.05, max = 0.5, value = 0.1, step = 0.05,
            width = "85%", ticks = FALSE)
        )
      })
      create_ui_element("umapFeaturePlotViolinUI6", {
        sliderInput("fpViolinLineThickness", "Line thickness",
          min = 0.1, max = 3, value = 0.5, step = 0.1,
          width = "85%", ticks = FALSE)
      })
    }

    # ── Heatmap per cluster settings ─────────────────────────────────────
    if (plot_type == "Heatmap per cluster") {
      create_ui_element("umapFeaturePlotHeatmapUI1", {
        checkboxInput("fpHeatmapPlotAll", "Plot all available features?", value = FALSE)
      })
      create_ui_element("umapFeaturePlotHeatmapUI2", {
        checkboxInput("umapFeaturePlotHeatmapCluster", "Cluster heatmap?", value = TRUE)
      })
      create_ui_element("umapFeaturePlotHeatmapUI3", {
        checkboxInput("umapFeaturePlotHeatmapFlip", "Flip heatmap?", value = TRUE)
      })
      create_ui_element("umapFeaturePlotHeatmapUI4", {
        selectInput("umapFeaturePlotHeatmapScaling", "Scaling method",
          choices = c("None", "Z-score", "Quantile"),
          selected = "None")
      })
      create_ui_element("umapFeaturePlotHeatmapUI5", {
        selectInput("umapFeaturePlotHeatmapScaleBasis", "Scaling basis",
          choices = c("Cell-level" = "cell", "Group-level" = "group"),
          selected = "cell")
      })
      create_ui_element("umapFeaturePlotHeatmapUI6", {
        tagList(
          checkboxInput("heatmapClusterCustomLimits",
            "Use custom colour limits?", value = FALSE),
          conditionalPanel(
            condition = "input.heatmapClusterCustomLimits == true",
            splitLayout(
              numericInput("heatmapClusterLimitLow", "Low",
                value = -2, step = 0.5, width = "66%"),
              numericInput("heatmapClusterLimitHigh", "High",
                value = 2, step = 0.5, width = "66%")
            )
          )
        )
      })
    }

    # ── Heatmap per cell: "Plot all features" checkbox ───────────────────
    if (plot_type == "Heatmap per cell") {
      create_ui_element("umapFeaturePlotHeatmapUI1", {
        checkboxInput("fpHeatmapPlotAll", "Plot all available features?", value = FALSE)
      })
    }

    # ── Ridge Plot settings ──────────────────────────────────────────────
    if (plot_type == "Ridge Plot") {
      create_ui_element("fpRidgePlotUI1", {
        checkboxInput("fpRidgePlotHideLegend", "Hide legend?", value = TRUE)
      })
      create_ui_element("fpRidgePlotUI2", {
        selectInput("fpRidgePlotLegendPosition", "Legend position",
          choices = c("Right" = "right", "Bottom" = "bottom",
                      "Left" = "left", "None" = "none"),
          selected = "none")
      })
    }

  }
)


# ── 4. "Plot all features" toggle logic ─────────────────────────────────────
previousFeatureSelection <- reactiveVal(NULL)

observeEvent({
  input$fpHeatmapPlotAll
  input$featurePlotType
}, ignoreNULL = TRUE, {
  req(!is.null(input$featurePlotType))
  req(!is.null(input$fpHeatmapPlotAll))
  if (input$featurePlotType %in% c("Heatmap per cluster", "Heatmap per cell", "Dot Plot")) {
    if (input$fpHeatmapPlotAll) {
      previousFeatureSelection(input$fpFeatureToPlot)
      marker_choices <- inputDataReactive$Results$sorted_markers_cache %||%
        rownames(inputDataReactive$Results$sce)
      updateSelectizeInput(session, "fpFeatureToPlot",
        choices = marker_choices,
        selected = rownames(inputDataReactive$Results$sce),
        server = TRUE)
    } else {
      prev <- previousFeatureSelection()
      marker_choices <- inputDataReactive$Results$sorted_markers_cache %||%
        rownames(inputDataReactive$Results$sce)
      updateSelectizeInput(session, "fpFeatureToPlot",
        choices = marker_choices,
        selected = prev,
        server = TRUE)
    }
  }
})


# ── 5. Adaptive debounce for feature plot reactive ──────────────────────────
feature_debounce_ms <- reactive({
  ncell <- inputDataReactive$Results$ncell %||% 0L
  if (ncell > 200000L) 400L else if (ncell > 50000L) 200L else 100L
})

# Combine all feature-plot-relevant inputs into a single reactive for debouncing.
# Only track inputs relevant to the active plot type to avoid wasted redraws.
fp_inputs_raw <- reactive({
  plot_type <- input$featurePlotType

  # Common inputs (always tracked)
  common <- list(
    plot_type, input$fpDRToPlot, genesReactive$genes,
    input$fpAssayToPlot, input$fpColumnToPlot, input$fpColumnToSplit,
    input$pointSizeFP, input$textSizeFP, input$ncolFPGene, input$ncolFPSplit,
    input$viridisColourFP, input$flipViridisFP, input$fpLegendPosition,
    input$fpShowLabels, input$fpLabelColour, input$fpShowAxes,
    input$plotByKeepBucket, input$splitByKeepBucket,
    input$rasteriseFP, input$rasterFP_DPI,
    input$fpContrast, input$fpDAFilter,
    input$fpSubsetMode, input$fpSubsetToGlobal,
    input$fpDRCustomMinMax, input$fpDRCustomMin, input$fpDRCustomMax
  )

  # Per-type inputs (only tracked when active)
  type_specific <- switch(plot_type,
    "Feature Plot" = , "Nebulosa Plot" = list(
      input$cellBordersFP, input$borderSizeFP,
      input$borderDensityFP, input$borderColourFP,
      input$fpNebulosaPlotTogether, input$fpNebulosaPlotTogetherOnly
    ),
    "Violin Plot" = list(
      input$fpViolinShowBoxplot, input$fpViolinAxisAngle,
      input$fpViolinTrim, input$fpViolinShowMedian,
      input$fpViolinShowQuartiles, input$fpViolinWidth,
      input$fpViolinBarWidth, input$fpViolinLineThickness
    ),
    "Dot Plot" = list(
      input$umapFeaturePlotDotplotFlip, input$fpDotPlotDotScale,
      input$fpDotPlotHideBorder, input$fpDotPlotHideLegend,
      input$fpDotPlotLegendPosition, input$fpDotPlotScaling,
      input$fpDotPlotUniformSize, input$fpDotPlotScaleBasis
    ),
    "Ridge Plot" = list(
      input$fpRidgePlotHideLegend, input$fpRidgePlotLegendPosition
    ),
    "Heatmap per cell" = , "Heatmap per cluster" = list(
      input$umapFeaturePlotHeatmapCluster, input$umapFeaturePlotHeatmapFlip,
      input$umapFeaturePlotHeatmapScaling, input$umapFeaturePlotHeatmapScaleBasis,
      input$heatmapClusterCustomLimits, input$heatmapClusterLimitLow,
      input$heatmapClusterLimitHigh, input$fpHeatmapPlotAll
    ),
    list()
  )

  c(common, type_specific)
})

fp_inputs <- fp_inputs_raw |> debounce(feature_debounce_ms)


# ── 6. Clear plot on type change ────────────────────────────────────────────
observeEvent(input$featurePlotType, {
  featurePlotReactive$fp <- NULL
  featurePlotReactive$needs_arrange <- FALSE
  featurePlotReactive$ncol <- 1
})


# ── 7. Main Feature Plot Observer ───────────────────────────────────────────
observeEvent(
  fp_inputs(),
  ignoreNULL = FALSE,
  {
    tryCatch({

      # ── Validate core data ──────────────────────────────────────────────
      req(
        !is.null(inputDataReactive$Results$sce),
        !is.null(inputDataReactive$Results$umapDFList)
      )

      plot_type <- input$featurePlotType %||% "Feature Plot"
      fpFeaturesToPlot <- genesReactive$genes

      req(length(fpFeaturesToPlot) > 0)

      sce <- inputDataReactive$Results$sce

      # ── Resolve assay ──────────────────────────────────────────────────
      assayToUse <- input$fpAssayToPlot %||% "exprsQuantNorm"
      avail_assays <- SummarizedExperiment::assayNames(sce)
      if (!assayToUse %in% avail_assays) assayToUse <- avail_assays[1]

      viridisFlip <- if (isTRUE(input$flipViridisFP)) -1 else 1

      fpColumnToPlot <- if (is.null(input$fpColumnToPlot) ||
        input$fpColumnToPlot == "None") NULL else input$fpColumnToPlot
      fpColumnToSplit <- if (is.null(input$fpColumnToSplit) ||
        input$fpColumnToSplit == "None") NULL else input$fpColumnToSplit

      # ── Resolve DR data frame ──────────────────────────────────────────
      drName <- input$fpDRToPlot %||% names(inputDataReactive$Results$umapDFList)[1]
      umapDF <- inputDataReactive$Results$umapDFList[[drName]]
      if (is.null(umapDF)) {
        umapDF <- inputDataReactive$Results$umapDFList[[
          names(inputDataReactive$Results$umapDFList)[1]
        ]]
      }

      # ── Apply cell subsetting ──────────────────────────────────────────
      subsetMode <- input$fpSubsetMode %||% "None"
      if (subsetMode != "None") {
        subsetIds <- inputDataReactive$Results[["subsetCellIds"]]
        if (!is.null(subsetIds) && length(subsetIds) > 0) {
          # Subset umapDF via sce_idx if available
          if (!is.null(umapDF) && "sce_idx" %in% colnames(umapDF)) {
            keep_idx <- which(
              colnames(inputDataReactive$Results$sce) %in% subsetIds
            )
            umapDF <- umapDF[umapDF$sce_idx %in% keep_idx, ]
          }
          sce <- sce[, colnames(sce) %in% subsetIds]
        }
      }

      # ── Apply bucket filtering (Plot-By) ───────────────────────────────
      if (!is.null(fpColumnToPlot) && !is.null(input$plotByKeepBucket)) {
        if (!is.null(umapDF) && fpColumnToPlot %in% colnames(umapDF)) {
          umapDF <- umapDF[umapDF[[fpColumnToPlot]] %in% input$plotByKeepBucket, ]
          umapDF[[fpColumnToPlot]] <- factor(
            umapDF[[fpColumnToPlot]], levels = input$plotByKeepBucket)
        }
        keep_cells <- colnames(sce)[sce[[fpColumnToPlot]] %in% input$plotByKeepBucket]
        if (length(keep_cells) > 0) {
          sce <- sce[, keep_cells]
          sce[[fpColumnToPlot]] <- factor(
            sce[[fpColumnToPlot]], levels = input$plotByKeepBucket)
        }
      }

      # ── Apply bucket filtering (Split-By) ──────────────────────────────
      if (!is.null(fpColumnToSplit) && !is.null(input$splitByKeepBucket)) {
        if (!is.null(umapDF) && fpColumnToSplit %in% colnames(umapDF)) {
          umapDF <- umapDF[umapDF[[fpColumnToSplit]] %in% input$splitByKeepBucket, ]
          umapDF[[fpColumnToSplit]] <- factor(
            umapDF[[fpColumnToSplit]], levels = input$splitByKeepBucket)
        }
        keep_cells <- colnames(sce)[sce[[fpColumnToSplit]] %in% input$splitByKeepBucket]
        if (length(keep_cells) > 0) sce <- sce[, keep_cells]
      }

      # ── DA cluster filtering ───────────────────────────────────────────
      da_mode  <- input$fpDAFilter %||% "None"
      contrast <- input$fpContrast %||% "None"
      if (da_mode != "None" && contrast != "None" &&
          !is.null(inputDataReactive$Results$selectedClustersList)) {
        scl       <- inputDataReactive$Results$selectedClustersList
        scl_names <- names(scl)
        matched   <- scl_names[startsWith(scl_names, contrast)]
        up_idx    <- grep("Up$|up$|\\.up$", matched)
        down_idx  <- grep("Down$|down$|\\.down$", matched)
        if (length(up_idx) == 0 && length(down_idx) == 0 && length(matched) >= 2) {
          up_idx <- 1; down_idx <- 2
        }
        up_clusters   <- if (length(up_idx) > 0) scl[[matched[up_idx[1]]]] else character(0)
        down_clusters <- if (length(down_idx) > 0) scl[[matched[down_idx[1]]]] else character(0)

        da_clusters <- switch(da_mode,
          "All" = as.character(c(up_clusters, down_clusters)),
          "Up only" = up_clusters,
          "Down only" = down_clusters,
          character(0)
        )
        if (length(da_clusters) > 0) {
          keep_cells <- colnames(sce)[as.character(sce$cluster_id) %in% da_clusters]
          if (length(keep_cells) > 0) {
            sce <- sce[, keep_cells]
            if (!is.null(umapDF) && "cluster_id" %in% colnames(umapDF)) {
              umapDF <- umapDF[as.character(umapDF$cluster_id) %in% da_clusters, ]
            }
          }
        } else {
          showNotification("There are no DA clusters in this contrast!", type = "error")
        }
      }

      req(!is.null(umapDF))

      # ── Shared batch expression extraction ─────────────────────────────
      # Used by Feature Plot and Nebulosa Plot for efficient one-read access
      orig_sce <- inputDataReactive$Results$sce
      expr_batch <- NULL

      if (plot_type %in% c("Feature Plot", "Nebulosa Plot") &&
            length(fpFeaturesToPlot) > 0) {
        if (assayToUse %in% SummarizedExperiment::assayNames(orig_sce)) {
          marker_idx <- match(fpFeaturesToPlot, rownames(orig_sce))
          na_pos <- is.na(marker_idx)
          if (any(na_pos)) {
            marker_idx[na_pos] <- match(
              gsub("-", "_", fpFeaturesToPlot[na_pos]), rownames(orig_sce))
          }
          valid_idx <- which(!is.na(marker_idx))
          valid_markers <- fpFeaturesToPlot[valid_idx]
          valid_row_idx <- marker_idx[valid_idx]
        } else {
          valid_markers <- character(0)
          valid_row_idx <- integer(0)
        }

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


      # ================================================================
      # Pre-compute KDE border_df once (shared by Feature Plot + Nebulosa)
      # ================================================================
      pre_border_df <- NULL
      if ((plot_type %in% c("Feature Plot", "Nebulosa Plot")) &&
          isTRUE(input$cellBordersFP %||% TRUE) && nrow(umapDF) > 10) {
        border_density_val <- input$borderDensityFP %||% 1
        kde <- MASS::kde2d(umapDF[["x"]], umapDF[["y"]], n = 100L)
        ix  <- pmax(1L, pmin(findInterval(umapDF[["x"]], kde$x), length(kde$x)))
        iy  <- pmax(1L, pmin(findInterval(umapDF[["y"]], kde$y), length(kde$y)))
        cell_density <- kde$z[cbind(ix, iy)]
        pre_border_df <- umapDF[cell_density <= quantile(cell_density, border_density_val), ]
      }

      # Pre-compute label base positions (group medians are marker-independent)
      base_label_pos <- NULL
      if (isTRUE(input$fpShowLabels) && !is.null(fpColumnToPlot) &&
          fpColumnToPlot %in% colnames(umapDF)) {
        base_label_pos <- compute_label_positions(umapDF, fpColumnToPlot)
      }

      # ================================================================
      # FEATURE PLOT (per-marker scatter)
      # ================================================================
      if (plot_type == "Feature Plot") {

        fp <- lapply(fpFeaturesToPlot, function(marker) {
          df <- umapDF

          # Attach expression from pre-extracted batch matrix
          if (!is.null(expr_batch) && marker %in% rownames(expr_batch)) {
            df[[marker]] <- as.numeric(expr_batch[marker, ])
          }
          if (!marker %in% colnames(df)) return(NULL)

          # Apply custom min/max clamping
          if (isTRUE(input$fpDRCustomMinMax)) {
            cmin <- input$fpDRCustomMin %||% 0
            cmax <- input$fpDRCustomMax %||% 6
            df[[marker]] <- pmax(cmin, pmin(cmax, df[[marker]]))
          }

          df <- df[order(df[[marker]], decreasing = FALSE), ]

          fp2 <- make_feature_scatter(
            df = df, marker = marker,
            palette = input$viridisColourFP %||% "viridis",
            direction = viridisFlip,
            point_size = input$pointSizeFP %||% 1,
            alpha = 0.6,
            rasterise = isTRUE(input$rasteriseFP),
            raster_dpi = input$rasterFP_DPI %||% 1024,
            border = input$cellBordersFP %||% TRUE,
            border_size = input$borderSizeFP %||% 2.0,
            border_density = input$borderDensityFP %||% 1,
            border_colour = input$borderColourFP %||% "black",
            border_df = pre_border_df,
            base_size = input$textSizeFP %||% 14,
            show_axes = isTRUE(input$fpShowAxes),
            legend_position = tolower(input$fpLegendPosition %||% "right")
          )

          # Facet by split column
          if (!is.null(fpColumnToSplit) && fpColumnToSplit %in% colnames(df)) {
            fp2 <- add_facet_with_counts(fp2, df, fpColumnToSplit,
              input$ncolFPSplit %||% 1)
          }

          # Cluster labels
          if (!is.null(base_label_pos)) {
            label_colour_mode <- input$fpLabelColour %||% "label"
            if (label_colour_mode != "label") {
              median_pos <- compute_label_positions(df, fpColumnToPlot, marker)
            } else {
              median_pos <- base_label_pos
            }
            fp2 <- fp2 + ggnewscale::new_scale_color() +
              ggnewscale::new_scale_fill()

            if (label_colour_mode == "label") {
              fp2 <- fp2 +
                ggrepel::geom_label_repel(
                  data = median_pos,
                  aes(
                    label = .data[[fpColumnToPlot]],
                    x = .data[["x"]], y = .data[["y"]],
                    fill = .data[[fpColumnToPlot]]
                  ),
                  colour = "white", fontface = "bold",
                  show.legend = FALSE,
                  size = (input$textSizeFP %||% 14) / 4,
                  max.overlaps = 100
                ) +
                scale_fill_manual(
                  values = inputDataReactive$Results$coloursList[[fpColumnToPlot]]
                )
            } else {
              fp2 <- fp2 +
                ggrepel::geom_label_repel(
                  data = median_pos,
                  aes(
                    label = .data[[fpColumnToPlot]],
                    x = .data[["x"]], y = .data[["y"]],
                    fill = .data[[label_colour_mode]]
                  ),
                  colour = "white", fontface = "bold",
                  show.legend = FALSE,
                  size = (input$textSizeFP %||% 14) / 4,
                  max.overlaps = 100
                )
              fp2 <- apply_continuous_scale(
                fp2, input$viridisColourFP %||% "viridis", viridisFlip, "fill")
            }
          }
          fp2
        })
        fp <- Filter(Negate(is.null), fp)

        featurePlotReactive$fp <- fp
        featurePlotReactive$needs_arrange <- length(fp) > 1
        featurePlotReactive$ncol <- input$ncolFPGene %||% 1


      # ================================================================
      # NEBULOSA PLOT (ks::kde weighted scatter)
      # ================================================================
      } else if (plot_type == "Nebulosa Plot") {

        if (!requireNamespace("ks", quietly = TRUE)) {
          showNotification(
            "ks package is not installed. Install with install.packages('ks').",
            type = "error")
          return(NULL)
        }

        emb_mat <- as.matrix(umapDF[, c("x", "y")])
        joint <- isTRUE(input$fpNebulosaPlotTogether) && length(fpFeaturesToPlot) > 1
        return_only_joint <- isTRUE(input$fpNebulosaPlotTogetherOnly)

        # Per-gene weighted density scatter
        fp <- lapply(fpFeaturesToPlot, function(marker) {
          df <- umapDF

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
          df <- df[order(df$density), ]

          fp2 <- make_feature_scatter(
            df = df, marker = "density",
            palette = input$viridisColourFP %||% "viridis",
            direction = viridisFlip,
            point_size = input$pointSizeFP %||% 1,
            alpha = 0.7,
            rasterise = isTRUE(input$rasteriseFP),
            raster_dpi = input$rasterFP_DPI %||% 1024,
            border = input$cellBordersFP %||% TRUE,
            border_size = input$borderSizeFP %||% 2.0,
            border_density = input$borderDensityFP %||% 1,
            border_colour = input$borderColourFP %||% "black",
            border_df = pre_border_df,
            base_size = input$textSizeFP %||% 14,
            show_axes = isTRUE(input$fpShowAxes),
            legend_position = tolower(input$fpLegendPosition %||% "right")
          ) + ggtitle(paste0(marker, " density")) + labs(colour = "Density")

          # Cluster labels
          if (!is.null(base_label_pos)) {
            label_colour_mode <- input$fpLabelColour %||% "label"
            if (label_colour_mode != "label") {
              median_pos <- compute_label_positions(df, fpColumnToPlot, marker)
            } else {
              median_pos <- base_label_pos
            }
            fp2 <- fp2 + ggnewscale::new_scale_color() +
              ggnewscale::new_scale_fill()
            if (label_colour_mode == "label") {
              fp2 <- fp2 +
                ggrepel::geom_label_repel(
                  data = median_pos,
                  aes(label = .data[[fpColumnToPlot]],
                      x = .data[["x"]], y = .data[["y"]],
                      fill = .data[[fpColumnToPlot]]),
                  colour = "white", fontface = "bold",
                  show.legend = FALSE,
                  size = (input$textSizeFP %||% 14) / 4,
                  max.overlaps = 100
                ) +
                scale_fill_manual(
                  values = inputDataReactive$Results$coloursList[[fpColumnToPlot]]
                )
            } else {
              fp2 <- fp2 +
                ggrepel::geom_label_repel(
                  data = median_pos,
                  aes(label = .data[[fpColumnToPlot]],
                      x = .data[["x"]], y = .data[["y"]],
                      fill = .data[[label_colour_mode]]),
                  colour = "white", fontface = "bold",
                  show.legend = FALSE,
                  size = (input$textSizeFP %||% 14) / 4,
                  max.overlaps = 100
                )
              fp2 <- apply_continuous_scale(
                fp2, input$viridisColourFP %||% "viridis", viridisFlip, "fill")
            }
          }
          fp2
        })
        fp <- Filter(Negate(is.null), fp)

        # Joint density (sum expression weights across genes, single KDE)
        if (joint && length(fpFeaturesToPlot) > 1) {
          df_joint <- umapDF
          # Vectorized weight summation across markers
          valid_markers <- fpFeaturesToPlot[fpFeaturesToPlot %in% rownames(expr_batch)]
          w_joint <- if (!is.null(expr_batch) && length(valid_markers) > 0) {
            colSums(expr_batch[valid_markers, , drop = FALSE], na.rm = TRUE)
          } else {
            rep(0, nrow(df_joint))
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
            palette = input$viridisColourFP %||% "viridis",
            direction = viridisFlip,
            point_size = input$pointSizeFP %||% 1,
            alpha = 0.7,
            rasterise = isTRUE(input$rasteriseFP),
            raster_dpi = input$rasterFP_DPI %||% 1024,
            border = input$cellBordersFP %||% TRUE,
            border_size = input$borderSizeFP %||% 2.0,
            border_density = input$borderDensityFP %||% 1,
            border_colour = input$borderColourFP %||% "black",
            border_df = pre_border_df,
            base_size = input$textSizeFP %||% 14,
            show_axes = isTRUE(input$fpShowAxes),
            legend_position = tolower(input$fpLegendPosition %||% "right")
          ) +
            ggtitle(paste0("Joint: ", paste(fpFeaturesToPlot, collapse = " + "))) +
            labs(colour = "Density")

          # Cluster labels on joint plot
          if (!is.null(base_label_pos)) {
            label_colour_mode <- input$fpLabelColour %||% "label"
            if (label_colour_mode != "label") {
              median_pos <- compute_label_positions(df_joint, fpColumnToPlot, "density")
            } else {
              median_pos <- base_label_pos
            }
            joint_plot <- joint_plot + ggnewscale::new_scale_color() +
              ggnewscale::new_scale_fill()
            if (label_colour_mode == "label") {
              joint_plot <- joint_plot +
                ggrepel::geom_label_repel(
                  data = median_pos,
                  aes(label = .data[[fpColumnToPlot]],
                      x = .data[["x"]], y = .data[["y"]],
                      fill = .data[[fpColumnToPlot]]),
                  colour = "white", fontface = "bold",
                  show.legend = FALSE,
                  size = (input$textSizeFP %||% 14) / 4,
                  max.overlaps = 100
                ) +
                scale_fill_manual(
                  values = inputDataReactive$Results$coloursList[[fpColumnToPlot]]
                )
            } else {
              joint_plot <- joint_plot +
                ggrepel::geom_label_repel(
                  data = median_pos,
                  aes(label = .data[[fpColumnToPlot]],
                      x = .data[["x"]], y = .data[["y"]],
                      fill = .data[[label_colour_mode]]),
                  colour = "white", fontface = "bold",
                  show.legend = FALSE,
                  size = (input$textSizeFP %||% 14) / 4,
                  max.overlaps = 100
                )
              joint_plot <- apply_continuous_scale(
                joint_plot, input$viridisColourFP %||% "viridis", viridisFlip, "fill")
            }
          }
          fp <- c(fp, list(joint_plot))
        }

        if (return_only_joint && length(fp) > 1) {
          fp <- list(fp[[length(fp)]])
        }

        featurePlotReactive$fp <- fp
        featurePlotReactive$needs_arrange <- length(fp) > 1
        featurePlotReactive$ncol <- input$ncolFPGene %||% 1


      # ================================================================
      # VIOLIN PLOT
      # ================================================================
      } else if (plot_type == "Violin Plot") {

        req(fpColumnToPlot)
        colsToViolin <- if (!is.null(fpColumnToSplit)) fpColumnToSplit else fpColumnToPlot

        # Batch-extract expression + colData once (avoid per-marker S4 dispatch)
        cd_violin <- as.data.frame(SummarizedExperiment::colData(sce))
        violin_row_idx <- match(fpFeaturesToPlot, rownames(sce))
        na_pos <- is.na(violin_row_idx)
        if (any(na_pos)) {
          violin_row_idx[na_pos] <- match(gsub("-", "_", fpFeaturesToPlot[na_pos]), rownames(sce))
        }
        valid_violin <- !is.na(violin_row_idx)
        expr_violin <- if (any(valid_violin)) {
          SummarizedExperiment::assay(sce, assayToUse)[violin_row_idx[valid_violin], , drop = FALSE]
        } else {
          NULL
        }

        fp <- lapply(fpFeaturesToPlot, function(marker) {
          if (!marker %in% rownames(expr_violin)) {
            alt <- gsub("-", "_", marker)
            if (!alt %in% rownames(expr_violin)) return(NULL)
          }

          cd <- cd_violin
          cd[[marker]] <- as.numeric(expr_violin[
            if (marker %in% rownames(expr_violin)) marker else gsub("-", "_", marker), ])

          # Build violin with advanced settings
          if (is.null(fpColumnToSplit)) {
            p <- ggplot(cd, aes(
              x = .data[[fpColumnToPlot]],
              y = .data[[marker]],
              fill = .data[[fpColumnToPlot]]
            ))
          } else {
            p <- ggplot(cd, aes(
              x = .data[[fpColumnToPlot]],
              y = .data[[marker]],
              fill = .data[[fpColumnToSplit]]
            ))
          }

          violin_trim <- isTRUE(input$fpViolinTrim %||% TRUE)
          violin_width <- input$fpViolinWidth %||% 0.9
          line_thickness <- input$fpViolinLineThickness %||% 0.5

          p <- p + geom_violin(
            scale = "width",
            trim = violin_trim,
            width = violin_width,
            linewidth = line_thickness
          )

          # Boxplot overlay
          if (isTRUE(input$fpViolinShowBoxplot)) {
            box_width <- input$fpViolinBarWidth %||% 0.1
            p <- p + geom_boxplot(
              width = box_width,
              outlier.size = 0.5,
              outlier.alpha = 0.5,
              linewidth = line_thickness
            )
          }

          # Median line
          if (isTRUE(input$fpViolinShowMedian)) {
            medians <- tapply(cd[[marker]], cd[[fpColumnToPlot]], median, na.rm = TRUE)
            med_df <- data.frame(
              group = names(medians),
              median_val = as.numeric(medians),
              stringsAsFactors = FALSE
            )
            colnames(med_df)[1] <- fpColumnToPlot
            med_df$xmin <- as.numeric(factor(med_df[[fpColumnToPlot]])) - violin_width / 3
            med_df$xmax <- as.numeric(factor(med_df[[fpColumnToPlot]])) + violin_width / 3
            p <- p + geom_segment(
              data = med_df,
              aes(x = xmin, xend = xmax, y = median_val, yend = median_val),
              inherit.aes = FALSE,
              colour = "black", linewidth = line_thickness * 1.5
            )
          }

          # Quartile lines
          if (isTRUE(input$fpViolinShowQuartiles)) {
            q25 <- tapply(cd[[marker]], cd[[fpColumnToPlot]],
              function(x) quantile(x, 0.25, na.rm = TRUE))
            q75 <- tapply(cd[[marker]], cd[[fpColumnToPlot]],
              function(x) quantile(x, 0.75, na.rm = TRUE))
            q_df <- data.frame(
              group = names(q25),
              q25 = as.numeric(q25),
              q75 = as.numeric(q75),
              stringsAsFactors = FALSE
            )
            colnames(q_df)[1] <- fpColumnToPlot
            q_df$xmin <- as.numeric(factor(q_df[[fpColumnToPlot]])) - violin_width / 4
            q_df$xmax <- as.numeric(factor(q_df[[fpColumnToPlot]])) + violin_width / 4
            p <- p +
              geom_segment(
                data = q_df,
                aes(x = xmin, xend = xmax, y = q25, yend = q25),
                inherit.aes = FALSE,
                colour = "grey40", linewidth = line_thickness, linetype = "dashed"
              ) +
              geom_segment(
                data = q_df,
                aes(x = xmin, xend = xmax, y = q75, yend = q75),
                inherit.aes = FALSE,
                colour = "grey40", linewidth = line_thickness, linetype = "dashed"
              )
          }

          # Colours
          colours <- inputDataReactive$Results$coloursList[[colsToViolin]]
          if (!is.null(colours)) {
            p <- p + scale_fill_manual(values = colours)
          }

          axis_angle <- input$fpViolinAxisAngle %||% 45
          base_size <- input$textSizeFP %||% 14

          p <- p +
            ggprism::theme_prism(base_size = base_size) +
            theme(
              axis.text.x = element_text(angle = axis_angle, hjust = 1),
              legend.position = "none"
            ) +
            labs(y = marker, x = NULL) +
            ggtitle(marker)

          # Facet by split column
          if (!is.null(fpColumnToSplit) && fpColumnToSplit %in% colnames(cd)) {
            p <- add_facet_with_counts(p, cd, fpColumnToSplit,
              input$ncolFPSplit %||% 1)
          }

          p
        })
        fp <- Filter(Negate(is.null), fp)

        featurePlotReactive$fp <- fp
        featurePlotReactive$needs_arrange <- length(fp) > 1
        featurePlotReactive$ncol <- input$ncolFPGene %||% 1


      # ================================================================
      # DOT PLOT
      # ================================================================
      } else if (plot_type == "Dot Plot") {

        req(fpColumnToPlot, length(fpFeaturesToPlot) > 0)

        expr_mat <- extract_expr_matrix(sce, assayToUse, fpFeaturesToPlot)
        if (nrow(expr_mat) == 0) {
          featurePlotReactive$fp <- NULL
          return(invisible(NULL))
        }

        cd <- as.data.frame(SummarizedExperiment::colData(sce))
        expr_df <- as.data.frame(t(expr_mat))
        expr_df[[fpColumnToPlot]] <- cd[[fpColumnToPlot]]
        markers_in_df <- rownames(expr_mat)[rownames(expr_mat) %in% colnames(expr_df)]

        # Scaling (cell-level or group-level)
        scaling_method <- input$fpDotPlotScaling %||% "None"
        scale_basis <- input$fpDotPlotScaleBasis %||% "cell"

        if (scaling_method == "Z-score" && scale_basis == "cell") {
          for (m in markers_in_df) {
            vals <- expr_df[[m]]
            expr_df[[m]] <- (vals - mean(vals, na.rm = TRUE)) /
              max(sd(vals, na.rm = TRUE), 1e-10)
          }
        } else if (scaling_method == "Quantile" && scale_basis == "cell") {
          for (m in markers_in_df) {
            vals <- expr_df[[m]]
            r <- rank(vals, na.last = "keep", ties.method = "average")
            expr_df[[m]] <- r / max(r, na.rm = TRUE)
          }
        }

        agg <- aggregate_expression(expr_df, markers_in_df, fpColumnToPlot)

        # Group-level scaling applied post-aggregation
        if (scaling_method == "Z-score" && scale_basis == "group") {
          for (m in colnames(agg$avg_expr)) {
            vals <- agg$avg_expr[, m]
            agg$avg_expr[, m] <- (vals - mean(vals, na.rm = TRUE)) /
              max(sd(vals, na.rm = TRUE), 1e-10)
          }
        } else if (scaling_method == "Quantile" && scale_basis == "group") {
          for (m in colnames(agg$avg_expr)) {
            vals <- agg$avg_expr[, m]
            r <- rank(vals, na.last = "keep", ties.method = "average")
            agg$avg_expr[, m] <- r / max(r, na.rm = TRUE)
          }
        }

        # Build long data frame
        dot_df <- expand.grid(
          group = rownames(agg$avg_expr),
          marker = colnames(agg$avg_expr),
          stringsAsFactors = FALSE
        )
        dot_df$avg_expr <- as.vector(agg$avg_expr)
        dot_df$pct_expr <- as.vector(agg$pct_expr)

        dot_scale <- input$fpDotPlotDotScale %||% 10
        flip <- isTRUE(input$umapFeaturePlotDotplotFlip %||% TRUE)
        hide_border <- isTRUE(input$fpDotPlotHideBorder)
        hide_legend <- isTRUE(input$fpDotPlotHideLegend)
        legend_pos <- input$fpDotPlotLegendPosition %||% "right"
        uniform_size <- isTRUE(input$fpDotPlotUniformSize)
        base_size <- input$textSizeFP %||% 14

        if (uniform_size) {
          p <- ggplot(dot_df, aes(
            x = .data[["marker"]],
            y = .data[["group"]],
            colour = .data[["avg_expr"]]
          )) +
            geom_point(size = dot_scale / 2)
        } else {
          p <- ggplot(dot_df, aes(
            x = .data[["marker"]],
            y = .data[["group"]],
            size = .data[["pct_expr"]],
            colour = .data[["avg_expr"]]
          )) +
            geom_point(stroke = if (hide_border) 0 else 0.5) +
            scale_size_continuous(range = c(0, dot_scale), name = "% Expressing")
        }

        p <- p +
          theme_classic(base_size = base_size) +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid = element_blank(),
            legend.position = if (hide_legend) "none" else legend_pos
          ) +
          labs(x = NULL, y = NULL)

        p <- apply_continuous_scale(
          p, input$viridisColourFP %||% "viridis", viridisFlip, "colour")
        if (flip) p <- p + coord_flip()

        featurePlotReactive$fp <- p
        featurePlotReactive$needs_arrange <- FALSE
        featurePlotReactive$ncol <- 1


      # ================================================================
      # HEATMAP PER CELL (ComplexHeatmap)
      # ================================================================
      } else if (plot_type == "Heatmap per cell") {

        req(fpColumnToPlot, length(fpFeaturesToPlot) > 0)

        expr_mat <- extract_expr_matrix(sce, assayToUse, fpFeaturesToPlot)
        if (ncol(expr_mat) == 0 || nrow(expr_mat) == 0) {
          featurePlotReactive$fp <- NULL
          return(invisible(NULL))
        }

        group_ids <- factor(sce[[fpColumnToPlot]])
        fp <- make_percell_heatmap(
          expr_mat = expr_mat,
          group_ids = group_ids,
          group_colours = inputDataReactive$Results$coloursList[[fpColumnToPlot]],
          palette = input$viridisColourFP %||% "viridis",
          direction = viridisFlip
        )

        featurePlotReactive$fp <- fp
        featurePlotReactive$needs_arrange <- FALSE
        featurePlotReactive$ncol <- 1


      # ================================================================
      # HEATMAP PER CLUSTER (aggregated geom_tile)
      # ================================================================
      } else if (plot_type == "Heatmap per cluster") {

        heatmap_group <- if (is.null(fpColumnToPlot)) "cluster_id" else fpColumnToPlot
        features_to_use <- if (length(fpFeaturesToPlot) > 0) {
          fpFeaturesToPlot
        } else {
          rownames(sce)
        }

        fp <- tryCatch({
          expr_mat <- extract_expr_matrix(sce, assayToUse, features_to_use)
          cd <- as.data.frame(SummarizedExperiment::colData(sce))
          df <- as.data.frame(t(expr_mat))
          df[[heatmap_group]] <- cd[[heatmap_group]]
          markers_in_df <- rownames(expr_mat)[rownames(expr_mat) %in% colnames(df)]
          agg_result <- aggregate_expression(df, markers_in_df, heatmap_group)
          mat <- agg_result$avg_expr
          mat[is.na(mat)] <- 0

          # Scaling
          scaling_method <- input$umapFeaturePlotHeatmapScaling %||% "None"
          scale_basis <- input$umapFeaturePlotHeatmapScaleBasis %||% "cell"

          if (scaling_method == "Z-score") {
            if (scale_basis == "group") {
              # Z-score per marker across groups
              for (m in colnames(mat)) {
                vals <- mat[, m]
                mat[, m] <- (vals - mean(vals, na.rm = TRUE)) /
                  max(sd(vals, na.rm = TRUE), 1e-10)
              }
            } else {
              # Cell-level z-score: re-aggregate from scaled cell data
              scaled_df <- df
              for (m in markers_in_df) {
                vals <- scaled_df[[m]]
                scaled_df[[m]] <- (vals - mean(vals, na.rm = TRUE)) /
                  max(sd(vals, na.rm = TRUE), 1e-10)
              }
              agg_scaled <- aggregate_expression(scaled_df, markers_in_df, heatmap_group)
              mat <- agg_scaled$avg_expr
              mat[is.na(mat)] <- 0
            }
          } else if (scaling_method == "Quantile") {
            if (scale_basis == "group") {
              for (m in colnames(mat)) {
                vals <- mat[, m]
                r <- rank(vals, na.last = "keep", ties.method = "average")
                mat[, m] <- r / max(r, na.rm = TRUE)
              }
            } else {
              scaled_df <- df
              for (m in markers_in_df) {
                vals <- scaled_df[[m]]
                r <- rank(vals, na.last = "keep", ties.method = "average")
                scaled_df[[m]] <- r / max(r, na.rm = TRUE)
              }
              agg_scaled <- aggregate_expression(scaled_df, markers_in_df, heatmap_group)
              mat <- agg_scaled$avg_expr
              mat[is.na(mat)] <- 0
            }
          }

          # Hierarchical clustering
          do_cluster <- isTRUE(input$umapFeaturePlotHeatmapCluster %||% TRUE)
          if (do_cluster && nrow(mat) > 1) {
            row_ord <- rownames(mat)[hclust(dist(mat, "euclidean"), "ward.D")$order]
          } else {
            row_ord <- rownames(mat)
          }
          if (do_cluster && ncol(mat) > 1) {
            col_ord <- colnames(mat)[hclust(dist(t(mat), "euclidean"), "ward.D")$order]
          } else {
            col_ord <- colnames(mat)
          }

          # Reshape to long format
          long <- reshape2::melt(mat, varnames = c("group", "feature"),
            value.name = "mean")
          long$group <- factor(long$group, levels = row_ord)
          long$feature <- factor(long$feature, levels = rev(col_ord))

          # Custom colour limits
          if (isTRUE(input$heatmapClusterCustomLimits)) {
            lo <- input$heatmapClusterLimitLow %||% -2
            hi <- input$heatmapClusterLimitHigh %||% 2
            long$mean <- pmax(lo, pmin(hi, long$mean))
          }

          do_flip <- isTRUE(input$umapFeaturePlotHeatmapFlip %||% TRUE)
          base_size <- input$textSizeFP %||% 14

          if (do_flip) {
            p <- ggplot(long, aes(x = .data$group, y = .data$feature,
              fill = .data$mean)) +
              labs(x = heatmap_group, y = NULL)
          } else {
            p <- ggplot(long, aes(x = .data$feature, y = .data$group,
              fill = .data$mean)) +
              labs(x = NULL, y = heatmap_group)
          }

          p <- p +
            geom_tile(color = "white", linewidth = 0.5) +
            scale_x_discrete(expand = c(0, 0), position = "top") +
            scale_y_discrete(expand = c(0, 0)) +
            coord_equal() +
            theme_minimal(base_size = base_size) +
            theme(
              axis.text.x.top = element_text(angle = 45, hjust = 0, vjust = 0),
              axis.text.x.bottom = element_blank(),
              axis.ticks.x.bottom = element_blank(),
              panel.grid = element_blank(),
              panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
              legend.position = "bottom",
              plot.background = element_rect(fill = "white", color = "white"),
              panel.background = element_rect(fill = "white", color = "white")
            )

          apply_continuous_scale(
            p, input$viridisColourFP %||% "viridis", viridisFlip, "fill")

        }, error = function(e) {
          showNotification(paste("Heatmap error:", e$message), type = "error")
          NULL
        })

        featurePlotReactive$fp <- fp
        featurePlotReactive$needs_arrange <- FALSE
        featurePlotReactive$ncol <- 1


      # ================================================================
      # RIDGE PLOT
      # ================================================================
      } else if (plot_type == "Ridge Plot") {

        req(fpColumnToPlot, length(fpFeaturesToPlot) > 0)

        hide_legend <- isTRUE(input$fpRidgePlotHideLegend %||% TRUE)
        legend_pos <- if (hide_legend) {
          "none"
        } else {
          input$fpRidgePlotLegendPosition %||% "none"
        }

        # Batch-extract expression + colData once (avoid per-marker S4 dispatch)
        cd_ridge <- as.data.frame(SummarizedExperiment::colData(sce))
        ridge_row_idx <- match(fpFeaturesToPlot, rownames(sce))
        na_pos_r <- is.na(ridge_row_idx)
        if (any(na_pos_r)) {
          ridge_row_idx[na_pos_r] <- match(gsub("-", "_", fpFeaturesToPlot[na_pos_r]), rownames(sce))
        }
        valid_ridge <- !is.na(ridge_row_idx)
        expr_ridge <- if (any(valid_ridge)) {
          SummarizedExperiment::assay(sce, assayToUse)[ridge_row_idx[valid_ridge], , drop = FALSE]
        } else {
          NULL
        }

        fp <- lapply(fpFeaturesToPlot, function(marker) {
          if (!marker %in% rownames(expr_ridge)) {
            alt <- gsub("-", "_", marker)
            if (!alt %in% rownames(expr_ridge)) return(NULL)
          }

          cd <- cd_ridge
          cd[[marker]] <- as.numeric(expr_ridge[
            if (marker %in% rownames(expr_ridge)) marker else gsub("-", "_", marker), ])

          colours <- inputDataReactive$Results$coloursList[[fpColumnToPlot]]
          base_size <- input$textSizeFP %||% 14

          p <- ggplot(cd, aes(
            x = .data[[marker]],
            y = .data[[fpColumnToPlot]],
            fill = .data[[fpColumnToPlot]]
          )) +
            ggridges::geom_density_ridges(scale = 1.2, alpha = 0.7) +
            ggprism::theme_prism(base_size = base_size) +
            theme(legend.position = legend_pos) +
            labs(x = marker, y = NULL) +
            ggtitle(marker)

          if (!is.null(colours)) {
            p <- p + scale_fill_manual(values = colours)
          }

          # Facet by split column
          if (!is.null(fpColumnToSplit) && fpColumnToSplit %in% colnames(cd)) {
            p <- add_facet_with_counts(p, cd, fpColumnToSplit,
              input$ncolFPSplit %||% 1)
          }

          p
        })
        fp <- Filter(Negate(is.null), fp)

        featurePlotReactive$fp <- fp
        featurePlotReactive$needs_arrange <- length(fp) > 1
        featurePlotReactive$ncol <- input$ncolFPGene %||% 1


      } # end plot type switch

    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error")) {
        showNotification(conditionMessage(e), type = "error")
      }
    })
  }
)


# ── 8. Render Feature Plot Output ───────────────────────────────────────────
output$umapFeaturePlotOutput <- renderPlot(
  {
    fp <- featurePlotReactive$fp
    if (is.null(fp)) return(NULL)

    # ComplexHeatmap objects need draw()
    if (inherits(fp, "Heatmap") || inherits(fp, "HeatmapList")) {
      ComplexHeatmap::draw(fp)

    # Multi-plot: patchwork assembly
    } else if (isTRUE(isolate(featurePlotReactive$needs_arrange)) && is.list(fp)) {
      fp_clean <- Filter(Negate(is.null), fp)
      if (length(fp_clean) == 0) return(NULL)
      patchwork::wrap_plots(fp_clean,
        ncol = isolate(featurePlotReactive$ncol) %||% 1)

    # Single plot in a list
    } else if (is.list(fp) && length(fp) == 1) {
      fp[[1]]

    # Direct ggplot
    } else {
      fp
    }
  },
  height = function() input$figHeightFP %||% 500,
  width = function() input$figWidthFP %||% 650
)
