# ── server-analysis.R ────────────────────────────────────────────────────────
# Analysis plots module for MARMOT Shiny app.
# Renders pipeline report plots: histograms, clustree, MDS, marker pairs,
# cluster frequency boxplots, cluster abundances, and marker boxplots.
# ─────────────────────────────────────────────────────────────────────────────

# ── Dynamic settings UI ────────────────────────────────────────────────────
observeEvent(input$analysisPlotType, {
  req(inputDataReactive$Results)
  res         <- inputDataReactive$Results
  sce         <- res$sce
  coloursList <- res$coloursList

  output$analysisSettingsUI <- renderUI({
    # Shared settings for all analysis plots
    shared_settings <- tagList(
      hr(style = "border-top: 1px solid #000000; margin-top: 10px;"),
      h5("Layout", style = "margin-bottom: 4px;"),
      splitLayout(
        sliderInput("analysisFigWidth", "Width (px)",
          min = 200, max = 2000, value = 700, step = 50,
          width = "85%", ticks = FALSE),
        sliderInput("analysisFigHeight", "Height (px)",
          min = 200, max = 2000, value = 700, step = 50,
          width = "85%", ticks = FALSE),
        sliderInput("analysisTextSize", "Font size",
          min = 8, max = 30, value = 14, step = 1,
          width = "85%", ticks = FALSE)
      )
    )

    plot_specific <- switch(input$analysisPlotType,

      # ── Cofactor Histograms ──────────────────────────────────────────────
      "Cofactor Histograms" = {
        assay_choices <- SummarizedExperiment::assayNames(sce)
        # Prefer user-friendly labels
        assay_labels <- c(
          "exprsTransformed" = "Arcsinh Transformed",
          "exprsQuantNorm"   = "Quantile Normalised",
          "norm"             = "Z-Scaled",
          "counts"           = "Raw Counts"
        )
        available <- intersect(names(assay_labels), assay_choices)
        choices   <- setNames(available, assay_labels[available])
        # Append any assays not in the label map
        extra <- setdiff(assay_choices, names(assay_labels))
        if (length(extra) > 0) choices <- c(choices, setNames(extra, extra))

        tagList(
          selectInput(
            inputId  = "analysisHistAssay",
            label    = "Expression assay",
            choices  = choices,
            selected = if ("exprsTransformed" %in% assay_choices) "exprsTransformed" else assay_choices[1]
          ),
          checkboxInput(
            inputId = "analysisHistFacetCondition",
            label   = "Facet by condition",
            value   = TRUE
          )
        )
      },

      # ── Clustree ─────────────────────────────────────────────────────────
      "Clustree" = {
        cd_names <- colnames(SummarizedExperiment::colData(sce))
        prefixes <- unique(gsub("[0-9]+$", "", cd_names[grep("^(meta|k|p)[0-9]+$", cd_names)]))
        if (length(prefixes) == 0) prefixes <- "meta"

        tagList(
          selectInput(
            inputId  = "analysisClustreePrefix",
            label    = "Clustering prefix",
            choices  = prefixes,
            selected = prefixes[1]
          ),
          sliderInput(
            inputId = "analysisClustreeHeight",
            label   = "Figure height (px)",
            min     = 400,
            max     = 2000,
            value   = 800,
            step    = 50,
            ticks   = FALSE
          )
        )
      },

      # ── Pseudo-bulk MDS ──────────────────────────────────────────────────
      "Pseudo-bulk MDS" = {
        condition_cols <- res$conditions
        if (is.null(condition_cols) || length(condition_cols) == 0)
          condition_cols <- "condition"

        tagList(
          selectInput(
            inputId  = "analysisMDSColourBy",
            label    = "Colour by",
            choices  = condition_cols,
            selected = condition_cols[1]
          ),
          checkboxInput(
            inputId = "analysisMDSShowLabels",
            label   = "Show sample labels?",
            value   = TRUE
          ),
          sliderInput(
            inputId = "analysisMDSPointSize",
            label   = "Point size",
            min     = 1, max = 15, value = 5, step = 0.5,
            ticks   = FALSE
          ),
          checkboxInput(
            inputId = "analysisMDSFixedCoord",
            label   = "Fixed aspect ratio (1:1)",
            value   = TRUE
          )
        )
      },

      # ── Marker Pair Scatter ──────────────────────────────────────────────
      "Marker Pair Scatter" = {
        mp <- parse_marker_pairs(res$smd)
        all_markers <- gtools::mixedsort(rownames(sce))

        assay_choices <- SummarizedExperiment::assayNames(sce)
        assay_labels <- c(
          "exprsTransformed" = "Arcsinh Transformed",
          "exprsQuantNorm"   = "Quantile Normalised",
          "norm"             = "Z-Scaled",
          "counts"           = "Raw Counts"
        )
        available <- intersect(names(assay_labels), assay_choices)
        choices   <- setNames(available, assay_labels[available])
        extra     <- setdiff(assay_choices, names(assay_labels))
        if (length(extra) > 0) choices <- c(choices, setNames(extra, extra))

        tagList(
          radioButtons(
            inputId  = "analysisMarkerPairMode",
            label    = "Selection mode",
            choices  = c("Cell type pairs" = "celltype",
                         "Individual markers" = "individual"),
            selected = if (!is.null(mp)) "celltype" else "individual",
            inline   = TRUE
          ),
          conditionalPanel(
            condition = "input.analysisMarkerPairMode == 'celltype'",
            if (!is.null(mp)) {
              selectInput(
                inputId  = "analysisMarkerPairType",
                label    = "Cell type pair",
                choices  = mp$types,
                selected = mp$types[1]
              )
            } else {
              tags$p(style = "color: #dc2626; font-size: 0.85rem;",
                "No 'Marker Pairs' column found in study metadata.")
            }
          ),
          conditionalPanel(
            condition = "input.analysisMarkerPairMode == 'individual'",
            selectizeInput(
              inputId  = "analysisMarkerX",
              label    = "X-axis marker",
              choices  = all_markers,
              selected = all_markers[1]
            ),
            selectizeInput(
              inputId  = "analysisMarkerY",
              label    = "Y-axis marker",
              choices  = all_markers,
              selected = if (length(all_markers) > 1) all_markers[2] else all_markers[1]
            )
          ),
          selectInput(
            inputId  = "analysisMarkerPairAssay",
            label    = "Expression assay",
            choices  = choices,
            selected = if ("exprsTransformed" %in% assay_choices) "exprsTransformed" else assay_choices[1]
          ),
          sliderInput(
            inputId = "analysisMarkerPairPointSize",
            label   = "Point size",
            min     = 0.1, max = 3, value = 0.3, step = 0.1,
            ticks   = FALSE
          ),
          sliderInput(
            inputId = "analysisMarkerPairMaxCells",
            label   = "Max cells to plot",
            min     = 1000, max = 100000, value = 20000, step = 1000,
            ticks   = FALSE
          )
        )
      },

      # ── Cluster Frequency Boxplots ───────────────────────────────────────
      "Cluster Frequency Boxplots" = {
        condition_cols <- res$conditions
        if (is.null(condition_cols) || length(condition_cols) == 0)
          condition_cols <- "condition"

        tagList(
          selectInput(
            inputId  = "analysisFreqCondition",
            label    = "Condition column",
            choices  = condition_cols,
            selected = condition_cols[1]
          ),
          sliderInput(
            inputId = "analysisFreqNcol",
            label   = "Columns",
            min     = 1,
            max     = 10,
            value   = 4,
            step    = 1,
            ticks   = FALSE
          )
        )
      },

      # ── Cluster Abundances ──────────────────────────────────────────────
      "Cluster Abundances" = {
        tagList(
          selectInput(
            inputId  = "analysisAbundanceBy",
            label    = "Group by",
            choices  = c("sample_id", "cluster_id"),
            selected = "sample_id"
          )
        )
      },

      # ── Marker Boxplots ─────────────────────────────────────────────────
      "Marker Boxplots" = {
        condition_cols <- res$conditions
        if (is.null(condition_cols) || length(condition_cols) == 0)
          condition_cols <- "condition"

        tagList(
          selectInput(
            inputId  = "analysisMarkerBPCondition",
            label    = "Condition column",
            choices  = condition_cols,
            selected = condition_cols[1]
          ),
          sliderInput(
            inputId = "analysisMarkerBPNcol",
            label   = "Columns",
            min     = 1,
            max     = 10,
            value   = 4,
            step    = 1,
            ticks   = FALSE
          )
        )
      },

      # ── Marker per Cluster Boxplot ────────────────────────────────────
      "Marker per Cluster Boxplot" = {
        all_markers <- gtools::mixedsort(rownames(sce))
        plottable   <- get_plottable_columns(sce)
        condition_cols <- res$conditions
        if (is.null(condition_cols) || length(condition_cols) == 0)
          condition_cols <- "condition"

        tagList(
          selectizeInput(
            inputId  = "analysisClusterBPMarkers",
            label    = "Select markers",
            choices  = all_markers,
            selected = all_markers[1:min(4, length(all_markers))],
            multiple = TRUE,
            options  = list(plugins = list("remove_button"))
          ),
          selectInput(
            inputId  = "analysisClusterBPGroup",
            label    = "Cluster column",
            choices  = plottable,
            selected = if ("cluster_id" %in% plottable) "cluster_id" else plottable[1]
          ),
          selectInput(
            inputId  = "analysisClusterBPCondition",
            label    = "Colour by condition",
            choices  = c("None", condition_cols),
            selected = condition_cols[1]
          ),
          sliderInput(
            inputId = "analysisClusterBPNcol",
            label   = "Columns",
            min     = 1, max = 10, value = 2, step = 1,
            ticks   = FALSE
          )
        )
      },

      # ── Abundance Barplot ──────────────────────────────────────────────
      "Abundance Barplot" = {
        plottable <- get_plottable_columns(sce)
        tagList(
          selectInput(
            inputId  = "analysisBarplotFill",
            label    = "Fill by",
            choices  = plottable,
            selected = if ("cluster_id" %in% plottable) "cluster_id" else plottable[1]
          ),
          selectInput(
            inputId  = "analysisBarplotX",
            label    = "X-axis (group by)",
            choices  = c("(same as fill)" = "", plottable),
            selected = ""
          ),
          checkboxInput("analysisBarplotPercentage", "Show proportions?", value = FALSE),
          checkboxInput("analysisBarplotShowNumbers", "Show count labels?", value = FALSE)
        )
      }
    ) # end switch

    tagList(plot_specific, shared_settings)
  }) # end renderUI
}, ignoreNULL = TRUE)


# ── Reactive: build the analysis plot ──────────────────────────────────────
analysisPlotReactive <- reactive({
  req(inputDataReactive$Results)
  req(input$analysisPlotType)

  res         <- inputDataReactive$Results
  sce         <- res$sce
  coloursList <- res$coloursList

  # Force reactive dependency on all dynamic settings inputs so that
  # changes in renderUI-created widgets always trigger re-evaluation.
  # (Reads return NULL harmlessly when the widget doesn't exist yet.)
  input$analysisMDSFixedCoord
  input$analysisMDSPointSize
  input$analysisMDSColourBy
  input$analysisMDSShowLabels
  input$analysisMarkerPairPointSize
  input$analysisMarkerPairMode
  input$analysisMarkerPairAssay
  input$analysisClustreePrefix

  switch(input$analysisPlotType,

    # ════════════════════════════════════════════════════════════════════════
    # 1. Cofactor Histograms
    # ════════════════════════════════════════════════════════════════════════
    "Cofactor Histograms" = {
      assay_name <- input$analysisHistAssay %||% "exprsTransformed"
      facet_cond <- isTRUE(input$analysisHistFacetCondition)

      tryCatch({
        expr_mat <- as.matrix(SummarizedExperiment::assay(sce, assay_name))
        cd       <- as.data.frame(SummarizedExperiment::colData(sce))
        df       <- data.frame(t(expr_mat), cd, check.names = FALSE)

        id_vars <- names(cd)
        long    <- reshape2::melt(df,
          value.name  = "value",
          variable.name = "antigen",
          id.vars     = id_vars
        )

        if (facet_cond && "condition" %in% colnames(cd)) {
          cond_colours <- coloursList$condition
          p <- ggplot2::ggplot(long,
            ggplot2::aes(x = value, y = ggplot2::after_stat(ndensity),
                         fill = condition, color = condition)) +
            ggplot2::geom_density(alpha = 0.1) +
            ggplot2::facet_wrap(~antigen, scales = "free") +
            ggplot2::ylab("Normalised Density") +
            ggplot2::xlab("Transformed Value") +
            ggprism::theme_prism(base_size = as.integer(input$analysisTextSize %||% 14)) +
            ggplot2::theme(
              strip.background = ggplot2::element_blank(),
              strip.text       = ggplot2::element_text(face = "bold")
            )
          if (!is.null(cond_colours)) {
            p <- p +
              ggplot2::scale_colour_manual(values = cond_colours) +
              ggplot2::scale_fill_manual(values = cond_colours)
          }
        } else {
          p <- ggplot2::ggplot(long,
            ggplot2::aes(x = value, y = ggplot2::after_stat(ndensity))) +
            ggplot2::geom_density(alpha = 0.3, fill = "#3f3f46", colour = "#3f3f46") +
            ggplot2::facet_wrap(~antigen, scales = "free") +
            ggplot2::ylab("Normalised Density") +
            ggplot2::xlab("Transformed Value") +
            ggprism::theme_prism(base_size = as.integer(input$analysisTextSize %||% 14)) +
            ggplot2::theme(
              strip.background = ggplot2::element_blank(),
              strip.text       = ggplot2::element_text(face = "bold")
            )
        }
        p
      }, error = function(e) {
        showNotification(
          paste("Cofactor Histograms error:", e$message),
          type = "error", duration = 8
        )
        NULL
      })
    },

    # ════════════════════════════════════════════════════════════════════════
    # 2. Clustree
    # ════════════════════════════════════════════════════════════════════════
    "Clustree" = {
      tryCatch({
        selected_prefix <- input$analysisClustreePrefix %||% "meta"

        # fast_clustree (plot_helpers.R) uses vectorized table() and skips
        # the O(r²c²n) SC3 stability calc — typically 10-50× faster.
        p <- fast_clustree(sce, prefix = selected_prefix)
        p
      }, error = function(e) {
        showNotification(
          paste("Clustree error:", e$message),
          type = "error", duration = 8
        )
        NULL
      })
    },

    # ════════════════════════════════════════════════════════════════════════
    # 3. Pseudo-bulk MDS
    # ════════════════════════════════════════════════════════════════════════
    "Pseudo-bulk MDS" = {
      tryCatch({
        if (!requireNamespace("CATALYST", quietly = TRUE)) {
          showNotification("CATALYST package is not installed.", type = "error")
          return(NULL)
        }

        colour_by    <- input$analysisMDSColourBy %||% "condition"
        cond_colours <- coloursList[[colour_by]]
        base_size    <- as.integer(input$analysisTextSize %||% 14)
        pt_size      <- as.numeric(input$analysisMDSPointSize %||% 5)
        fixed_coord  <- isTRUE(input$analysisMDSFixedCoord)

        p <- CATALYST::pbMDS(
          x        = sce,
          color_by = colour_by,
          size_by  = FALSE,
          features = "type"
        ) +
          ggprism::theme_prism(base_size = base_size) +
          ggplot2::theme(panel.grid = ggplot2::element_blank())

        # Override point size with user slider (pbMDS sets its own)
        for (i in seq_along(p$layers)) {
          if (inherits(p$layers[[i]]$geom, "GeomPoint")) {
            p$layers[[i]]$aes_params$size <- pt_size
            break
          }
        }

        if (!isTRUE(input$analysisMDSShowLabels)) {
          # Remove text/label layers (sample labels from CATALYST)
          p$layers <- p$layers[vapply(p$layers, function(l) {
            !inherits(l$geom, "GeomText") && !inherits(l$geom, "GeomTextRepel")
          }, logical(1))]
        }

        if (!is.null(cond_colours)) {
          p <- p + ggplot2::scale_colour_manual(values = cond_colours)
        }
        # CATALYST::pbMDS bakes in coord_equal(); replace with
        # coord_cartesian() when the user wants a free aspect ratio.
        if (!fixed_coord) {
          p <- p + ggplot2::coord_cartesian()
        }
        p
      }, error = function(e) {
        showNotification(
          paste("Pseudo-bulk MDS error:", e$message),
          type = "error", duration = 8
        )
        NULL
      })
    },

    # ════════════════════════════════════════════════════════════════════════
    # 4. Marker Pair Scatter
    # ════════════════════════════════════════════════════════════════════════
    "Marker Pair Scatter" = {
      tryCatch({
        pair_mode  <- input$analysisMarkerPairMode %||% "celltype"
        assay_name <- input$analysisMarkerPairAssay %||% "exprsTransformed"
        base_size  <- as.integer(input$analysisTextSize %||% 14)

        if (pair_mode == "individual") {
          markers <- c(input$analysisMarkerX, input$analysisMarkerY)
          req(length(markers) == 2, all(nzchar(markers)))
          plot_title <- paste(markers[1], "vs", markers[2])
        } else {
          mp <- parse_marker_pairs(res$smd)
          if (is.null(mp)) {
            showNotification("No marker pairs found in study metadata.", type = "warning")
            return(NULL)
          }
          selected_type <- input$analysisMarkerPairType %||% mp$types[1]
          if (!selected_type %in% names(mp$models)) {
            showNotification(paste("Unknown pair type:", selected_type), type = "warning")
            return(NULL)
          }
          markers <- mp$models[[selected_type]]
          if (length(markers) < 2) {
            showNotification(
              paste("Need exactly 2 markers for scatter, got:", length(markers)),
              type = "warning"
            )
            return(NULL)
          }
          plot_title <- selected_type
        }

        expr_mat <- extract_expr_matrix(sce, assay_name, markers)
        if (nrow(expr_mat) == 0) {
          showNotification("Selected markers not found in expression data.", type = "warning")
          return(NULL)
        }
        # Use the actual rownames after matching (in case of - vs _ substitution)
        markers <- rownames(expr_mat)

        expr_df            <- data.frame(t(expr_mat), check.names = FALSE)
        expr_df$cluster_id <- as.character(SummarizedExperiment::colData(sce)$cluster_id)

        # Downsample for performance
        max_cells <- as.numeric(input$analysisMarkerPairMaxCells %||% 20000)
        if (nrow(expr_df) > max_cells) {
          expr_df <- expr_df[sample(nrow(expr_df), max_cells), ]
        }

        # Compute cluster medians for labels
        clusters  <- unique(expr_df$cluster_id)
        median_df <- do.call(rbind, lapply(clusters, function(cl) {
          idx <- expr_df$cluster_id == cl
          data.frame(
            cluster_id = cl,
            x          = median(expr_df[[markers[1]]][idx], na.rm = TRUE),
            y          = median(expr_df[[markers[2]]][idx], na.rm = TRUE),
            stringsAsFactors = FALSE
          )
        }))

        clr_vals <- coloursList$cluster_id
        p <- ggplot2::ggplot(expr_df,
          ggplot2::aes(
            x      = .data[[markers[1]]],
            y      = .data[[markers[2]]],
            color  = cluster_id
          )) +
          ggplot2::geom_point(
            size  = as.numeric(input$analysisMarkerPairPointSize %||% 0.3),
            alpha = 0.5
          ) +
          ggprism::theme_prism(base_size = base_size) +
          ggplot2::theme(legend.position = "none") +
          ggplot2::ggtitle(plot_title)

        if (!is.null(clr_vals)) {
          p <- p + ggplot2::scale_color_manual(values = clr_vals)
        }

        if (requireNamespace("ggrepel", quietly = TRUE)) {
          p <- p + ggrepel::geom_label_repel(
            data         = median_df,
            ggplot2::aes(x = x, y = y, label = cluster_id, fill = cluster_id),
            colour       = "black",
            show.legend  = FALSE,
            size         = 4,
            max.overlaps = 20
          )
          if (!is.null(clr_vals)) {
            p <- p + ggplot2::scale_fill_manual(values = clr_vals)
          }
        }

        p
      }, error = function(e) {
        showNotification(
          paste("Marker Pair Scatter error:", e$message),
          type = "error", duration = 8
        )
        NULL
      })
    },

    # ════════════════════════════════════════════════════════════════════════
    # 5. Cluster Frequency Boxplots
    # ════════════════════════════════════════════════════════════════════════
    "Cluster Frequency Boxplots" = {
      tryCatch({
        condition_col <- input$analysisFreqCondition %||% "condition"
        ncol_setting  <- input$analysisFreqNcol %||% 4L
        base_size     <- as.integer(input$analysisTextSize %||% 12)

        cd <- as.data.frame(SummarizedExperiment::colData(sce))

        if (!"sample_id" %in% colnames(cd)) {
          showNotification("No 'sample_id' column found in cell metadata.", type = "warning")
          return(NULL)
        }

        # Compute cluster frequencies per sample
        ns  <- table(cd$cluster_id, cd$sample_id)
        fq  <- prop.table(ns, 2)
        fq_mat <- as.matrix(unclass(fq))

        # Z-normalise arcsin-sqrt transformed frequencies
        fq_asin <- asin(sqrt(fq_mat))
        col_means <- colMeans(fq_asin, na.rm = TRUE)
        col_sds   <- apply(fq_asin, 2, stats::sd, na.rm = TRUE)
        col_sds[col_sds == 0] <- 1
        fq_z <- scale(t(fq_asin))

        boxplot_df <- as.data.frame(fq_z)
        boxplot_df <- tibble::rownames_to_column(boxplot_df, "sample_id")

        # Join with sample metadata for condition grouping
        if (!is.null(res$md)) {
          md_join <- res$md
          # Ensure condition_col exists (may be dot-converted)
          if (!condition_col %in% colnames(md_join)) {
            alt_col <- gsub("\\.", "-", condition_col)
            if (alt_col %in% colnames(md_join)) {
              colnames(md_join)[colnames(md_join) == alt_col] <- condition_col
            }
          }
          boxplot_df <- merge(boxplot_df, md_join, by = "sample_id", all.x = TRUE)
        }

        if (!condition_col %in% colnames(boxplot_df)) {
          showNotification(
            paste("Condition column '", condition_col, "' not found in metadata."),
            type = "warning"
          )
          return(NULL)
        }

        clusters <- gtools::mixedsort(unique(as.character(cd$cluster_id)))
        # Filter to clusters that are valid column names in boxplot_df
        clusters <- intersect(clusters, colnames(boxplot_df))
        if (length(clusters) == 0) {
          showNotification("No cluster frequency data could be computed.", type = "warning")
          return(NULL)
        }

        cond_colours <- coloursList[[condition_col]] %||% coloursList$condition

        plots <- lapply(clusters, function(k) {
          p <- ggplot2::ggplot(boxplot_df,
            ggplot2::aes(x = .data[[condition_col]], y = .data[[k]])) +
            ggplot2::geom_boxplot(
              ggplot2::aes(fill = .data[[condition_col]]),
              alpha = 0.2, outlier.shape = NA
            ) +
            ggprism::theme_prism(base_size = base_size) +
            ggplot2::ggtitle(paste("Cluster", k)) +
            ggplot2::theme(
              legend.position = "none",
              axis.title      = ggplot2::element_blank()
            )

          if (requireNamespace("ggbeeswarm", quietly = TRUE)) {
            p <- p + ggbeeswarm::geom_beeswarm(
              ggplot2::aes(fill = .data[[condition_col]]),
              pch = 21, size = 3, stroke = 0.3
            )
          } else {
            p <- p + ggplot2::geom_jitter(
              ggplot2::aes(fill = .data[[condition_col]]),
              pch = 21, size = 3, stroke = 0.3, width = 0.2
            )
          }

          if (!is.null(cond_colours)) {
            p <- p + ggplot2::scale_fill_manual(values = cond_colours)
          }
          p
        })

        patchwork::wrap_plots(plots, ncol = as.integer(ncol_setting))
      }, error = function(e) {
        showNotification(
          paste("Cluster Frequency Boxplots error:", e$message),
          type = "error", duration = 8
        )
        NULL
      })
    },

    # ════════════════════════════════════════════════════════════════════════
    # 6. Cluster Abundances
    # ════════════════════════════════════════════════════════════════════════
    "Cluster Abundances" = {
      tryCatch({
        if (!requireNamespace("CATALYST", quietly = TRUE)) {
          showNotification("CATALYST package is not installed.", type = "error")
          return(NULL)
        }

        by_value <- input$analysisAbundanceBy %||% "sample_id"

        # CATALYST::plotAbundances requires cluster info in metadata
        clusteringMethodToUse <- res$clusteringMethodToUse
        if (is.null(clusteringMethodToUse)) {
          showNotification(
            "Clustering method not found in pipeline settings. Cannot plot abundances.",
            type = "warning"
          )
          return(NULL)
        }

        # Derive the merging column prefix
        mergeBy <- switch(clusteringMethodToUse,
          "Rphenograph" = "k", "Mphenograph" = "k", "MfastPG" = "k",
          "PARC" = "p", "FlowSOM" = "meta",
          "meta"
        )

        # Validate: check mergeBy exists in cluster_codes
        valid_codes <- tryCatch(
          names(CATALYST::cluster_codes(sce)),
          error = function(e) character(0)
        )
        if (!mergeBy %in% valid_codes && length(valid_codes) > 0) {
          mergeBy <- valid_codes[1]
        }

        base_size <- as.integer(input$analysisTextSize %||% 14)
        p <- CATALYST::plotAbundances(sce, k = mergeBy, by = by_value) +
          ggprism::theme_prism(base_size = base_size)

        # Fix lexicographic cluster ordering (1,10,2,20 → 1,2,10,20)
        if ("cluster_id" %in% colnames(p$data)) {
          p$data$cluster_id <- factor(p$data$cluster_id,
            levels = gtools::mixedsort(unique(as.character(p$data$cluster_id))))
        }

        # Apply correct cluster colours
        clr_vals <- coloursList$cluster_id
        if (!is.null(clr_vals)) {
          p <- p + ggplot2::scale_colour_manual(values = clr_vals, aesthetics = c("colour", "fill"))
        }

        # Add condition faceting if sample_id grouping and condition exists
        if (by_value == "sample_id" && "condition" %in% colnames(SummarizedExperiment::colData(sce))) {
          p <- p + ggplot2::facet_grid(~condition, space = "free", scales = "free")
        }
        p
      }, error = function(e) {
        showNotification(
          paste("Cluster Abundances error:", e$message),
          type = "error", duration = 8
        )
        NULL
      })
    },

    # ════════════════════════════════════════════════════════════════════════
    # 7. Marker Boxplots
    # ════════════════════════════════════════════════════════════════════════
    "Marker Boxplots" = {
      tryCatch({
        condition_col <- input$analysisMarkerBPCondition %||% "condition"
        ncol_setting  <- input$analysisMarkerBPNcol %||% 4L
        base_size     <- as.integer(input$analysisTextSize %||% 12)

        cd <- as.data.frame(SummarizedExperiment::colData(sce))

        if (!"sample_id" %in% colnames(cd)) {
          showNotification("No 'sample_id' column found in cell metadata.", type = "warning")
          return(NULL)
        }

        # Compute median expression per sample per marker
        assay_name <- "exprsQuantNorm"
        avail      <- SummarizedExperiment::assayNames(sce)
        if (!assay_name %in% avail) assay_name <- avail[1]

        expr_mat <- as.matrix(SummarizedExperiment::assay(sce, assay_name))
        sample_ids <- cd$sample_id

        # Median per marker per sample
        median_per_sample <- t(apply(expr_mat, 1, function(x) {
          tapply(x, sample_ids, median, na.rm = TRUE)
        }))

        median_df <- as.data.frame(t(median_per_sample))
        median_df <- tibble::rownames_to_column(median_df, "sample_id")

        # Join with sample metadata for condition grouping
        if (!is.null(res$md)) {
          md_join <- res$md
          if (!condition_col %in% colnames(md_join)) {
            alt_col <- gsub("\\.", "-", condition_col)
            if (alt_col %in% colnames(md_join)) {
              colnames(md_join)[colnames(md_join) == alt_col] <- condition_col
            }
          }
          median_df <- merge(median_df, md_join, by = "sample_id", all.x = TRUE)
        }

        if (!condition_col %in% colnames(median_df)) {
          showNotification(
            paste("Condition column '", condition_col, "' not found in metadata."),
            type = "warning"
          )
          return(NULL)
        }

        markers      <- rownames(sce)
        cond_colours <- coloursList[[condition_col]] %||% coloursList$condition

        plots <- lapply(markers, function(m) {
          # Skip if marker column not present (edge case with special characters)
          if (!m %in% colnames(median_df)) return(NULL)

          p <- ggplot2::ggplot(median_df,
            ggplot2::aes(x = .data[[condition_col]], y = .data[[m]])) +
            ggplot2::geom_boxplot(
              ggplot2::aes(fill = .data[[condition_col]]),
              alpha = 0.2, outlier.shape = NA
            ) +
            ggprism::theme_prism(base_size = base_size) +
            ggplot2::ggtitle(m) +
            ggplot2::theme(
              legend.position = "none",
              axis.title      = ggplot2::element_blank()
            )

          if (requireNamespace("ggbeeswarm", quietly = TRUE)) {
            p <- p + ggbeeswarm::geom_beeswarm(
              ggplot2::aes(fill = .data[[condition_col]]),
              pch = 21, size = 3, stroke = 0.3
            )
          } else {
            p <- p + ggplot2::geom_jitter(
              ggplot2::aes(fill = .data[[condition_col]]),
              pch = 21, size = 3, stroke = 0.3, width = 0.2
            )
          }

          if (!is.null(cond_colours)) {
            p <- p + ggplot2::scale_fill_manual(values = cond_colours)
          }
          p
        })

        # Remove NULLs from failed markers
        plots <- Filter(Negate(is.null), plots)
        if (length(plots) == 0) {
          showNotification("No valid marker plots could be generated.", type = "warning")
          return(NULL)
        }

        patchwork::wrap_plots(plots, ncol = as.integer(ncol_setting))
      }, error = function(e) {
        showNotification(
          paste("Marker Boxplots error:", e$message),
          type = "error", duration = 8
        )
        NULL
      })
    },

    # ════════════════════════════════════════════════════════════════════════
    # 8. Marker per Cluster Boxplot
    # ════════════════════════════════════════════════════════════════════════
    "Marker per Cluster Boxplot" = {
      tryCatch({
        markers_sel   <- input$analysisClusterBPMarkers
        group_col     <- input$analysisClusterBPGroup %||% "cluster_id"
        condition_col <- input$analysisClusterBPCondition %||% "None"
        ncol_setting  <- input$analysisClusterBPNcol %||% 2L
        base_size     <- as.integer(input$analysisTextSize %||% 12)

        req(length(markers_sel) > 0)

        assay_name <- "exprsQuantNorm"
        avail      <- SummarizedExperiment::assayNames(sce)
        if (!assay_name %in% avail) assay_name <- avail[1]

        expr_mat <- extract_expr_matrix(sce, assay_name, markers_sel)
        cd       <- as.data.frame(SummarizedExperiment::colData(sce))
        df       <- as.data.frame(t(expr_mat))
        df[[group_col]] <- cd[[group_col]]
        if (condition_col != "None" && condition_col %in% colnames(cd)) {
          df[[condition_col]] <- cd[[condition_col]]
        }

        # Sort cluster levels
        df[[group_col]] <- factor(df[[group_col]],
          levels = gtools::mixedsort(unique(as.character(df[[group_col]]))))

        actual_markers <- rownames(expr_mat)
        cond_colours   <- if (condition_col != "None") coloursList[[condition_col]] else NULL

        plots <- lapply(actual_markers, function(m) {
          if (!m %in% colnames(df)) return(NULL)

          if (condition_col != "None" && condition_col %in% colnames(df)) {
            p <- ggplot2::ggplot(df, ggplot2::aes(
              x = .data[[group_col]], y = .data[[m]],
              fill = .data[[condition_col]]
            ))
          } else {
            p <- ggplot2::ggplot(df, ggplot2::aes(
              x = .data[[group_col]], y = .data[[m]]
            ))
          }

          p <- p +
            ggplot2::geom_boxplot(alpha = 0.3, outlier.shape = NA) +
            ggprism::theme_prism(base_size = base_size) +
            ggplot2::ggtitle(m) +
            ggplot2::theme(
              axis.title = ggplot2::element_blank()
            )

          if (!is.null(cond_colours)) {
            p <- p + ggplot2::scale_fill_manual(values = cond_colours)
          }
          p
        })

        plots <- Filter(Negate(is.null), plots)
        if (length(plots) == 0) return(NULL)
        patchwork::wrap_plots(plots, ncol = as.integer(ncol_setting))
      }, error = function(e) {
        showNotification(
          paste("Marker per Cluster Boxplot error:", e$message),
          type = "error", duration = 8
        )
        NULL
      })
    },

    # ════════════════════════════════════════════════════════════════════════
    # 9. Abundance Barplot
    # ════════════════════════════════════════════════════════════════════════
    "Abundance Barplot" = {
      tryCatch({
        fill_col <- input$analysisBarplotFill %||% "cluster_id"
        x_col    <- input$analysisBarplotX
        if (is.null(x_col) || x_col == "") x_col <- fill_col

        # Use first (largest) umapDF
        umapDFAll <- res$umapDFList$All
        if (is.null(umapDFAll)) umapDFAll <- res$umapDFList[[1]]
        req(!is.null(umapDFAll), fill_col %in% colnames(umapDFAll))

        colours <- coloursList[[fill_col]]

        make_barplot(
          df           = umapDFAll,
          x_col        = x_col,
          fill_col     = fill_col,
          colours      = colours,
          fractional   = isTRUE(input$analysisBarplotPercentage),
          show_numbers = isTRUE(input$analysisBarplotShowNumbers),
          base_size    = 14
        )
      }, error = function(e) {
        showNotification(
          paste("Abundance Barplot error:", e$message),
          type = "error", duration = 8
        )
        NULL
      })
    }
  ) # end switch
})


# ── Render output ──────────────────────────────────────────────────────────
output$analysisPlotOutput <- renderPlot({
  p <- analysisPlotReactive()
  req(p)
  p
},
height = function() { as.integer(input$analysisFigHeight %||% 600) },
width  = function() { as.integer(input$analysisFigWidth  %||% 900) })
