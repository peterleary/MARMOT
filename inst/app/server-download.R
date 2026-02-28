# server-download.R
# Download handlers for plots, data, FCS files, settings

# Download DR plot
output$dlUMAP <- downloadHandler(
  filename = function() {
    paste(input$umapColumnToPlot, tolower(input$dlFormat), sep = ".")
  },
  content = function(file) {
    req(!is.null(umapReactive()$umapStatic))
    w <- as.numeric(input$figWidthUMAP / 60)
    h <- as.numeric(input$figHeightUMAP / 60)
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

# Download Feature Plot
output$dlFP <- downloadHandler(
  filename = function() {
    paste(gsub(" ", "", input$featurePlotType),
          paste(input$fpFeatureToPlot, collapse = "_"),
          tolower(input$dlFormat), sep = ".")
  },
  content = function(file) {
    req(!is.null(featurePlotReactive$fp))
    w <- as.numeric(input$figWidthFP / 60)
    h <- as.numeric(input$figHeightFP / 60)
    if (input$dlFormat == "PDF") {
      pdf(file = file, width = w, height = h)
    } else if (input$dlFormat == "SVG") {
      svg(file = file, width = w, height = h)
    } else if (input$dlFormat == "PNG") {
      png(filename = file, width = w, height = h,
          units = "in", res = as.numeric(input$pngRes))
    }
    fp <- featurePlotReactive$fp
    if (inherits(fp, "Heatmap") || inherits(fp, "HeatmapList")) {
      ComplexHeatmap::draw(fp)
    } else if (input$featurePlotType %in%
               c("Feature Plot", "Nebulosa Plot", "Ridge Plot", "Violin Plot") &&
               is.list(fp) && length(fp) >= 2) {
      print(patchwork::wrap_plots(fp, ncol = input$ncolFPGene))
    } else {
      print(fp)
    }
    dev.off()
  }
)

# Download inputs as Excel
output$downloadInputsE <- downloadHandler(
  filename = function() "Input_Options.xlsx",
  content = function(file) {
    x <- reactiveValuesToList(input)
    x <- data.frame(unlist(x))
    x <- tibble::rownames_to_column(x, "Input")
    colnames(x)[[2]] <- "Value"
    writexl::write_xlsx(x, path = file)
  }
)

# Download cluster codes
output$downloadClusterCodes <- downloadHandler(
  filename = function() "clusterCodes.xlsx",
  content = function(file) {
    umap_a <- inputDataReactive$Results$umapDFList$All
    clusterCodes <- build_cluster_codes(umap_a)
    writexl::write_xlsx(clusterCodes, path = file)
  }
)

# Download modified FCS files
output$downloadFCS <- downloadHandler(
  filename = function() {
    paste("modified_fcs_files_", Sys.Date(), ".zip", sep = "")
  },
  content = function(file) {
    req(
      !is.null(inputDataReactive$Results$framesList),
      !is.null(inputDataReactive$Results$md$sample_id)
    )
    temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
    dir.create(temp_directory)

    umap_a <- inputDataReactive$Results$umapDFList$All
    clusterCodes <- build_cluster_codes(umap_a)

    fcsFilesList <- lapply(levels(inputDataReactive$Results$md$sample_id), function(s) {
      umap_xx <- umap_a[which(umap_a$sample_id == s), ]
      apps <- data.frame(
        umap_x = umap_xx$x, umap_y = umap_xx$y,
        cluster_id_codes = clusterCodes$cluster_id_codes[
          match(umap_xx$cluster_id, clusterCodes$cluster_ids)
        ]
      )
      if ("relabelled_clusters" %in% colnames(umap_xx)) {
        apps$new_cluster_codes <- clusterCodes$new_cluster_codes[
          match(umap_xx$cluster_id, clusterCodes$cluster_ids)
        ]
      }
      fn1 <- inputDataReactive$Results$md$file_name[
        inputDataReactive$Results$md$sample_id == s
      ][[1]]
      fn2 <- file.path(temp_directory, paste0(s, "_modified.fcs"))

      flowCore::write.FCS(
        x = flowCore::fr_append_cols(
          fr = inputDataReactive$Results$framesList$`All Cells`[[fn1]],
          cols = as.matrix(apps)
        ),
        filename = fn2,
        delimiter = "#"
      )
    }) |> setNames(unique(inputDataReactive$Results$md$sample_id))

    zip::zip(zipfile = file, files = dir(temp_directory), root = temp_directory)
    unlink(temp_directory, recursive = TRUE)
  },
  contentType = "application/zip"
)
