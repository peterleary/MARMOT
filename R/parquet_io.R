#' @title Parquet I/O Functions for MARMOT
#' @description Save and load pipeline results as Parquet files for fast, interoperable storage.
#' @author Peter Leary
#' @name parquet_io
NULL

#' Save All Pipeline Data to Parquet
#'
#' Decomposes all pipeline R objects into tabular Parquet files within a
#' `parquet/` subdirectory of the R_files output directory.
#'
#' @param qs_dir Path to the R_files output directory
#' @param envir Environment containing the pipeline variables (default: .GlobalEnv)
#' @export
save_parquet_data <- function(qs_dir, envir = .GlobalEnv) {
  requireNamespace("arrow", quietly = TRUE)

  pq_dir <- file.path(qs_dir, "parquet")
  dir.create(pq_dir, recursive = TRUE, showWarnings = FALSE)

  # Sub-directories
  dirs <- c("expression", "reductions", "dr_dataframes", "colours",
            "da_results", "ds_results", "qc")
  for (d in dirs) dir.create(file.path(pq_dir, d), showWarnings = FALSE)

  # Helper to safely get a variable from the environment
  safe_get <- function(name) {
    if (exists(name, envir = envir, inherits = FALSE)) get(name, envir = envir) else NULL
  }

  # -- Manifest --
  manifest <- list(
    schema_version = 1L,
    pipeline_version = as.character(utils::packageVersion("MARMOT")),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    format = "marmot-parquet-v1"
  )
  jsonlite::write_json(manifest, file.path(pq_dir, "_manifest.json"), auto_unbox = TRUE, pretty = TRUE)

  # -- Small metadata tables --
  md <- safe_get("md")
  if (!is.null(md)) arrow::write_parquet(as.data.frame(md), file.path(pq_dir, "sample_metadata.parquet"))

  smd <- safe_get("smd")
  if (!is.null(smd)) arrow::write_parquet(as.data.frame(smd), file.path(pq_dir, "study_metadata.parquet"))

  panel <- safe_get("panel")
  if (!is.null(panel)) arrow::write_parquet(as.data.frame(panel), file.path(pq_dir, "panel.parquet"))

  # -- Pipeline settings (scalar values -> key-value table) --
  settings_vars <- c("clusteringMethodToUse", "dimRedMethodToUse", "knn",
                     "downsampleTo", "daPValToUse", "kValuesIWant")
  settings_list <- lapply(settings_vars, function(v) {
    val <- safe_get(v)
    if (!is.null(val)) data.frame(key = v, value = paste(val, collapse = ","), stringsAsFactors = FALSE)
    else NULL
  })
  settings_df <- do.call(rbind, Filter(Negate(is.null), settings_list))
  if (!is.null(settings_df) && nrow(settings_df) > 0) {
    arrow::write_parquet(settings_df, file.path(pq_dir, "pipeline_settings.parquet"))
  }

  # -- SCE decomposition --
  sce <- safe_get("sce")
  if (!is.null(sce) && inherits(sce, "SingleCellExperiment")) {
    # Cell IDs: use colnames, fall back to rownames of colData, then generate
    cell_ids <- colnames(sce)
    if (is.null(cell_ids)) cell_ids <- rownames(SummarizedExperiment::colData(sce))
    if (is.null(cell_ids)) cell_ids <- paste0("cell", seq_len(ncol(sce)))

    # Cell metadata
    cd <- as.data.frame(SummarizedExperiment::colData(sce))
    cd$cell_id <- cell_ids
    arrow::write_parquet(cd, file.path(pq_dir, "cell_metadata.parquet"))

    # Expression assays
    assay_names <- SummarizedExperiment::assayNames(sce)
    for (aname in assay_names) {
      mat <- as.matrix(SummarizedExperiment::assay(sce, aname))
      df <- as.data.frame(t(mat))
      df$cell_id <- cell_ids
      # Move cell_id to first column
      df <- df[, c("cell_id", setdiff(colnames(df), "cell_id"))]
      arrow::write_parquet(df, file.path(pq_dir, "expression", paste0(aname, ".parquet")))
    }

    # Reduced dimensions
    rd_names <- SingleCellExperiment::reducedDimNames(sce)
    for (rname in rd_names) {
      rd <- as.data.frame(SingleCellExperiment::reducedDim(sce, rname))
      rd$cell_id <- cell_ids
      rd <- rd[, c("cell_id", setdiff(colnames(rd), "cell_id"))]
      arrow::write_parquet(rd, file.path(pq_dir, "reductions", paste0(rname, ".parquet")))
    }

    # cluster_codes (needed by diffcyt on reload)
    cc <- S4Vectors::metadata(sce)$cluster_codes
    if (!is.null(cc) && is.data.frame(cc)) {
      arrow::write_parquet(cc, file.path(pq_dir, "cluster_codes.parquet"))
    }

    # rowData (marker metadata - needed by diffcyt on reload)
    rd <- as.data.frame(SummarizedExperiment::rowData(sce))
    if (nrow(rd) > 0 && ncol(rd) > 0) {
      arrow::write_parquet(rd, file.path(pq_dir, "row_data.parquet"))
    }
  }

  # -- DR data frames (umapDFList) --
  umapDFList <- safe_get("umapDFList")
  if (!is.null(umapDFList) && is.list(umapDFList)) {
    for (name in names(umapDFList)) {
      df <- umapDFList[[name]]
      if (is.data.frame(df)) {
        safe_name <- gsub("[^a-zA-Z0-9._-]", "_", name)
        arrow::write_parquet(df, file.path(pq_dir, "dr_dataframes", paste0(safe_name, ".parquet")))
      }
    }
  }

  # -- Colours --
  coloursList <- safe_get("coloursList")
  if (!is.null(coloursList) && is.list(coloursList)) {
    for (name in names(coloursList)) {
      cols <- coloursList[[name]]
      if (!is.null(cols) && length(cols) > 0 && is.character(cols) && !is.null(names(cols))) {
        df <- data.frame(level = names(cols), colour = unname(cols),
                         .original_name = name, stringsAsFactors = FALSE)
        safe_name <- gsub("[^a-zA-Z0-9._-]", "_", name)
        arrow::write_parquet(df, file.path(pq_dir, "colours", paste0(safe_name, ".parquet")))
      }
    }
  }

  # -- DA/DS results --
  save_da_ds_parquet(pq_dir, safe_get("daList"), safe_get("dsList"),
                     safe_get("selectedClustersList"), safe_get("daPValToUse"))

  # -- QC data --
  QCmini <- safe_get("QCmini")
  if (!is.null(QCmini) && is.data.frame(QCmini)) {
    arrow::write_parquet(QCmini, file.path(pq_dir, "qc", "qc_summary.parquet"))
  }

  cf2 <- safe_get("cf2")
  if (!is.null(cf2)) {
    if (is.data.frame(cf2)) {
      arrow::write_parquet(cf2, file.path(pq_dir, "qc", "cofactors.parquet"))
    } else if (is.numeric(cf2)) {
      df <- data.frame(marker_name = names(cf2), cofactor_value = unname(cf2), stringsAsFactors = FALSE)
      arrow::write_parquet(df, file.path(pq_dir, "qc", "cofactors.parquet"))
    }
  }

  message("Parquet data saved to: ", pq_dir)
  invisible(pq_dir)
}


#' Save DA/DS Results to Parquet
#'
#' Saves only the differential analysis results. Called both during fresh pipeline
#' runs and during RDataFolder reload (DA/DS always re-runs).
#'
#' @param pq_dir Path to the parquet/ directory
#' @param daList List of DA result data frames (one per contrast)
#' @param dsList List of DS result data frames (one per contrast)
#' @param selectedClustersList List of selected DA clusters
#' @param daPValToUse Character: "p_adj" or "p_val"
#' @export
save_da_ds_parquet <- function(pq_dir, daList = NULL, dsList = NULL,
                               selectedClustersList = NULL, daPValToUse = NULL) {
  requireNamespace("arrow", quietly = TRUE)

  da_dir <- file.path(pq_dir, "da_results")
  ds_dir <- file.path(pq_dir, "ds_results")
  dir.create(da_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(ds_dir, showWarnings = FALSE, recursive = TRUE)

  # DA results
  if (!is.null(daList) && is.list(daList)) {
    for (name in names(daList)) {
      df <- daList[[name]]
      if (is.data.frame(df)) {
        safe_name <- gsub("[^a-zA-Z0-9._-]", "_", name)
        arrow::write_parquet(df, file.path(da_dir, paste0(safe_name, ".parquet")))
      }
    }
  }

  # DS results
  if (!is.null(dsList) && is.list(dsList)) {
    for (name in names(dsList)) {
      df <- dsList[[name]]
      if (is.data.frame(df)) {
        safe_name <- gsub("[^a-zA-Z0-9._-]", "_", name)
        arrow::write_parquet(df, file.path(ds_dir, paste0(safe_name, ".parquet")))
      }
    }
  }

  # Selected clusters
  if (!is.null(selectedClustersList) && is.list(selectedClustersList)) {
    # Convert named list to data.frame
    rows <- lapply(names(selectedClustersList), function(n) {
      clusters <- selectedClustersList[[n]]
      if (length(clusters) > 0) {
        data.frame(contrast_direction = n, cluster_id = clusters, stringsAsFactors = FALSE)
      } else {
        NULL
      }
    })
    df <- do.call(rbind, Filter(Negate(is.null), rows))
    if (!is.null(df) && nrow(df) > 0) {
      arrow::write_parquet(df, file.path(da_dir, "selected_clusters.parquet"))
    }
  }

  # Save daPValToUse if provided
  if (!is.null(daPValToUse)) {
    settings_file <- file.path(pq_dir, "pipeline_settings.parquet")
    if (file.exists(settings_file)) {
      existing <- arrow::read_parquet(settings_file)
      existing <- existing[existing$key != "daPValToUse", , drop = FALSE]
      existing <- rbind(existing, data.frame(key = "daPValToUse", value = daPValToUse, stringsAsFactors = FALSE))
      arrow::write_parquet(existing, settings_file)
    }
  }

  invisible(pq_dir)
}


#' Load Parquet Data into Environment
#'
#' Reads all Parquet tables from the MARMOT parquet directory and reconstructs
#' the pipeline R objects in the specified environment.
#'
#' @param pq_dir Path to the parquet/ directory
#' @param envir Environment to load objects into (default: .GlobalEnv)
#' @export
load_parquet_to_env <- function(pq_dir, envir = .GlobalEnv) {
  requireNamespace("arrow", quietly = TRUE)

  # Read manifest
  manifest_path <- file.path(pq_dir, "_manifest.json")
  if (!file.exists(manifest_path)) stop("No _manifest.json found in: ", pq_dir)

  # -- Metadata --
  .safe_read <- function(path) {
    if (file.exists(path)) arrow::read_parquet(path) else NULL
  }

  md <- .safe_read(file.path(pq_dir, "sample_metadata.parquet"))
  if (!is.null(md)) assign("md", md, envir = envir)

  smd <- .safe_read(file.path(pq_dir, "study_metadata.parquet"))
  if (!is.null(smd)) assign("smd", smd, envir = envir)

  panel <- .safe_read(file.path(pq_dir, "panel.parquet"))
  if (!is.null(panel)) assign("panel", panel, envir = envir)

  # -- Pipeline settings --
  settings <- .safe_read(file.path(pq_dir, "pipeline_settings.parquet"))
  if (!is.null(settings)) {
    for (i in seq_len(nrow(settings))) {
      val <- settings$value[i]
      key <- settings$key[i]
      # Try to parse numeric values
      num_val <- suppressWarnings(as.numeric(val))
      if (!is.na(num_val) && !grepl(",", val)) {
        assign(key, num_val, envir = envir)
      } else if (grepl(",", val)) {
        # Comma-separated values
        parts <- trimws(strsplit(val, ",")[[1]])
        num_parts <- suppressWarnings(as.numeric(parts))
        if (all(!is.na(num_parts))) {
          assign(key, num_parts, envir = envir)
        } else {
          assign(key, parts, envir = envir)
        }
      } else {
        assign(key, val, envir = envir)
      }
    }
  }

  # -- Reconstruct SCE --
  sce <- reconstruct_sce_from_parquet(pq_dir)
  if (!is.null(sce)) assign("sce", sce, envir = envir)

  # -- umapDFList --
  dr_dir <- file.path(pq_dir, "dr_dataframes")
  if (dir.exists(dr_dir)) {
    dr_files <- list.files(dr_dir, pattern = "\\.parquet$", full.names = TRUE)
    umapDFList <- lapply(dr_files, arrow::read_parquet)
    names(umapDFList) <- tools::file_path_sans_ext(basename(dr_files))
    # Restore dots in names (e.g., "Downsampled_UMAP" -> "Downsampled.UMAP")
    names(umapDFList) <- gsub("_", ".", names(umapDFList))
    assign("umapDFList", umapDFList, envir = envir)
  }

  # -- Colours --
  col_dir <- file.path(pq_dir, "colours")
  if (dir.exists(col_dir)) {
    col_files <- list.files(col_dir, pattern = "\\.parquet$", full.names = TRUE)
    coloursList <- lapply(col_files, function(f) {
      df <- arrow::read_parquet(f)
      setNames(df$colour, df$level)
    })
    names(coloursList) <- vapply(col_files, function(f) {
      df <- arrow::read_parquet(f)
      if (".original_name" %in% colnames(df) && nrow(df) > 0) {
        df$.original_name[1]
      } else {
        tools::file_path_sans_ext(basename(f))
      }
    }, character(1))
    assign("coloursList", coloursList, envir = envir)
  }

  # -- DA results --
  da_dir <- file.path(pq_dir, "da_results")
  if (dir.exists(da_dir)) {
    da_files <- list.files(da_dir, pattern = "\\.parquet$", full.names = TRUE)
    da_files <- da_files[basename(da_files) != "selected_clusters.parquet"]
    if (length(da_files) > 0) {
      daList <- lapply(da_files, arrow::read_parquet)
      names(daList) <- tools::file_path_sans_ext(basename(da_files))
      assign("daList", daList, envir = envir)
    }
    # Selected clusters
    sc_file <- file.path(da_dir, "selected_clusters.parquet")
    if (file.exists(sc_file)) {
      sc_df <- arrow::read_parquet(sc_file)
      selectedClustersList <- split(sc_df$cluster_id, sc_df$contrast_direction)
      assign("selectedClustersList", selectedClustersList, envir = envir)
    }
  }

  # -- DS results --
  ds_dir <- file.path(pq_dir, "ds_results")
  if (dir.exists(ds_dir)) {
    ds_files <- list.files(ds_dir, pattern = "\\.parquet$", full.names = TRUE)
    if (length(ds_files) > 0) {
      dsList <- lapply(ds_files, arrow::read_parquet)
      names(dsList) <- tools::file_path_sans_ext(basename(ds_files))
      assign("dsList", dsList, envir = envir)
    }
  }

  # -- QC --
  qc_summary <- .safe_read(file.path(pq_dir, "qc", "qc_summary.parquet"))
  if (!is.null(qc_summary)) assign("QCmini", qc_summary, envir = envir)

  cofactors <- .safe_read(file.path(pq_dir, "qc", "cofactors.parquet"))
  if (!is.null(cofactors)) {
    if ("marker_name" %in% colnames(cofactors)) {
      cf2 <- setNames(cofactors$cofactor_value, cofactors$marker_name)
    } else {
      cf2 <- cofactors
    }
    assign("cf2", cf2, envir = envir)
  }

  message("Parquet data loaded from: ", pq_dir)
  invisible(TRUE)
}


#' Load Parquet Data for Shiny App
#'
#' Returns a list suitable for use as \code{inputDataReactive$Results} in the Shiny app.
#'
#' @param pq_dir Path to the parquet/ directory
#' @return A named list matching the Shiny app's expected Results structure
#' @export
load_parquet_for_shiny <- function(pq_dir) {
  requireNamespace("arrow", quietly = TRUE)

  .safe_read <- function(path) {
    if (file.exists(path)) arrow::read_parquet(path) else NULL
  }

  files <- list()

  # Metadata
  files$md <- .safe_read(file.path(pq_dir, "sample_metadata.parquet"))
  files$smd <- .safe_read(file.path(pq_dir, "study_metadata.parquet"))

  # Pipeline settings
  settings <- .safe_read(file.path(pq_dir, "pipeline_settings.parquet"))
  if (!is.null(settings)) {
    for (i in seq_len(nrow(settings))) {
      key <- settings$key[i]
      val <- settings$value[i]
      num_val <- suppressWarnings(as.numeric(val))
      files[[key]] <- if (!is.na(num_val) && !grepl(",", val)) num_val else val
    }
  }

  # SCE
  files$sce <- reconstruct_sce_from_parquet(pq_dir)

  # Conditions
  if (!is.null(files$md)) {
    conditions <- setdiff(colnames(files$md), c("file_name", "sample_id", "condition"))
    files$conditions <- gsub("-", ".", c("condition", conditions))
  }

  # mergeBy
  clusteringMethodToUse <- files$clusteringMethodToUse
  if (!is.null(clusteringMethodToUse)) {
    files$mergeBy <- switch(clusteringMethodToUse,
      "Rphenograph" = "k", "FastPG" = "k", "PARC" = "p", "FlowSOM" = "meta"
    )
  }

  # umapDFList
  dr_dir <- file.path(pq_dir, "dr_dataframes")
  if (dir.exists(dr_dir)) {
    dr_files <- list.files(dr_dir, pattern = "\\.parquet$", full.names = TRUE)
    files$umapDFList <- lapply(dr_files, arrow::read_parquet)
    names(files$umapDFList) <- gsub("_", ".", tools::file_path_sans_ext(basename(dr_files)))
  }

  # Colours
  col_dir <- file.path(pq_dir, "colours")
  if (dir.exists(col_dir)) {
    col_files <- list.files(col_dir, pattern = "\\.parquet$", full.names = TRUE)
    files$coloursList <- lapply(col_files, function(f) {
      df <- arrow::read_parquet(f)
      setNames(df$colour, df$level)
    })
    names(files$coloursList) <- vapply(col_files, function(f) {
      df <- arrow::read_parquet(f)
      if (".original_name" %in% colnames(df) && nrow(df) > 0) {
        df$.original_name[1]
      } else {
        tools::file_path_sans_ext(basename(f))
      }
    }, character(1))
  }

  # DA results
  da_dir <- file.path(pq_dir, "da_results")
  if (dir.exists(da_dir)) {
    da_files <- list.files(da_dir, pattern = "\\.parquet$", full.names = TRUE)
    da_files <- da_files[basename(da_files) != "selected_clusters.parquet"]
    if (length(da_files) > 0) {
      files$daList <- lapply(da_files, arrow::read_parquet)
      names(files$daList) <- tools::file_path_sans_ext(basename(da_files))
    }
    sc_file <- file.path(da_dir, "selected_clusters.parquet")
    if (file.exists(sc_file)) {
      sc_df <- arrow::read_parquet(sc_file)
      files$selectedClustersList <- split(sc_df$cluster_id, sc_df$contrast_direction)
    }
  }

  # DS results
  ds_dir <- file.path(pq_dir, "ds_results")
  if (dir.exists(ds_dir)) {
    ds_files <- list.files(ds_dir, pattern = "\\.parquet$", full.names = TRUE)
    if (length(ds_files) > 0) {
      files$dsList <- lapply(ds_files, arrow::read_parquet)
      names(files$dsList) <- tools::file_path_sans_ext(basename(ds_files))
    }
  }

  # Top marker table (check parent dir)
  # This will be checked by the importer separately

  files
}


#' Reconstruct a SingleCellExperiment from Parquet Tables
#'
#' Builds a minimal SCE object from the decomposed Parquet tables.
#' Needed for functions like \code{plotExprHeatmap()}, \code{plotFreqHeatmap()},
#' and \code{Nebulosa::plot_density()}.
#'
#' @param pq_dir Path to the parquet/ directory
#' @return A SingleCellExperiment object, or NULL if data is missing
#' @export
reconstruct_sce_from_parquet <- function(pq_dir) {
  requireNamespace("arrow", quietly = TRUE)
  requireNamespace("SingleCellExperiment", quietly = TRUE)
  requireNamespace("SummarizedExperiment", quietly = TRUE)

  # Read cell metadata
  cd_path <- file.path(pq_dir, "cell_metadata.parquet")
  if (!file.exists(cd_path)) return(NULL)
  cd <- as.data.frame(arrow::read_parquet(cd_path))
  cell_ids <- cd$cell_id
  cd$cell_id <- NULL
  rownames(cd) <- cell_ids

  # Read expression assays
  expr_dir <- file.path(pq_dir, "expression")
  if (!dir.exists(expr_dir)) return(NULL)
  expr_files <- list.files(expr_dir, pattern = "\\.parquet$", full.names = TRUE)
  if (length(expr_files) == 0) return(NULL)

  assays_list <- lapply(expr_files, function(f) {
    df <- arrow::read_parquet(f)
    cids <- df$cell_id
    df$cell_id <- NULL
    mat <- t(as.matrix(df))
    colnames(mat) <- cids
    mat
  })
  names(assays_list) <- tools::file_path_sans_ext(basename(expr_files))

  # Build SCE
  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = assays_list,
    colData = S4Vectors::DataFrame(cd)
  )

  # Add reduced dimensions
  red_dir <- file.path(pq_dir, "reductions")
  if (dir.exists(red_dir)) {
    red_files <- list.files(red_dir, pattern = "\\.parquet$", full.names = TRUE)
    for (f in red_files) {
      rd <- arrow::read_parquet(f)
      rd$cell_id <- NULL
      rd_name <- tools::file_path_sans_ext(basename(f))
      SingleCellExperiment::reducedDim(sce, rd_name) <- as.matrix(rd)
    }
  }

  # Restore rowData (marker metadata - needed by diffcyt on reload)
  rd_path <- file.path(pq_dir, "row_data.parquet")
  if (file.exists(rd_path)) {
    rd <- as.data.frame(arrow::read_parquet(rd_path))
    # Restore factor columns
    for (col in colnames(rd)) {
      if (is.character(rd[[col]])) rd[[col]] <- factor(rd[[col]])
    }
    SummarizedExperiment::rowData(sce) <- S4Vectors::DataFrame(rd)
  }

  # Try to add experiment_info to metadata (needed for CATALYST functions)
  md_path <- file.path(pq_dir, "sample_metadata.parquet")
  if (file.exists(md_path)) {
    md <- arrow::read_parquet(md_path)
    S4Vectors::metadata(sce)$experiment_info <- md
  }

  # Restore cluster_codes (needed by diffcyt on reload)
  cc_path <- file.path(pq_dir, "cluster_codes.parquet")
  if (file.exists(cc_path)) {
    cc <- as.data.frame(arrow::read_parquet(cc_path))
    # Restore factor columns
    for (col in colnames(cc)) {
      if (is.character(cc[[col]])) cc[[col]] <- factor(cc[[col]])
    }
    S4Vectors::metadata(sce)$cluster_codes <- cc
  }

  sce
}
