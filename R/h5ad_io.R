#' @title h5ad I/O Functions for MARMOT
#' @description Save and load pipeline results as h5ad (AnnData HDF5) files.
#' Uses anndataR for pure-R h5ad read/write — no Python required.
#' @author Peter Leary
#' @name h5ad_io
NULL

#' Save All Pipeline Data to h5ad
#'
#' Packs all pipeline R objects into a single h5ad file using the AnnData format.
#' SCE data maps to standard AnnData slots (X, obs, var, obsm, layers).
#' MARMOT-specific metadata is stored in the \code{uns} slot.
#'
#' @param output_dir Path to the R_files output directory
#' @param envir Environment containing the pipeline variables (default: .GlobalEnv)
#' @export
save_h5ad_data <- function(output_dir, envir = .GlobalEnv) {
  requireNamespace("anndataR", quietly = TRUE)

  # Helper to safely get a variable from the environment
  safe_get <- function(name) {
    if (exists(name, envir = envir, inherits = FALSE)) get(name, envir = envir) else NULL
  }

  # -- Build AnnData from SCE --
  sce <- safe_get("sce")
  if (is.null(sce) || !inherits(sce, "SingleCellExperiment")) {
    stop("No SingleCellExperiment found in environment")
  }

  # Ensure colnames exist (needed for cell identity)
  if (is.null(colnames(sce))) {
    cell_ids <- rownames(SummarizedExperiment::colData(sce))
    if (is.null(cell_ids)) cell_ids <- paste0("cell", seq_len(ncol(sce)))
    colnames(sce) <- cell_ids
  }

  ad <- anndataR::as_AnnData(sce)

  # -- Populate uns with MARMOT metadata --
  uns <- list()

  # Manifest
  uns$marmot_manifest <- list(
    schema_version = 1L,
    pipeline_version = as.character(utils::packageVersion("MARMOT")),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    format = "marmot-h5ad-v1"
  )

  # Small metadata tables
  md <- safe_get("md")
  if (!is.null(md)) uns$sample_metadata <- as.data.frame(md)

  smd <- safe_get("smd")
  if (!is.null(smd)) uns$study_metadata <- as.data.frame(smd)

  panel <- safe_get("panel")
  if (!is.null(panel)) uns$panel <- as.data.frame(panel)

  # Pipeline settings (key-value as named list of strings)
  settings_vars <- c("clusteringMethodToUse", "dimRedMethodToUse", "knn",
                     "downsampleTo", "daPValToUse", "kValuesIWant")
  settings_list <- list()
  for (v in settings_vars) {
    val <- safe_get(v)
    if (!is.null(val)) settings_list[[v]] <- paste(val, collapse = ",")
  }
  if (length(settings_list) > 0) uns$pipeline_settings <- settings_list

  # cluster_codes (from SCE metadata, needed by diffcyt on reload)
  cc <- S4Vectors::metadata(sce)$cluster_codes
  if (!is.null(cc) && is.data.frame(cc)) {
    # Convert factor columns to character for safe serialization
    cc_save <- as.data.frame(cc)
    for (col in colnames(cc_save)) {
      if (is.factor(cc_save[[col]])) cc_save[[col]] <- as.character(cc_save[[col]])
    }
    uns$cluster_codes <- cc_save
  }

  # DR data frames (umapDFList)
  umapDFList <- safe_get("umapDFList")
  if (!is.null(umapDFList) && is.list(umapDFList)) {
    dr_list <- list()
    for (name in names(umapDFList)) {
      df <- umapDFList[[name]]
      if (is.data.frame(df)) {
        safe_name <- gsub("[^a-zA-Z0-9._-]", "_", name)
        dr_list[[safe_name]] <- df
      }
    }
    if (length(dr_list) > 0) uns$dr_dataframes <- dr_list
  }

  # Colours (convert named vectors to 2-col data.frames for safe serialization)
  coloursList <- safe_get("coloursList")
  if (!is.null(coloursList) && is.list(coloursList)) {
    col_list <- list()
    for (name in names(coloursList)) {
      cols <- coloursList[[name]]
      if (!is.null(cols) && length(cols) > 0 && is.character(cols) && !is.null(names(cols))) {
        col_list[[name]] <- data.frame(
          level = names(cols), colour = unname(cols),
          stringsAsFactors = FALSE
        )
      }
    }
    if (length(col_list) > 0) uns$colours <- col_list
  }

  # DA/DS results
  .pack_da_ds <- function(daList, dsList, selectedClustersList, daPValToUse) {
    if (!is.null(daList) && is.list(daList)) {
      da_list <- list()
      for (name in names(daList)) {
        df <- daList[[name]]
        if (is.data.frame(df)) {
          safe_name <- gsub("[^a-zA-Z0-9._-]", "_", name)
          da_list[[safe_name]] <- df
        }
      }
      if (length(da_list) > 0) uns$da_results <<- da_list
    }

    if (!is.null(dsList) && is.list(dsList)) {
      ds_list <- list()
      for (name in names(dsList)) {
        df <- dsList[[name]]
        safe_name <- gsub("[^a-zA-Z0-9._-]", "_", name)
        if (is.data.frame(df)) {
          ds_list[[safe_name]] <- df
        } else if (is.list(df) && !is.null(df$tbl_DS)) {
          ds_list[[safe_name]] <- as.data.frame(df$tbl_DS)
        }
      }
      if (length(ds_list) > 0) uns$ds_results <<- ds_list
    }

    if (!is.null(selectedClustersList) && is.list(selectedClustersList)) {
      rows <- lapply(names(selectedClustersList), function(n) {
        clusters <- selectedClustersList[[n]]
        if (length(clusters) > 0) {
          data.frame(contrast_direction = n, cluster_id = clusters, stringsAsFactors = FALSE)
        } else {
          NULL
        }
      })
      df <- do.call(rbind, Filter(Negate(is.null), rows))
      if (!is.null(df) && nrow(df) > 0) uns$selected_clusters <<- df
    }

    if (!is.null(daPValToUse) && !is.null(uns$pipeline_settings)) {
      uns$pipeline_settings$daPValToUse <<- daPValToUse
    }
  }

  .pack_da_ds(safe_get("daList"), safe_get("dsList"),
              safe_get("selectedClustersList"), safe_get("daPValToUse"))

  # QC data
  QCmini <- safe_get("QCmini")
  if (!is.null(QCmini) && is.data.frame(QCmini)) {
    qc <- list(qc_summary = QCmini)
    cf2 <- safe_get("cf2")
    if (!is.null(cf2)) {
      if (is.data.frame(cf2)) {
        qc$cofactors <- cf2
      } else if (is.numeric(cf2)) {
        qc$cofactors <- data.frame(
          marker_name = names(cf2), cofactor_value = unname(cf2),
          stringsAsFactors = FALSE
        )
      }
    }
    uns$qc <- qc
  }

  ad$uns <- uns

  # Write
  h5ad_path <- file.path(output_dir, "marmot_results.h5ad")
  anndataR::write_h5ad(ad, h5ad_path, compression = "gzip", mode = "w")

  message("h5ad data saved to: ", h5ad_path)
  invisible(h5ad_path)
}


#' Save DA/DS Results to h5ad
#'
#' Reads the existing h5ad, updates DA/DS results in uns, and rewrites.
#' Called both during fresh pipeline runs and during RDataFolder reload
#' (DA/DS always re-runs).
#'
#' @param h5ad_path Path to the marmot_results.h5ad file
#' @param daList List of DA result data frames (one per contrast)
#' @param dsList List of DS result data frames (one per contrast)
#' @param selectedClustersList List of selected DA clusters
#' @param daPValToUse Character: "p_adj" or "p_val"
#' @export
save_da_ds_h5ad <- function(h5ad_path, daList = NULL, dsList = NULL,
                             selectedClustersList = NULL, daPValToUse = NULL) {
  requireNamespace("anndataR", quietly = TRUE)

  ad <- anndataR::read_h5ad(h5ad_path)
  uns <- ad$uns
  if (is.null(uns)) uns <- list()

  # DA results
  if (!is.null(daList) && is.list(daList)) {
    da_list <- list()
    for (name in names(daList)) {
      df <- daList[[name]]
      if (is.data.frame(df)) {
        safe_name <- gsub("[^a-zA-Z0-9._-]", "_", name)
        da_list[[safe_name]] <- df
      }
    }
    if (length(da_list) > 0) uns$da_results <- da_list
  }

  # DS results
  if (!is.null(dsList) && is.list(dsList)) {
    ds_list <- list()
    for (name in names(dsList)) {
      df <- dsList[[name]]
      safe_name <- gsub("[^a-zA-Z0-9._-]", "_", name)
      if (is.data.frame(df)) {
        ds_list[[safe_name]] <- df
      } else if (is.list(df) && !is.null(df$tbl_DS)) {
        ds_list[[safe_name]] <- as.data.frame(df$tbl_DS)
      }
    }
    if (length(ds_list) > 0) uns$ds_results <- ds_list
  }

  # Selected clusters
  if (!is.null(selectedClustersList) && is.list(selectedClustersList)) {
    rows <- lapply(names(selectedClustersList), function(n) {
      clusters <- selectedClustersList[[n]]
      if (length(clusters) > 0) {
        data.frame(contrast_direction = n, cluster_id = clusters, stringsAsFactors = FALSE)
      } else {
        NULL
      }
    })
    df <- do.call(rbind, Filter(Negate(is.null), rows))
    if (!is.null(df) && nrow(df) > 0) uns$selected_clusters <- df
  }

  # Update daPValToUse in pipeline_settings
  if (!is.null(daPValToUse)) {
    if (is.null(uns$pipeline_settings)) uns$pipeline_settings <- list()
    uns$pipeline_settings$daPValToUse <- daPValToUse
  }

  ad$uns <- uns
  anndataR::write_h5ad(ad, h5ad_path, compression = "gzip", mode = "w")

  invisible(h5ad_path)
}


#' Load h5ad Data into Environment
#'
#' Reads a MARMOT h5ad file and reconstructs all pipeline R objects
#' in the specified environment.
#'
#' @param h5ad_path Path to the marmot_results.h5ad file
#' @param envir Environment to load objects into (default: .GlobalEnv)
#' @export
load_h5ad_to_env <- function(h5ad_path, envir = .GlobalEnv) {
  requireNamespace("anndataR", quietly = TRUE)

  if (!file.exists(h5ad_path)) stop("h5ad file not found: ", h5ad_path)

  ad <- anndataR::read_h5ad(h5ad_path)
  uns <- ad$uns
  if (is.null(uns)) uns <- list()

  # -- Metadata --
  if (!is.null(uns$sample_metadata)) assign("md", as.data.frame(uns$sample_metadata), envir = envir)
  if (!is.null(uns$study_metadata)) assign("smd", as.data.frame(uns$study_metadata), envir = envir)
  if (!is.null(uns$panel)) assign("panel", as.data.frame(uns$panel), envir = envir)

  # -- Pipeline settings --
  if (!is.null(uns$pipeline_settings)) {
    for (key in names(uns$pipeline_settings)) {
      val <- uns$pipeline_settings[[key]]
      # Try to parse numeric values
      num_val <- suppressWarnings(as.numeric(val))
      if (!is.na(num_val) && !grepl(",", val)) {
        assign(key, num_val, envir = envir)
      } else if (grepl(",", val)) {
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
  sce <- reconstruct_sce_from_h5ad(h5ad_path, ad = ad)
  if (!is.null(sce)) assign("sce", sce, envir = envir)

  # -- umapDFList --
  if (!is.null(uns$dr_dataframes) && is.list(uns$dr_dataframes)) {
    umapDFList <- lapply(uns$dr_dataframes, as.data.frame)
    # Restore dots in names (e.g., "Downsampled_UMAP" -> "Downsampled.UMAP")
    names(umapDFList) <- gsub("_", ".", names(umapDFList))
    assign("umapDFList", umapDFList, envir = envir)
  }

  # -- Colours (reconstruct named vectors from 2-col data.frames) --
  if (!is.null(uns$colours) && is.list(uns$colours)) {
    coloursList <- lapply(uns$colours, function(df) {
      df <- as.data.frame(df)
      setNames(df$colour, df$level)
    })
    assign("coloursList", coloursList, envir = envir)
  }

  # -- DA results --
  if (!is.null(uns$da_results) && is.list(uns$da_results)) {
    daList <- lapply(uns$da_results, as.data.frame)
    assign("daList", daList, envir = envir)
  }

  # -- Selected clusters --
  if (!is.null(uns$selected_clusters)) {
    sc_df <- as.data.frame(uns$selected_clusters)
    selectedClustersList <- split(sc_df$cluster_id, sc_df$contrast_direction)
    assign("selectedClustersList", selectedClustersList, envir = envir)
  }

  # -- DS results --
  if (!is.null(uns$ds_results) && is.list(uns$ds_results)) {
    dsList <- lapply(uns$ds_results, as.data.frame)
    assign("dsList", dsList, envir = envir)
  }

  # -- QC --
  if (!is.null(uns$qc)) {
    if (!is.null(uns$qc$qc_summary)) {
      assign("QCmini", as.data.frame(uns$qc$qc_summary), envir = envir)
    }
    if (!is.null(uns$qc$cofactors)) {
      cofactors <- as.data.frame(uns$qc$cofactors)
      if ("marker_name" %in% colnames(cofactors)) {
        cf2 <- setNames(cofactors$cofactor_value, cofactors$marker_name)
      } else {
        cf2 <- cofactors
      }
      assign("cf2", cf2, envir = envir)
    }
  }

  message("h5ad data loaded from: ", h5ad_path)
  invisible(TRUE)
}


#' Load h5ad Data for Shiny App
#'
#' Returns a list suitable for use as \code{inputDataReactive$Results} in the Shiny app.
#'
#' @param h5ad_path Path to the marmot_results.h5ad file
#' @return A named list matching the Shiny app's expected Results structure
#' @export
load_h5ad_for_shiny <- function(h5ad_path) {
  requireNamespace("anndataR", quietly = TRUE)

  if (!file.exists(h5ad_path)) stop("h5ad file not found: ", h5ad_path)

  ad <- anndataR::read_h5ad(h5ad_path)
  uns <- ad$uns
  if (is.null(uns)) uns <- list()

  files <- list()

  # Metadata
  if (!is.null(uns$sample_metadata)) files$md <- as.data.frame(uns$sample_metadata)
  if (!is.null(uns$study_metadata)) files$smd <- as.data.frame(uns$study_metadata)

  # Pipeline settings
  if (!is.null(uns$pipeline_settings)) {
    for (key in names(uns$pipeline_settings)) {
      val <- uns$pipeline_settings[[key]]
      num_val <- suppressWarnings(as.numeric(val))
      files[[key]] <- if (!is.na(num_val) && !grepl(",", val)) num_val else val
    }
  }

  # SCE
  files$sce <- reconstruct_sce_from_h5ad(h5ad_path, ad = ad)

  # Conditions
  if (!is.null(files$md)) {
    conditions <- setdiff(colnames(files$md), c("file_name", "sample_id", "condition"))
    files$conditions <- gsub("-", ".", c("condition", conditions))
  }

  # mergeBy
  clusteringMethodToUse <- files$clusteringMethodToUse
  if (!is.null(clusteringMethodToUse)) {
    files$mergeBy <- switch(clusteringMethodToUse,
      "Rphenograph" = "k", "Mphenograph" = "k", "MfastPG" = "k",
      "PARC" = "p", "FlowSOM" = "meta"
    )
  }

  # umapDFList
  if (!is.null(uns$dr_dataframes) && is.list(uns$dr_dataframes)) {
    files$umapDFList <- lapply(uns$dr_dataframes, as.data.frame)
    names(files$umapDFList) <- gsub("_", ".", names(files$umapDFList))
  }

  # Colours (reconstruct named vectors)
  if (!is.null(uns$colours) && is.list(uns$colours)) {
    files$coloursList <- lapply(uns$colours, function(df) {
      df <- as.data.frame(df)
      setNames(df$colour, df$level)
    })
  }

  # DA results
  if (!is.null(uns$da_results) && is.list(uns$da_results)) {
    files$daList <- lapply(uns$da_results, as.data.frame)
  }

  # Selected clusters
  if (!is.null(uns$selected_clusters)) {
    sc_df <- as.data.frame(uns$selected_clusters)
    files$selectedClustersList <- split(sc_df$cluster_id, sc_df$contrast_direction)
  }

  # DS results
  if (!is.null(uns$ds_results) && is.list(uns$ds_results)) {
    files$dsList <- lapply(uns$ds_results, as.data.frame)
  }

  files
}


#' Reconstruct a SingleCellExperiment from h5ad
#'
#' Reads the h5ad file and converts to SCE, restoring factor columns
#' and injecting cluster_codes + experiment_info into metadata.
#'
#' @param h5ad_path Path to the marmot_results.h5ad file
#' @return A SingleCellExperiment object, or NULL if file is missing
#' @export
reconstruct_sce_from_h5ad <- function(h5ad_path, ad = NULL) {
  requireNamespace("anndataR", quietly = TRUE)
  requireNamespace("SingleCellExperiment", quietly = TRUE)
  requireNamespace("SummarizedExperiment", quietly = TRUE)

  if (!file.exists(h5ad_path)) return(NULL)

  if (is.null(ad)) ad <- anndataR::read_h5ad(h5ad_path)
  sce <- ad$as_SingleCellExperiment()
  uns <- ad$uns
  if (is.null(uns)) uns <- list()

  # Restore factor columns in rowData
  rd <- as.data.frame(SummarizedExperiment::rowData(sce))
  if (nrow(rd) > 0 && ncol(rd) > 0) {
    for (col in colnames(rd)) {
      if (is.character(rd[[col]])) rd[[col]] <- factor(rd[[col]])
    }
    SummarizedExperiment::rowData(sce) <- S4Vectors::DataFrame(rd)
  }

  # Restore experiment_info from uns
  if (!is.null(uns$sample_metadata)) {
    S4Vectors::metadata(sce)$experiment_info <- as.data.frame(uns$sample_metadata)
  }

  # Restore cluster_codes from uns
  if (!is.null(uns$cluster_codes)) {
    cc <- as.data.frame(uns$cluster_codes)
    for (col in colnames(cc)) {
      if (is.character(cc[[col]])) cc[[col]] <- factor(cc[[col]])
    }
    S4Vectors::metadata(sce)$cluster_codes <- cc
  }

  sce
}
