#' Install optional MARMOT extras
#'
#' Installs additional packages and sets up the Python environment on top of
#' the base MARMOT install.
#'
#' MARMOT has a two-tier install:
#' \describe{
#'   \item{Tier 1 (base)}{
#'     \code{BiocManager::install("peterleary/MARMOT")} — installs FlowSOM,
#'     CATALYST, diffcyt, UMAP, TSNE, and everything R-based. Works immediately.
#'   }
#'   \item{Tier 2 (extras)}{
#'     \code{install_marmot_extras()} — installs optional R packages (PeacoQC,
#'     flowAI, Seurat, etc.) and, when \code{include_python = TRUE}, creates a
#'     \code{p4r} conda environment for PARC clustering and PaCMAP
#'     dimensionality reduction. Requires conda or mamba (e.g. miniforge).
#'   }
#' }
#'
#' @param include_suggests If \code{TRUE} (the default), also install optional
#'   CRAN packages such as Seurat.
#' @param include_python If \code{TRUE} (the default), also create the
#'   \code{p4r} conda environment for PARC and PaCMAP.
#'   Requires conda or mamba (install miniforge from
#'   \url{https://github.com/conda-forge/miniforge}).
#'   Fails gracefully if conda is not found.
#'
#' @return Invisibly returns a character vector of package names that failed
#'   or were skipped.
#'
#' @importFrom utils install.packages
#' @export
install_marmot_extras <- function(include_suggests = TRUE, include_python = TRUE) {

  skipped <- character(0)

  # -- Bootstrap BiocManager --
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }

  # --- Helper: install a single GitHub package with graceful failure ---
  try_install_github <- function(pkg_name, repo) {
    if (requireNamespace(pkg_name, quietly = TRUE)) {
      return(TRUE)
    }
    message("Installing from GitHub: ", repo)
    tryCatch({
      if (requireNamespace("pak", quietly = TRUE)) {
        pak::pkg_install(repo, ask = FALSE)
      } else if (requireNamespace("remotes", quietly = TRUE)) {
        remotes::install_github(repo)
      } else {
        BiocManager::install(repo, ask = FALSE, update = FALSE)
      }
      TRUE
    }, error = function(e) {
      warning(
        pkg_name, " failed to install from GitHub (", repo, "): ",
        conditionMessage(e),
        "\nThe pipeline can still run without it.",
        call. = FALSE
      )
      FALSE
    })
  }

  # =========================================================================
  # Tier 1: CRAN + Bioconductor (reliable batch install)
  # =========================================================================
  message("\n-- Tier 1: Core packages (CRAN + Bioconductor) --")

  bioc_pkgs <- c(
    "BiocGenerics", "limma", "S4Vectors", "SummarizedExperiment",
    "SingleCellExperiment",
    "flowCore", "FlowSOM", "CATALYST", "diffcyt", "ComplexHeatmap",
    "Nebulosa", "PeacoQC", "flowAI", "scGate", "UCell"
  )

  cran_pkgs <- c(
    "ggplot2", "dplyr", "tidyr", "purrr", "tibble", "readr",
    "readxl", "reshape2", "matrixStats", "glue", "gtools",
    "future", "future.apply", "cowplot", "plotly", "ggrepel", "ggbeeswarm",
    "ggpubr", "ggprism", "RColorBrewer", "gridExtra", "kableExtra", "DT",
    "clustree", "rstatix", "colorspace", "viridis", "scales", "circlize",
    "htmltools", "knitr", "MASS", "rlang",
    "openxlsx", "writexl", "data.table", "ragg", "anndataR", "jsonlite",
    "qs2", "pacman", "scattermore", "scico", "ggnewscale", "pals",
    "patchwork", "ggridges", "zip",
    "reticulate", "BiocManager",
    "shiny", "shinydashboard", "shinyBS", "shinyalert", "shinycssloaders",
    "shinyjs", "shinyWidgets", "colourpicker", "sortable", "waiter",
    "fresh", "chameleon", "later"
  )

  # Bioconductor batch
  missing_bioc <- bioc_pkgs[!vapply(bioc_pkgs, requireNamespace,
                                     quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing_bioc) > 0) {
    message("Installing Bioconductor packages: ", paste(missing_bioc, collapse = ", "))
    BiocManager::install(missing_bioc, ask = FALSE, update = FALSE)
  }

  # CRAN batch
  missing_cran <- cran_pkgs[!vapply(cran_pkgs, requireNamespace,
                                     quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing_cran) > 0) {
    message("Installing CRAN packages: ", paste(missing_cran, collapse = ", "))
    install.packages(missing_cran)
  }

  # Post-check: warn about any still-missing Tier 1 packages
  still_missing_bioc <- bioc_pkgs[!vapply(bioc_pkgs, requireNamespace,
                                           quietly = TRUE, FUN.VALUE = logical(1))]
  still_missing_cran <- cran_pkgs[!vapply(cran_pkgs, requireNamespace,
                                           quietly = TRUE, FUN.VALUE = logical(1))]
  still_missing <- c(still_missing_bioc, still_missing_cran)
  if (length(still_missing) > 0) {
    warning(
      "These core packages failed to install: ",
      paste(still_missing, collapse = ", "),
      "\nTry installing them manually.",
      call. = FALSE
    )
  }

  # =========================================================================
  # Tier 2: GitHub packages (fragile, per-package tryCatch)
  # =========================================================================
  message("\n-- Tier 2: GitHub packages (optional, may require compilation) --")

  github_pkgs <- list(
    Rphenograph = "i-cyto/Rphenograph",
    fireworks   = "hypebright/fireworks"
  )

  for (pkg_name in names(github_pkgs)) {
    ok <- try_install_github(pkg_name, github_pkgs[[pkg_name]])
    if (!ok) {
      skipped <- c(skipped, pkg_name)
    }
  }

  if ("Rphenograph" %in% skipped) {
    message("Note: Rphenograph (C++) not installed -- use MfastPG or Mphenograph ",
            "(bundled in MARMOT, identical results)")
  }

  # =========================================================================
  # Tier 3: Optional CRAN (gated behind include_suggests)
  # =========================================================================
  if (include_suggests) {
    message("\n-- Tier 3: Optional packages --")
    if (!requireNamespace("Seurat", quietly = TRUE)) {
      message("Installing optional package: Seurat")
      tryCatch(
        install.packages("Seurat"),
        error = function(e) {
          warning("Seurat failed to install: ", conditionMessage(e), call. = FALSE)
          skipped <<- c(skipped, "Seurat")
        }
      )
    }
  }

  # =========================================================================
  # Python environment (gated behind include_python)
  # =========================================================================
  if (include_python) {
    message("\n-- Python environment (PARC/PaCMAP) --")
    message("  Requires conda or mamba (e.g. miniforge).")
    tryCatch({
      ok <- install_marmot_python()
      if (!isTRUE(ok)) {
        skipped <- c(skipped, "Python (PARC/PaCMAP)")
      }
    }, error = function(e) {
      warning(
        "Python setup failed: ", conditionMessage(e),
        "\nPARC and PaCMAP will not be available.",
        "\nThis does NOT affect the rest of MARMOT -- FlowSOM, MfastPG, and UMAP all work without Python.",
        "\nTo retry later: MARMOT::install_marmot_python()",
        call. = FALSE
      )
      skipped <<- c(skipped, "Python (PARC/PaCMAP)")
    })
  }

  # =========================================================================
  # Summary
  # =========================================================================
  cat("\n")
  if (length(skipped) > 0) {
    message(
      "Done! The following packages were skipped:\n  ",
      paste(skipped, collapse = ", "),
      "\nThe pipeline can still run without them."
    )
  } else {
    message("All done! All packages installed successfully.")
  }

  invisible(skipped)
}
