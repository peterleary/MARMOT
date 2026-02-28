#' Install all MARMOT dependencies
#'
#' Checks for and installs all packages required by the MARMOT pipeline,
#' including CRAN, Bioconductor, and GitHub packages.
#'
#' @param include_suggests If \code{TRUE} (the default), also install optional
#'   packages (FastPG, flowAI, PeacoQC, SCpubr, scGate, UCell).
#' @param include_python If \code{TRUE}, also set up the Python conda
#'   environment for PARC/PaCMAP via \code{\link{setup_python}}.
#'   Default \code{FALSE}.
#'
#' @importFrom utils install.packages
#' @export
install_dependencies <- function(include_suggests = TRUE, include_python = FALSE) {

  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }

  # -- Core Bioconductor packages --
  bioc_pkgs <- c(
    "BiocGenerics", "S4Vectors", "SummarizedExperiment", "SingleCellExperiment",
    "flowCore", "FlowSOM", "CATALYST", "diffcyt", "Nebulosa", "slingshot"
  )
  missing_bioc <- bioc_pkgs[!sapply(bioc_pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing_bioc) > 0) {
    message("Installing Bioconductor packages: ", paste(missing_bioc, collapse = ", "))
    BiocManager::install(missing_bioc, ask = FALSE, update = FALSE)
  }

  # -- CRAN packages --
  cran_pkgs <- c(
    "ggplot2", "dplyr", "tidyr", "purrr", "tibble", "readr",
    "readxl", "reshape2", "matrixStats", "qs", "glue", "gtools",
    "future", "future.apply", "reticulate", "rmarkdown",
    "ComplexHeatmap", "circlize", "plotly", "cowplot", "ggpubr", "ggprism",
    "ggrepel", "ggbeeswarm", "RColorBrewer", "gridExtra", "kableExtra", "DT",
    "clustree", "rstatix", "colorspace", "viridis", "scales", "scattermore",
    "scico", "ggnewscale", "pals",
    "openxlsx", "writexl",
    "shiny", "shinydashboard", "shinyBS", "shinyalert", "shinycssloaders",
    "shinyjs", "shinyWidgets", "colourpicker", "sortable", "waiter", "fresh", "ragg",
    "chameleon", "BiocManager",
    "pacman", "data.table", "zip", "later"
  )
  missing_cran <- cran_pkgs[!sapply(cran_pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing_cran) > 0) {
    message("Installing CRAN packages: ", paste(missing_cran, collapse = ", "))
    install.packages(missing_cran)
  }

  # -- GitHub packages --
  github_pkgs <- list(
    Rphenograph = "i-cyto/Rphenograph",
    fireworks   = "hypebright/fireworks",
    SCpubr      = "enblacar/SCpubr"
  )
  for (pkg_name in names(github_pkgs)) {
    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      message("Installing from GitHub: ", github_pkgs[[pkg_name]])
      if (requireNamespace("pak", quietly = TRUE)) {
        pak::pkg_install(github_pkgs[[pkg_name]], ask = FALSE)
      } else {
        remotes::install_github(github_pkgs[[pkg_name]])
      }
    }
  }

  # -- Optional / Suggests --
  if (include_suggests) {
    suggests <- list(
      FastPG    = "sararselitsky/FastPG",
      flowAI    = "flowAI",
      PeacoQC   = "PeacoQC",
      scGate    = "scGate",
      UCell     = "UCell",
      Seurat    = "Seurat"
    )
    for (pkg_name in names(suggests)) {
      if (!requireNamespace(pkg_name, quietly = TRUE)) {
        src <- suggests[[pkg_name]]
        message("Installing optional package: ", pkg_name)
        if (grepl("/", src)) {
          # GitHub
          if (requireNamespace("pak", quietly = TRUE)) {
            pak::pkg_install(src, ask = FALSE)
          } else {
            remotes::install_github(src)
          }
        } else {
          # Bioconductor
          BiocManager::install(src, ask = FALSE, update = FALSE)
        }
      }
    }
  }

  # -- Python --
  if (include_python) {
    setup_python()
  }

  message("All done!")
}
