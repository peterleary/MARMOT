#' Check MARMOT installation status
#'
#' Prints a formatted status report showing which packages and the Python
#' environment are installed and available. Core packages (CRAN/Bioconductor)
#' show a red X if missing; GitHub and optional packages show a yellow warning.
#'
#' @return Invisibly returns a data.frame with columns \code{package},
#'   \code{type}, \code{status}, and \code{version}.
#' @export
check_setup <- function() {

  check_pkg <- function(pkg) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      ver <- as.character(utils::packageVersion(pkg))
      list(status = "installed", version = ver)
    } else {
      list(status = "MISSING", version = NA_character_)
    }
  }

  # -- Core: CRAN + Bioconductor (should always install) --
  core_pkgs <- c(
    "MARMOT",
    # Bioconductor
    "BiocGenerics", "limma", "S4Vectors", "SummarizedExperiment",
    "SingleCellExperiment",
    "flowCore", "FlowSOM", "CATALYST", "diffcyt", "ComplexHeatmap",
    "Nebulosa", "PeacoQC", "flowAI", "scGate", "UCell",
    # CRAN (key pipeline + Shiny)
    "ggplot2", "dplyr", "tidyr", "purrr", "tibble", "readr",
    "htmltools", "knitr", "MASS", "rlang",
    "reticulate", "future", "pacman",
    "plotly", "qs2", "shiny", "shinydashboard",
    "data.table", "anndataR", "jsonlite", "patchwork"
  )

  # -- GitHub packages (fragile, known compilation issues) --
  github_pkgs <- c("Rphenograph", "fireworks")

  # -- Optional (gated behind include_suggests) --
  optional_pkgs <- c("Seurat", "ragg")

  all_pkgs <- c(core_pkgs, github_pkgs, optional_pkgs)
  types <- c(
    rep("core",     length(core_pkgs)),
    rep("github",   length(github_pkgs)),
    rep("optional", length(optional_pkgs))
  )

  results <- data.frame(
    package = all_pkgs,
    type    = types,
    status  = NA_character_,
    version = NA_character_,
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(results))) {
    info <- check_pkg(results$package[i])
    results$status[i]  <- info$status
    results$version[i] <- info$version
  }

  # Print report
  cat("\n=== MARMOT Setup Check ===\n\n")

  # Core packages (red X if missing)
  cat("-- Core packages (CRAN / Bioconductor) --\n")
  core <- results[results$type == "core", ]
  for (i in seq_len(nrow(core))) {
    icon <- if (core$status[i] == "installed") "\u2705" else "\u274c"
    ver  <- if (!is.na(core$version[i])) paste0(" (", core$version[i], ")") else ""
    cat(sprintf("  %s %s%s\n", icon, core$package[i], ver))
  }

  # GitHub packages (yellow warning if missing)
  cat("\n-- GitHub packages (may require compilation) --\n")
  gh <- results[results$type == "github", ]
  for (i in seq_len(nrow(gh))) {
    icon <- if (gh$status[i] == "installed") "\u2705" else "\u26a0\ufe0f"
    ver  <- if (!is.na(gh$version[i])) paste0(" (", gh$version[i], ")") else ""
    cat(sprintf("  %s %s%s\n", icon, gh$package[i], ver))
  }

  cat("\n  i  Clustering: MfastPG + Mphenograph are always available (bundled in MARMOT)\n")
  cat("     Rphenograph (C++) is optional -- install via install_marmot_extras()\n")

  # Optional packages (yellow warning if missing)
  cat("\n-- Optional packages --\n")
  opt <- results[results$type == "optional", ]
  for (i in seq_len(nrow(opt))) {
    icon <- if (opt$status[i] == "installed") "\u2705" else "\u26a0\ufe0f"
    ver  <- if (!is.na(opt$version[i])) paste0(" (", opt$version[i], ")") else ""
    cat(sprintf("  %s %s%s\n", icon, opt$package[i], ver))
  }

  # Rendering (Quarto)
  cat("\n-- Rendering --\n")
  quarto_bin <- Sys.which("quarto")
  if (nzchar(quarto_bin)) {
    quarto_ver <- tryCatch(
      system2("quarto", "--version", stdout = TRUE, stderr = TRUE),
      error = function(e) "unknown"
    )
    cat(sprintf("  \u2705 Quarto: %s (%s)\n", quarto_ver, quarto_bin))
  } else {
    cat("  \u274c Quarto not found \u2014 install from https://quarto.org/docs/get-started/\n")
  }

  # Python environment (conda p4r)
  cat("\n-- Python (PARC/PaCMAP) --\n")
  py_status <- tryCatch(marmot_python_status(), error = function(e) {
    list(available = FALSE, python_path = NA_character_, conda_path = NA_character_)
  })
  if (py_status$available) {
    cat(sprintf("  \u2705 PARC and PaCMAP available (%s)\n", py_status$python_path))
  } else if (!is.na(py_status$conda_path)) {
    cat("  \u26a0\ufe0f p4r environment not found (conda detected)\n")
    cat("     Run: MARMOT::install_marmot_python()\n")
    cat("     Mparc and Mpacmap (R fallbacks) are always available\n")
  } else {
    cat("  \u26a0\ufe0f No conda/mamba found\n")
    cat("     Install miniforge: https://github.com/conda-forge/miniforge\n")
    cat("     Then run: MARMOT::install_marmot_python()\n")
    cat("     Mparc and Mpacmap (R fallbacks) are always available\n")
  }

  cat("\n")
  invisible(results)
}
