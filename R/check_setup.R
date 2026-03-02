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
    "data.table", "arrow", "jsonlite", "patchwork", "ragg"
  )

  # -- GitHub packages (fragile, known compilation issues) --
  github_pkgs <- c("Rphenograph", "fireworks", "FastPG")

  # -- Optional (gated behind include_suggests) --
  optional_pkgs <- c("Seurat")

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

  # Python environment
  cat("\n-- Python (PARC/PaCMAP) --\n")
  if (requireNamespace("reticulate", quietly = TRUE)) {
    conda <- tryCatch(reticulate::conda_binary(), error = function(e) NULL)
    if (!is.null(conda)) {
      cat("  \u2705 conda found:", conda, "\n")
      envs <- tryCatch(reticulate::conda_list(conda = conda), error = function(e) NULL)
      if (!is.null(envs) && "p4r" %in% envs$name) {
        cat("  \u2705 p4r environment found\n")
        # Load in-process - avoids SIP stripping DYLD in subprocesses
        parc_ok <- tryCatch({
          reticulate::use_condaenv("p4r", conda = conda, required = FALSE)
          reticulate::py_run_string("import parc",   convert = FALSE)
          TRUE
        }, error = function(e) FALSE)
        pcm_ok <- tryCatch({
          reticulate::py_run_string("import pacmap", convert = FALSE)
          TRUE
        }, error = function(e) FALSE)
        cat(sprintf("  %s PARC: %s\n",
                    if (parc_ok) "\u2705" else "\u274c",
                    if (parc_ok) "importable" else "not importable \u2014 run MARMOT::setup_python()"))
        cat(sprintf("  %s PaCMAP: %s\n",
                    if (pcm_ok) "\u2705" else "\u274c",
                    if (pcm_ok) "importable" else "not importable \u2014 run MARMOT::setup_python()"))
      } else {
        cat("  \u274c p4r environment not found \u2014 run MARMOT::setup_python()\n")
      }
    } else {
      cat("  \u274c conda not found \u2014 install miniforge from https://github.com/conda-forge/miniforge\n")
    }
  } else {
    cat("  \u274c reticulate not installed\n")
  }

  cat("\n")
  invisible(results)
}
