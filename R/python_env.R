#' @name marmot-python
#' @title MARMOT Python environment helpers
#'
#' @description
#' Functions for discovering, using, and installing the \code{p4r} conda
#' environment that provides PARC clustering and PaCMAP dimensionality
#' reduction. No external Python manager (basilisk, pyenv) is required --
#' only conda or mamba (e.g.
#' \href{https://github.com/conda-forge/miniforge}{miniforge}).
NULL

# Module-level cache — avoids re-binding reticulate on every pipeline call.
.marmot_py <- new.env(parent = emptyenv())
.marmot_py$bound     <- FALSE
.marmot_py$available <- FALSE
.marmot_py$path      <- NA_character_

#' Check Python availability for PARC/PaCMAP (read-only)
#'
#' Probes for a \code{p4r} conda environment and verifies that \code{parc}
#' and \code{pacmap} can be imported. Uses \code{system2()} so it never binds
#' reticulate to a Python session -- safe to call from status checks.
#'
#' @return A list with elements \code{available} (logical), \code{python_path}
#'   (character or \code{NA}), and \code{conda_path} (character or \code{NA}).
#' @export
marmot_python_status <- function() {
  result <- list(available = FALSE, python_path = NA_character_,
                 conda_path = NA_character_)


  # Find conda binary (prefer conda over mamba — reticulate::conda_list()

  # returns garbled results when called with the mamba binary)
  conda_bin <- Sys.which("conda")
  if (!nzchar(conda_bin)) {
    conda_bin <- tryCatch(reticulate::conda_binary(), error = function(e) NULL)
  }
  if (is.null(conda_bin) || !nzchar(conda_bin)) return(result)
  result$conda_path <- conda_bin


  # Look for p4r env
  envs <- tryCatch(reticulate::conda_list(conda = conda_bin),
                   error = function(e) NULL)
  if (is.null(envs) || !("p4r" %in% envs$name)) return(result)

  py_bin <- envs$python[envs$name == "p4r"]
  if (!file.exists(py_bin)) return(result)
  result$python_path <- py_bin

  # Verify imports via subprocess (never binds reticulate)
  ok <- tryCatch({
    quote_type <- if (.Platform$OS.type == "windows") "cmd" else "sh"
    out <- system2(py_bin, c("-c", shQuote("import parc; import pacmap",
                                           type = quote_type)),
                   stdout = TRUE, stderr = TRUE)
    is.null(attr(out, "status")) || identical(attr(out, "status"), 0L)
  }, error = function(e) FALSE)

  result$available <- isTRUE(ok)
  result
}

#' Bind the p4r Python environment for the current R session
#'
#' Called internally at the start of a pipeline run. Binds reticulate to the
#' \code{p4r} conda environment if available. Returns \code{TRUE} on success,
#' \code{FALSE} (with a warning) if Python is unavailable.
#'
#' @return Logical: \code{TRUE} if Python with PARC/PaCMAP is ready.
#' @keywords internal
use_marmot_python <- function() {
  # Return cached result if already attempted this session

  if (.marmot_py$bound) return(.marmot_py$available)

  # If reticulate is already bound to some Python, test that one
  if (reticulate::py_available(initialize = FALSE)) {
    ok <- tryCatch({
      reticulate::py_run_string("import parc; import pacmap", convert = FALSE)
      TRUE
    }, error = function(e) FALSE)
    .marmot_py$bound     <- TRUE
    .marmot_py$available <- ok
    if (ok) {
      .marmot_py$path <- reticulate::py_config()$python
      return(TRUE)
    }
    warning("Reticulate is already bound to a Python that lacks parc/pacmap.\n",
            "Mparc and Mpacmap (R fallbacks) will be used instead.",
            call. = FALSE)
    return(FALSE)
  }

  # Try to bind p4r (pass conda= explicitly — reticulate's auto-detection
  # returns garbled results when it finds mamba instead of conda)
  conda_bin <- Sys.which("conda")
  if (!nzchar(conda_bin)) conda_bin <- "auto"
  ok <- tryCatch({
    reticulate::use_condaenv("p4r", conda = conda_bin, required = TRUE)
    reticulate::py_run_string("import parc; import pacmap", convert = FALSE)
    TRUE
  }, error = function(e) FALSE)

  .marmot_py$bound     <- TRUE
  .marmot_py$available <- ok
  if (ok) {
    .marmot_py$path <- reticulate::py_config()$python
  } else {
    warning("Python environment 'p4r' not found or missing parc/pacmap.\n",
            "Mparc and Mpacmap (R fallbacks) will be used instead.\n",
            "For Python versions, run MARMOT::install_marmot_python().",
            call. = FALSE)
  }
  ok
}

#' Install the p4r conda environment for PARC/PaCMAP
#'
#' Creates (or recreates) the \code{p4r} conda environment from the bundled
#' \code{environment.yml}. Requires conda or mamba (e.g.
#' \href{https://github.com/conda-forge/miniforge}{miniforge}).
#'
#' @param force If \code{TRUE}, remove and recreate the environment even if
#'   it already exists and appears healthy. Default \code{FALSE}.
#' @return Invisibly returns \code{TRUE} on success, \code{FALSE} on failure.
#' @export
install_marmot_python <- function(force = FALSE) {

  # Find conda (prefer conda over mamba — reticulate::conda_create uses
  # conda_list internally for verification, which is broken with mamba)
  conda_bin <- Sys.which("conda")
  if (!nzchar(conda_bin)) {
    conda_bin <- tryCatch(reticulate::conda_binary(), error = function(e) NULL)
  }
  if (is.null(conda_bin) || !nzchar(conda_bin) || !file.exists(conda_bin)) {
    message("No conda/mamba installation found.")
    message("Install miniforge from: https://github.com/conda-forge/miniforge")
    message("Then re-run MARMOT::install_marmot_python()")
    return(invisible(FALSE))
  }
  message("Using conda binary: ", conda_bin)

  # Check existing env
  if (!force) {
    status <- marmot_python_status()
    if (status$available) {
      message("p4r environment already exists and is working.")
      message("Python: ", status$python_path)
      message("Use install_marmot_python(force = TRUE) to recreate.")
      return(invisible(TRUE))
    }
  }

  # Remove existing if force
  if (force) {
    message("Removing existing p4r environment...")
    tryCatch(
      reticulate::conda_remove(envname = "p4r", conda = conda_bin),
      error = function(e) NULL
    )
  }

  # On Apple Silicon, force arm64 packages
  if (.Machine$sizeof.pointer == 8 && Sys.info()[["machine"]] == "arm64") {
    old_subdir <- Sys.getenv("CONDA_SUBDIR", unset = NA)
    Sys.setenv(CONDA_SUBDIR = "osx-arm64")
    on.exit({
      if (is.na(old_subdir)) Sys.unsetenv("CONDA_SUBDIR")
      else Sys.setenv(CONDA_SUBDIR = old_subdir)
    }, add = TRUE)
  }

  env_yml <- system.file("python", "environment.yml", package = "MARMOT")
  if (!nzchar(env_yml)) {
    stop("Could not find bundled environment.yml in MARMOT package.")
  }

  message("Creating p4r conda environment from: ", env_yml)
  message("This may take a few minutes...")
  tryCatch({
    reticulate::conda_create(envname = "p4r", environment = env_yml,
                             conda = conda_bin,
                             additional_create_args = "--yes")
    message("Done! PARC and PaCMAP are now available.")
    message("The pipeline will auto-detect this environment.")
    invisible(TRUE)
  }, error = function(e) {
    warning("Failed to create p4r environment: ", conditionMessage(e),
            call. = FALSE)
    invisible(FALSE)
  })
}
