#' Set up the Python environment for PARC and PaCMAP
#'
#' Creates (or repairs) the \code{p4r} conda environment from the bundled
#' \code{environment.yml}, giving MARMOT access to PARC (clustering) and
#' PaCMAP (dimensionality reduction).
#'
#' @param conda Path to conda/mamba binary. If \code{NULL} (default),
#'   auto-detected via \code{reticulate::conda_binary()}.
#' @param force If \code{TRUE}, recreate the environment even if it already
#'   exists and appears healthy. Default \code{FALSE}.
#'
#' @return Invisibly returns the path to the Python binary in the environment.
#' @export
setup_python <- function(conda = NULL, force = FALSE) {

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required. Install it with install.packages('reticulate').")
  }

  if (is.null(conda)) {
    conda <- tryCatch(reticulate::conda_binary(), error = function(e) NULL)
  }
  if (is.null(conda) || !file.exists(conda)) {
    stop(
      "Could not find a conda/mamba installation.\n",
      "Install miniforge from: https://github.com/conda-forge/miniforge"
    )
  }
  message("Using conda binary: ", conda)

  envs <- reticulate::conda_list(conda = conda)

  if ("p4r" %in% envs$name && !force) {
    # Env exists — verify PARC and PaCMAP are actually importable in-process
    p4r_py <- envs$python[envs$name == "p4r"]
    parc_ok <- tryCatch({
      reticulate::use_condaenv("p4r", conda = conda, required = FALSE)
      reticulate::py_run_string("import parc",   convert = FALSE)
      TRUE
    }, error = function(e) FALSE)
    pcm_ok <- tryCatch({
      reticulate::py_run_string("import pacmap", convert = FALSE)
      TRUE
    }, error = function(e) FALSE)

    if (parc_ok && pcm_ok) {
      message("p4r environment found and PARC/PaCMAP are working.")
      return(invisible(p4r_py))
    }
    message("p4r environment exists but packages not importable — recreating...")

    # Remove before recreating (conda_create can't overwrite)
    system2(conda, c("env", "remove", "-n", "p4r", "-y"),
            stdout = FALSE, stderr = FALSE)
  }

  env_yml <- system.file("python", "environment.yml", package = "MARMOT")
  if (!nzchar(env_yml)) stop("Could not find bundled environment.yml.")

  message("Creating p4r conda environment from: ", env_yml)
  message("This may take a few minutes...")

  # On Apple Silicon, force arm64 packages so libpython is dlopen-able from arm64 R.
  # CONDA_SUBDIR is ignored on x86_64 and Linux, so it's safe to set unconditionally.
  conda_subdir <- if (.Machine$sizeof.pointer == 8 &&
                      Sys.info()[["machine"]] == "arm64") "osx-arm64" else NULL
  old_subdir <- Sys.getenv("CONDA_SUBDIR", unset = NA)
  if (!is.null(conda_subdir)) Sys.setenv(CONDA_SUBDIR = conda_subdir)
  on.exit({
    if (is.na(old_subdir)) Sys.unsetenv("CONDA_SUBDIR") else Sys.setenv(CONDA_SUBDIR = old_subdir)
  }, add = TRUE)

  reticulate::conda_create(envname = "p4r", environment = env_yml, conda = conda)

  message("Done! PARC and PaCMAP are now available.")
  message("The pipeline will auto-detect this environment.")

  envs2 <- reticulate::conda_list(conda = conda)
  invisible(envs2$python[envs2$name == "p4r"])
}
