#' @title Initialise Python Environment for MARMOT
#' @description
#' Helper functions to manage and use the Python environment required by MARMOT.
#' This includes automatic sourcing of the bundled \code{f_pacmap.py} and \code{f_parc.py} scripts.
#' @author Peter Leary
#' @name python_interface
NULL

#' @title Install Python Environment for MARMOT
#' @description
#' Installs a conda environment called \code{p4r} using the \code{environment.yml}
#' specification bundled with the package.
#' @return A message indicating success.
#' @export
#' @examples
#' \dontrun{
#' install_p4r_env()
#' }
install_p4r_env <- function() {
  yaml <- system.file("python", "environment.yml", package = "MARMOT", mustWork = TRUE)
  message("📦 Creating conda environment 'p4r' from ", yaml)
  reticulate::conda_create("p4r", yaml = yaml)
  message("✅ Environment 'p4r' installed.")
}

check_p4r_env <- function() {
  # check if conda is available
  if (is.null(reticulate::conda_binary())) {
    return(FALSE)
  }
  # check if p4r environment exists
  envs <- reticulate::conda_list()
  if (!"p4r" %in% envs$name) {
    return(FALSE)
  }
  TRUE
}

# Internal: set conda env on package load
.onLoad <- function(libname, pkgname) {
  if (is.null(reticulate::conda_binary())) {
    packageStartupMessage(
      "⚠️ Conda not found. Functions run_pacmap() and run_parc() will not work until you install conda.\n",
      "You can install a local miniconda with: reticulate::install_miniconda()."
    )
    return(invisible(NULL))
  }
  
  # Try to use 'p4r' environment if it exists
  envs <- reticulate::conda_list()
  if ("p4r" %in% envs$name) {
    reticulate::use_condaenv("p4r", required = FALSE)
    
    # Try to source bundled scripts if possible
    try({
      source_p4r_scripts()
    }, silent = TRUE)
  } else {
    packageStartupMessage(
      "⚠️ Conda environment 'p4r' not found. PaCMAP and PARC functions will be skipped.\n",
      "Run install_p4r_env() to create it."
    )
  }
}

#' @title Source bundled Python scripts
#' @description
#' Loads \code{f_pacmap.py} and \code{f_parc.py} into the current R session via \pkg{reticulate}.
#' @return Nothing, but defines Python functions in the R environment.
#' @export
#' @examples
#' \dontrun{
#' source_p4r_scripts()
#' }
source_p4r_scripts <- function() {
  pacmap_script <- system.file("python", "f_pacmap.py", package = "MARMOT", mustWork = TRUE)
  parc_script   <- system.file("python", "f_parc.py", package = "MARMOT", mustWork = TRUE)
  
  reticulate::source_python(pacmap_script)
  reticulate::source_python(parc_script)
  
  message("✅ Python scripts f_pacmap.py and f_parc.py have been loaded.")
}

#' @title Run PARC clustering
#' @description
#' Wrapper for the \code{parc_clust} function from the bundled Python script \code{f_parc.py}.
#' @param mat A numeric matrix (cells x features).
#' @param k Nearest neighbour parameter for PARC.
#' @param num_threads Number of threads to use.
#' @return An integer vector of cluster assignments.
#' @export
#' @examples
#' \dontrun{
#' mat <- matrix(rnorm(1000), nrow = 100, ncol = 10)
#' parc_clusters <- run_parc(mat, k = 30, num_threads = 4)
#' }
run_parc <- function(mat, k = 30L, num_threads = 1L) {
  if (!check_p4r_env()) {
    message("⚠️ 'p4r' env not available, switching clustering method to FlowSOM.")
    return(NULL)  # caller handles fallback
  }
  if (!exists("parc_clust", mode = "function")) {
    source_p4r_scripts()
  }
  parc_clust(mat, knn = as.integer(k), num_threads = as.integer(num_threads))
}

#' @title Run PaCMAP dimensionality reduction
#' @description
#' Wrapper for the \code{pacmap_fit} function from the bundled Python script \code{f_pacmap.py}.
#' @param mat A numeric data frame or matrix (cells x features).
#' @param verbose Logical; print progress messages.
#' @param apply_pca Logical; whether to apply PCA before PaCMAP.
#' @return A numeric matrix of reduced coordinates (cells x components).
#' @export
#' @examples
#' \dontrun{
#' mat <- matrix(rnorm(1000), nrow = 100, ncol = 10)
#' embedding <- run_pacmap(mat, verbose = TRUE, apply_pca = FALSE)
#' }
run_pacmap <- function(mat, verbose = TRUE, apply_pca = FALSE) {
  if (!check_p4r_env()) {
    message("⚠️ 'p4r' env not available, skipping PaCMAP.")
    return(NULL)
  }
  if (!exists("pacmap_fit", mode = "function")) {
    source_p4r_scripts()
  }
  pacmap_fit(as.data.frame(mat), verbose = verbose, apply_pca = apply_pca)
}