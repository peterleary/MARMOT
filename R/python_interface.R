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
  
  if (!file.exists(yaml)) {
    stop("❌ Environment file not found at: ", yaml)
  }
  
  if (is.null(reticulate::conda_binary())) {
    stop("❌ Conda not found. Please install conda or miniconda first.\n",
         "You can install with: reticulate::install_miniconda()")
  }
  
  message("📦 Creating conda environment 'p4r' from ", yaml)
  
  tryCatch({
    reticulate::conda_create("p4r", yaml = yaml, forge = TRUE)
    message("✅ Environment 'p4r' installed successfully.")
  }, error = function(e) {
    stop("❌ Failed to create conda environment: ", e$message)
  })
}

#' @title Check if p4r environment exists and is usable
#' @description Internal function to verify conda and p4r environment availability
#' @return Logical indicating if environment is ready
check_p4r_env <- function() {
  if (is.null(reticulate::conda_binary())) {
    return(FALSE)
  }
  
  tryCatch({
    envs <- reticulate::conda_list()
    if (!"p4r" %in% envs$name) {
      return(FALSE)
    }
    
    reticulate::use_condaenv("p4r", required = FALSE)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Internal: set conda env on package load
.onLoad <- function(libname, pkgname) {
  if (is.null(reticulate::conda_binary())) {
    packageStartupMessage(
      "⚠️  Conda not found. Functions run_pacmap() and run_parc() will not work until you install conda.\n",
      "   You can install with: reticulate::install_miniconda()"
    )
    return(invisible(NULL))
  }
  
  if (check_p4r_env()) {
    tryCatch({
      reticulate::use_condaenv("p4r", required = TRUE)
      source_p4r_scripts()
    }, error = function(e) {
      packageStartupMessage(
        "⚠️  Could not activate 'p4r' environment: ", e$message, "\n",
        "   Run install_p4r_env() to create it."
      )
    })
  } else {
    packageStartupMessage(
      "⚠️  Conda environment 'p4r' not found. PaCMAP and PARC functions will be skipped.\n",
      "   Run install_p4r_env() to create it."
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
  if (!check_p4r_env()) {
    stop("❌ Python environment 'p4r' is not available. Run install_p4r_env() first.")
  }
  
  pacmap_script <- system.file("python", "f_pacmap.py", package = "MARMOT", mustWork = TRUE)
  parc_script   <- system.file("python", "f_parc.py", package = "MARMOT", mustWork = TRUE)
  
  if (!file.exists(pacmap_script)) {
    stop("❌ PaCMAP script not found at: ", pacmap_script)
  }
  
  if (!file.exists(parc_script)) {
    stop("❌ PARC script not found at: ", parc_script)
  }
  
  tryCatch({
    reticulate::source_python(pacmap_script)
    reticulate::source_python(parc_script)
    message("✅ Python scripts f_pacmap.py and f_parc.py have been loaded.")
  }, error = function(e) {
    stop("❌ Failed to load Python scripts: ", e$message, "\n",
         "   Check that the 'p4r' environment has all required packages installed.")
  })
}

#' @title Run PARC clustering
#' @description
#' Wrapper for the \code{parc_clust} function from the bundled Python script \code{f_parc.py}.
#' @param mat A numeric matrix (cells x features).
#' @param k Nearest neighbour parameter for PARC.
#' @param num_threads Number of threads to use.
#' @return An integer vector of cluster assignments, or NULL if PARC unavailable.
#' @export
#' @examples
#' \dontrun{
#' mat <- matrix(rnorm(1000), nrow = 100, ncol = 10)
#' parc_clusters <- run_parc(mat, k = 30, num_threads = 4)
#' }
run_parc <- function(mat, k = 30L, num_threads = 1L) {
  if (!is.matrix(mat) && !is.data.frame(mat)) {
    stop("❌ Input 'mat' must be a matrix or data frame")
  }
  
  if (!is.numeric(k) || k < 1) {
    stop("❌ Parameter 'k' must be a positive integer")
  }
  
  if (!is.numeric(num_threads) || num_threads < 1) {
    stop("❌ Parameter 'num_threads' must be a positive integer")
  }
  
  if (!check_p4r_env()) {
    message("⚠️  'p4r' environment not available, switching clustering method to FlowSOM.")
    return(NULL)
  }
  
  # Ensure scripts are loaded and Python functions available
  py_main <- reticulate::import_main(convert = FALSE)
  
  if (is.null(py_main$parc_clust)) {
    tryCatch({
      source_p4r_scripts()
      py_main <- reticulate::import_main(convert = FALSE)
    }, error = function(e) {
      message("⚠️  Could not load Python scripts: ", e$message)
      return(NULL)
    })
  }
  
  if (is.null(py_main$parc_clust)) {
    message("⚠️  PARC function not available, switching to FlowSOM.")
    return(NULL)
  }
  
  # Run PARC clustering
  tryCatch({
    result <- py_main$parc_clust(mat, knn = as.integer(k), num_threads = as.integer(num_threads))
    
    if (is.null(result) || length(result) != nrow(mat)) {
      warning("⚠️  PARC returned unexpected output, switching to FlowSOM")
      return(NULL)
    }
    
    return(as.integer(result))
  }, error = function(e) {
    message("⚠️  PARC clustering failed: ", e$message, ". Switching to FlowSOM.")
    return(NULL)
  })
}

#' @title Run PaCMAP dimensionality reduction
#' @description
#' Wrapper for the \code{pacmap_fit} function from the bundled Python script \code{f_pacmap.py}.
#' @param mat A numeric data frame or matrix (cells x features).
#' @param verbose Logical; print progress messages.
#' @param apply_pca Logical; whether to apply PCA before PaCMAP.
#' @return A numeric matrix of reduced coordinates (cells x components), or NULL if unavailable.
#' @export
#' @examples
#' \dontrun{
#' mat <- matrix(rnorm(1000), nrow = 100, ncol = 10)
#' embedding <- run_pacmap(mat, verbose = TRUE, apply_pca = FALSE)
#' }
run_pacmap <- function(mat, verbose = TRUE, apply_pca = FALSE) {
  if (!is.matrix(mat) && !is.data.frame(mat)) {
    stop("❌ Input 'mat' must be a matrix or data frame")
  }
  
  if (!is.logical(verbose)) {
    stop("❌ Parameter 'verbose' must be logical")
  }
  
  if (!is.logical(apply_pca)) {
    stop("❌ Parameter 'apply_pca' must be logical")
  }
  
  if (!check_p4r_env()) {
    message("⚠️  'p4r' environment not available, skipping PaCMAP.")
    return(NULL)
  }
  
  # Ensure scripts are loaded and Python functions available
  py_main <- reticulate::import_main(convert = FALSE)
  
  if (is.null(py_main$pacmap_fit)) {
    tryCatch({
      source_p4r_scripts()
      py_main <- reticulate::import_main(convert = FALSE)
    }, error = function(e) {
      message("⚠️  Could not load Python scripts: ", e$message)
      return(NULL)
    })
  }
  
  if (is.null(py_main$pacmap_fit)) {
    message("⚠️  PaCMAP function not available, skipping PaCMAP.")
    return(NULL)
  }
  
  # Convert to data frame for Python compatibility
  mat_df <- as.data.frame(mat)
  
  # Run PaCMAP
  tryCatch({
    result <- py_main$pacmap_fit(mat_df, verbose = verbose, apply_pca = apply_pca)
    
    if (is.null(result) || nrow(result) != nrow(mat)) {
      warning("⚠️  PaCMAP returned unexpected output")
      return(NULL)
    }
    
    return(as.matrix(result))
  }, error = function(e) {
    message("⚠️  PaCMAP dimensionality reduction failed: ", e$message)
    return(NULL)
  })
}