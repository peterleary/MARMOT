#' @name marmot-python-subprocess
#' @title Run PaCMAP / PARC in an isolated Python subprocess
#'
#' @description
#' On macOS arm64, running Python's \code{pacmap} / \code{parc} in the same
#' process as R (via \code{reticulate}) segfaults inside ANNOY/hnswlib because
#' R's \code{libomp.dylib} (linked by \code{data.table} and other CRAN
#' binaries) and conda's \code{libomp.dylib} (linked by the \code{annoy}
#' wheel) cannot coexist as two separate OpenMP runtimes in one process.
#'
#' These helpers bypass reticulate entirely: the matrix is written to a temp
#' file as raw \code{float64}, a subprocess is spawned against the \code{p4r}
#' conda env's Python binary, and the result is read back from another temp
#' file. Python never shares a process with R, so no libomp collision.
NULL

# Locate the p4r python binary without binding reticulate. Cached per session
# on the .marmot_py env defined in R/python_env.R.
.find_p4r_python <- function() {
  if (isTRUE(.marmot_py$subprocess_checked)) {
    return(if (is.na(.marmot_py$subprocess_path)) NULL
           else .marmot_py$subprocess_path)
  }
  status <- tryCatch(marmot_python_status(), error = function(e) NULL)
  py <- if (!is.null(status) && isTRUE(status$available)) status$python_path
        else NA_character_
  .marmot_py$subprocess_checked <- TRUE
  .marmot_py$subprocess_path    <- py
  if (is.na(py)) NULL else py
}

# Write an R matrix as raw float64 (column-major — R's native storage order).
.write_matrix_bin <- function(X, path) {
  stopifnot(is.matrix(X), is.numeric(X))
  con <- file(path, "wb")
  on.exit(close(con), add = TRUE)
  writeBin(as.double(X), con, size = 8L, endian = "little")
}

#' Run PaCMAP in a Python subprocess
#'
#' @param X Numeric matrix, rows = cells, cols = features.
#' @param n_components,n_neighbors,MN_ratio,FP_ratio,distance,lr,num_iters,apply_pca,random_state,verbose
#'   Passed through to \code{pacmap.PaCMAP}.
#' @param timeout Seconds before the subprocess is killed. Default 600.
#' @return A \code{nrow(X) x n_components} numeric matrix, or \code{NULL} on
#'   failure (caller should fall back to \code{Mpacmap}).
#' @keywords internal
run_pacmap_subprocess <- function(X,
                                  n_components = 2L,
                                  n_neighbors = 10L,
                                  MN_ratio = 0.5,
                                  FP_ratio = 2.0,
                                  distance = "euclidean",
                                  lr = 1.0,
                                  num_iters = 450L,
                                  apply_pca = TRUE,
                                  random_state = NULL,
                                  verbose = FALSE,
                                  timeout = 600) {
  py <- .find_p4r_python()
  if (is.null(py)) {
    message("Python PaCMAP unavailable (p4r env not found).")
    return(NULL)
  }
  script <- system.file("python", "run_pacmap.py", package = "MARMOT")
  if (!nzchar(script)) {
    message("run_pacmap.py not found in installed MARMOT package.")
    return(NULL)
  }

  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X); d <- ncol(X)

  in_file  <- tempfile(fileext = ".bin")
  out_file <- tempfile(fileext = ".bin")
  on.exit(unlink(c(in_file, out_file), force = TRUE), add = TRUE)
  .write_matrix_bin(X, in_file)

  args <- c(
    shQuote(script),
    "--input",  shQuote(in_file),
    "--output", shQuote(out_file),
    "--rows",   n,
    "--cols",   d,
    "--n-components", n_components,
    "--n-neighbors",  n_neighbors,
    "--mn-ratio",     MN_ratio,
    "--fp-ratio",     FP_ratio,
    "--distance",     shQuote(distance),
    "--lr",           lr,
    "--num-iters",    num_iters
  )
  if (isTRUE(apply_pca)) args <- c(args, "--apply-pca")
  if (isTRUE(verbose))   args <- c(args, "--verbose")
  if (!is.null(random_state)) {
    args <- c(args, "--random-state", as.integer(random_state))
  }

  status <- tryCatch(
    system2(py, args, stdout = "", stderr = "", timeout = timeout),
    error = function(e) { message("PaCMAP subprocess error: ", e$message); -1L }
  )
  if (!identical(as.integer(status), 0L)) {
    message("PaCMAP subprocess exited with status ", status, ".")
    return(NULL)
  }
  if (!file.exists(out_file)) {
    message("PaCMAP subprocess produced no output.")
    return(NULL)
  }

  # Read back: Python wrote row-major float64 (n x n_components)
  con <- file(out_file, "rb")
  on.exit(close(con), add = TRUE)
  flat <- readBin(con, what = "double", n = n * n_components,
                  size = 8L, endian = "little")
  if (length(flat) != n * n_components) {
    message("PaCMAP subprocess output truncated: got ", length(flat),
            " doubles, expected ", n * n_components, ".")
    return(NULL)
  }
  matrix(flat, nrow = n, ncol = n_components, byrow = TRUE)
}

#' Run PARC clustering in a Python subprocess
#'
#' @param X Numeric matrix, rows = cells, cols = features.
#' @param knn,num_threads,random_seed,resolution Passed to \code{parc.PARC}.
#' @param timeout Seconds before the subprocess is killed. Default 1200.
#' @return Integer vector of cluster labels (length \code{nrow(X)}), or
#'   \code{NULL} on failure (caller should fall back to \code{Mparc}).
#' @keywords internal
run_parc_subprocess <- function(X,
                                knn = 30L,
                                num_threads = -1L,
                                random_seed = 42L,
                                resolution = 1.0,
                                timeout = 1200) {
  py <- .find_p4r_python()
  if (is.null(py)) {
    message("Python PARC unavailable (p4r env not found).")
    return(NULL)
  }
  script <- system.file("python", "run_parc.py", package = "MARMOT")
  if (!nzchar(script)) {
    message("run_parc.py not found in installed MARMOT package.")
    return(NULL)
  }

  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X); d <- ncol(X)

  in_file  <- tempfile(fileext = ".bin")
  out_file <- tempfile(fileext = ".bin")
  on.exit(unlink(c(in_file, out_file), force = TRUE), add = TRUE)
  .write_matrix_bin(X, in_file)

  args <- c(
    shQuote(script),
    "--input",  shQuote(in_file),
    "--output", shQuote(out_file),
    "--rows",   n,
    "--cols",   d,
    "--knn",         as.integer(knn),
    "--num-threads", as.integer(num_threads),
    "--random-seed", as.integer(random_seed),
    "--resolution",  resolution
  )

  status <- tryCatch(
    system2(py, args, stdout = "", stderr = "", timeout = timeout),
    error = function(e) { message("PARC subprocess error: ", e$message); -1L }
  )
  if (!identical(as.integer(status), 0L)) {
    message("PARC subprocess exited with status ", status, ".")
    return(NULL)
  }
  if (!file.exists(out_file)) {
    message("PARC subprocess produced no output.")
    return(NULL)
  }

  con <- file(out_file, "rb")
  on.exit(close(con), add = TRUE)
  labels <- readBin(con, what = "integer", n = n, size = 4L,
                    endian = "little", signed = TRUE)
  if (length(labels) != n) {
    message("PARC subprocess output truncated: got ", length(labels),
            " labels, expected ", n, ".")
    return(NULL)
  }
  labels
}
