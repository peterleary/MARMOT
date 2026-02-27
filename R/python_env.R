#' Basilisk Python environment for PARC and PaCMAP
#'
#' A \code{\link[basilisk]{BasiliskEnvironment}} providing the \code{p4r} conda
#' environment used by MARMOT for PARC clustering and PaCMAP dimensionality
#' reduction. The environment is created automatically on first use via
#' \pkg{basilisk} and managed in the user's package cache.
#'
#' @format A \code{BasiliskEnvironment} object.
#' @importFrom basilisk BasiliskEnvironment
p4r_env <- BasiliskEnvironment(
  envname  = "p4r_env",
  pkgname  = "MARMOT",
  packages = c(
    "python=3.9",
    "numpy==1.22.3",
    "scikit-learn==1.0.2",
    "python-annoy==1.17.3",
    "numba==0.60.0",
    "llvmlite==0.43.0",
    "python-igraph==0.11.6",
    "leidenalg==0.10.2",
    "hnswlib==0.8.0"
  ),
  pip = c("pacmap==0.8.0", "parc==0.40")
)
