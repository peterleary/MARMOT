#' Install everything MARMOT can use
#'
#' Installs every package MARMOT uses that is not installed yet, as
#' ready-built copies: nothing is compiled. The list is read from MARMOT's own
#' DESCRIPTION, so nothing is missed. Packages come from MARMOT's r-universe
#' (ready-built MARMOT, Rphenograph and the GitHub-only packages), CRAN and
#' Bioconductor. With `include_python = TRUE`, it also sets up the original
#' Python PARC and PaCMAP (see [install_marmot_python()]).
#'
#' MARMOT encourages the original implementations: Rphenograph, and the Python
#' PARC and PaCMAP. Its own R versions (Mphenograph, Mparc, Mpacmap) are there
#' as fallbacks, and the report always says which one ran.
#'
#' @param include_suggests If `TRUE` (the default), install the optional
#'   packages too (QC, scGate, Seurat, Harmony, Rphenograph and so on).
#' @param include_python If `TRUE` (the default), also set up the Python
#'   environment for the original PARC and PaCMAP.
#' @return Invisibly, the packages that could not be installed.
#' @importFrom utils install.packages
#' @export
install_marmot_extras <- function(include_suggests = TRUE, include_python = TRUE) {
  # Never compile: on a machine without Rtools or the Xcode tools, R would
  # otherwise ask, or fail. Ready-built copies are always used.
  old_opt <- options(install.packages.compile.from.source = "never")
  on.exit(options(old_opt), add = TRUE)
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager", repos = "https://cloud.r-project.org")
  }
  repos <- marmot_repos()

  deps <- .marmot_dependencies()
  wanted <- c(deps$imports, if (include_suggests) deps$suggests)
  installed <- function(p) vapply(p, requireNamespace, logical(1), quietly = TRUE)
  missing <- wanted[!installed(wanted)]

  if (length(missing)) {
    message("Installing ", length(missing), " package(s): ", paste(missing, collapse = ", "))
    tryCatch(
      install.packages(missing, repos = repos, dependencies = c("Depends", "Imports", "LinkingTo")),
      error = function(e) message("Some packages could not be installed: ", conditionMessage(e))
    )
  } else {
    message("Every R package MARMOT uses is already installed.")
  }
  failed <- wanted[!installed(wanted)]

  if (include_python) {
    message("\n-- The original PARC and PaCMAP (Python) --")
    ok <- tryCatch(isTRUE(install_marmot_python()), error = function(e) {
      message("Python setup did not finish: ", conditionMessage(e))
      FALSE
    })
    if (!ok) failed <- c(failed, "Python (PARC, PaCMAP)")
  }

  cat("\n")
  if (length(failed)) {
    message("Done, except for: ", paste(failed, collapse = ", "), ".\n",
            "MARMOT still runs without them. Run MARMOT::check_setup() to see what that means, ",
            "and MARMOT::install_marmot_extras() again to retry.")
  } else {
    message("All done: everything MARMOT can use is installed.")
  }
  invisible(failed)
}
