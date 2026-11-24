#' Check MARMOT installation status
#'
#' Says in plain words what this computer can do with MARMOT, and for anything
#' missing, the one command that fixes it. The packages checked are read from
#' MARMOT's own DESCRIPTION, so the check never drifts from what MARMOT uses.
#'
#' @return Invisibly, a data frame with one row per package: `package`, `type`
#'   ("needed" or "optional"), `status` and `version`.
#' @export
check_setup <- function() {
  deps <- .marmot_dependencies()
  version_of <- function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
  results <- data.frame(
    package = c(deps$imports, deps$suggests),
    type    = c(rep("needed", length(deps$imports)), rep("optional", length(deps$suggests))),
    stringsAsFactors = FALSE
  )
  results$version <- vapply(results$package, version_of, character(1))
  results$status  <- ifelse(is.na(results$version), "MISSING", "installed")
  has <- function(p) all(p %in% results$package[results$status == "installed"])
  fix <- "MARMOT::install_marmot_extras()"
  ok <- function(x) cat("  \u2705 ", x, "\n", sep = "")
  no <- function(x, how) cat("  \u274c ", x, "\n       Fix: ", how, "\n", sep = "")
  maybe <- function(x, how) cat("  \u26a0\ufe0f  ", x, "\n       To add it: ", how, "\n", sep = "")

  cat("\n=== MARMOT setup check ===\n\n")
  cat("MARMOT ", version_of("MARMOT"), ", R ", as.character(getRversion()), "\n\n", sep = "")

  needed_missing <- results$package[results$type == "needed" & results$status == "MISSING"]
  if (!length(needed_missing)) ok("You can run the pipeline.") else
    no(paste0("The pipeline cannot run yet: missing ", paste(needed_missing, collapse = ", "), "."), fix)

  quarto_bin <- .find_quarto()
  if (nzchar(quarto_bin)) {
    ok(paste0("You can render reports (Quarto ", tryCatch(as.character(quarto::quarto_version()), error = function(e) "found"),
              ", ", quarto_bin, ")."))
  } else {
    no("Reports cannot be rendered: Quarto was not found.",
       "install Quarto from https://quarto.org/docs/get-started/, or use RStudio or Positron, which include it.")
  }

  cat("\nOriginal methods (the marmots would be happier if you used and cited these; their own R versions are fall-backs, not validated and not supported):\n")
  if (has("Rphenograph")) ok("Rphenograph, the original PhenoGraph in C++.") else
    maybe("Rphenograph is not installed; MARMOT's Mphenograph will stand in.", fix)
  py <- tryCatch(marmot_python_status(), error = function(e) list(available = FALSE))
  if (isTRUE(py$available)) ok(paste0("PARC and PaCMAP, the original Python versions (", py$python_path, ").")) else
    maybe("The Python PARC and PaCMAP are not set up; MARMOT's Mparc and Mpacmap will stand in.",
          "MARMOT::install_marmot_python()")

  cat("\nOptional features:\n")
  feature <- function(name, pkgs) {
    if (has(pkgs)) ok(name) else maybe(paste0(name, ": missing ", paste(setdiff(pkgs, results$package[results$status == "installed"]), collapse = ", "), "."), fix)
  }
  feature("Quality control (FlowAI, PeacoQC)", c("flowAI", "PeacoQC"))
  feature("Batch correction (Harmony)", "harmony")
  feature("scGate gating", c("scGate", "UCell", "Seurat"))
  cat("\n")
  invisible(results)
}
