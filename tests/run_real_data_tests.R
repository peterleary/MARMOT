#!/usr/bin/env Rscript
# ============================================================================
# MARMOT Real-Data Pipeline Test Suite
# ============================================================================
# Downsamples real FCS files ONCE, then runs every pipeline permutation.
# NOT for the repo — local testing only.
#
# Usage:
#   Rscript tests/run_real_data_tests.R
#   Rscript tests/run_real_data_tests.R --skip-downsample   # reuse existing
#
# Requires: installed MARMOT package, quarto, real data at SOURCE_DIR below
# ============================================================================

library(MARMOT)
library(flowCore)

# ── Config ──────────────────────────────────────────────────────────────────
SOURCE_DIR   <- "~/Desktop/marmot_test_data/FCS_folder"
TEST_BASE    <- file.path(tempdir(), "marmot_real_tests")
CELLS_SMALL  <- 500    # too few for QC — tests skip paths
CELLS_MEDIUM <- 2500   # enough for FlowAI, borderline PeacoQC
CELLS_LARGE  <- 6000   # enough for PeacoQC
N_SAMPLES    <- 8      # use 8 samples (2 per condition) — DA/DS needs replication
METADATA     <- file.path(SOURCE_DIR, "My_MARMOT_Analysis_metadata.xlsx")

skip_downsample <- "--skip-downsample" %in% commandArgs(trailingOnly = TRUE)

cat("═══════════════════════════════════════════════════\n")
cat("  MARMOT Real-Data Pipeline Test Suite\n")
cat("═══════════════════════════════════════════════════\n\n")

stopifnot(file.exists(METADATA))

# ── Step 1: Downsample real FCS files ────────────────────────────────────────
downsample_fcs <- function(source_dir, out_dir, n_cells, n_samples) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  md <- readxl::read_xlsx(file.path(source_dir, "My_MARMOT_Analysis_metadata.xlsx"),
                          sheet = "File Data")
  # Pick 2 samples per condition (minimum for DA/DS replication)
  sampled <- do.call(rbind, lapply(split(md, md$condition), function(x) {
    x[seq_len(min(2, nrow(x))), ]
  }))
  # Top up if needed
  if (nrow(sampled) < n_samples) {
    extra <- md[!md$file_name %in% sampled$file_name, ]
    sampled <- rbind(sampled, extra[seq_len(min(n_samples - nrow(sampled), nrow(extra))), ])
  }
  sampled <- sampled[seq_len(min(n_samples, nrow(sampled))), ]

  for (i in seq_len(nrow(sampled))) {
    src <- file.path(source_dir, sampled$file_name[i])
    if (!file.exists(src)) {
      cat("  SKIP (not found):", sampled$file_name[i], "\n")
      next
    }
    ff <- read.FCS(src, transformation = FALSE, truncate_max_range = FALSE)
    if (nrow(ff) > n_cells) {
      set.seed(42)
      ff <- ff[sort(sample(nrow(ff), n_cells)), ]
    }
    write.FCS(ff, file.path(out_dir, sampled$file_name[i]))
    cat(sprintf("  %s: %d → %d cells\n", sampled$file_name[i], nrow(ff), n_cells))
  }

  # Write metadata with only the selected samples
  smd <- readxl::read_xlsx(file.path(source_dir, "My_MARMOT_Analysis_metadata.xlsx"),
                           sheet = "Study Data")
  settings <- readxl::read_xlsx(file.path(source_dir, "My_MARMOT_Analysis_metadata.xlsx"),
                                sheet = "Pipeline Settings")

  md_out <- sampled

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Pipeline Settings")
  openxlsx::writeData(wb, "Pipeline Settings", settings)
  openxlsx::addWorksheet(wb, "File Data")
  openxlsx::writeData(wb, "File Data", md_out)
  openxlsx::addWorksheet(wb, "Study Data")
  openxlsx::writeData(wb, "Study Data", smd)
  meta_path <- file.path(out_dir, "My_MARMOT_Analysis_metadata.xlsx")
  openxlsx::saveWorkbook(wb, meta_path, overwrite = TRUE)

  list(dir = out_dir, metadata = meta_path, samples = sampled)
}

# Create three dataset sizes
datasets <- list()
if (!skip_downsample) {
  cat("Downsampling real FCS files...\n")
  for (size_name in c("small", "medium", "large")) {
    n <- switch(size_name, small = CELLS_SMALL, medium = CELLS_MEDIUM, large = CELLS_LARGE)
    out <- file.path(TEST_BASE, paste0("fcs_", size_name))
    cat(sprintf("\n[%s] %d cells/sample:\n", size_name, n))
    datasets[[size_name]] <- downsample_fcs(SOURCE_DIR, out, n, N_SAMPLES)
  }
  cat("\nDownsampling complete.\n\n")
} else {
  cat("Reusing existing downsampled data.\n\n")
  for (size_name in c("small", "medium", "large")) {
    out <- file.path(TEST_BASE, paste0("fcs_", size_name))
    datasets[[size_name]] <- list(
      dir = out,
      metadata = file.path(out, "My_MARMOT_Analysis_metadata.xlsx")
    )
  }
}

# ── Step 2: Define test permutations ─────────────────────────────────────────

# Helper: override pipeline settings in metadata
write_test_metadata <- function(base_metadata, out_path, overrides = list()) {
  settings <- readxl::read_xlsx(base_metadata, sheet = "Pipeline Settings")
  md <- readxl::read_xlsx(base_metadata, sheet = "File Data")
  smd <- readxl::read_xlsx(base_metadata, sheet = "Study Data")

  for (nm in names(overrides)) {
    idx <- which(settings$Variable == nm)
    if (length(idx) == 1) {
      settings$Setting[idx] <- as.character(overrides[[nm]])
    }
  }

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Pipeline Settings")
  openxlsx::writeData(wb, "Pipeline Settings", settings)
  openxlsx::addWorksheet(wb, "File Data")
  openxlsx::writeData(wb, "File Data", md)
  openxlsx::addWorksheet(wb, "Study Data")
  openxlsx::writeData(wb, "Study Data", smd)
  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  out_path
}

# Each test: name, dataset size, setting overrides
tests <- list(
  # ── Clustering methods ──────────────────────────────────────
  list(name = "FlowSOM_baseline",
       size = "medium",
       overrides = list(clusteringMethodToUse = "FlowSOM",
                        dimRedMethodToUse = "UMAP",
                        runQC = "None", useQC = "FALSE",
                        runScGate = "FALSE", downsampleTo = NA)),

  list(name = "FlowSOM_TSNE",
       size = "medium",
       overrides = list(clusteringMethodToUse = "FlowSOM",
                        dimRedMethodToUse = "TSNE",
                        runQC = "None", useQC = "FALSE",
                        runScGate = "FALSE", downsampleTo = NA)),

  list(name = "Rphenograph",
       size = "medium",
       overrides = list(clusteringMethodToUse = "Rphenograph",
                        dimRedMethodToUse = "UMAP",
                        runQC = "None", useQC = "FALSE",
                        runScGate = "FALSE", downsampleTo = NA)),

  list(name = "PARC",
       size = "medium",
       overrides = list(clusteringMethodToUse = "Mparc",
                        dimRedMethodToUse = "UMAP",
                        runQC = "None", useQC = "FALSE",
                        runScGate = "FALSE", downsampleTo = NA)),

  list(name = "PaCMAP",
       size = "medium",
       overrides = list(clusteringMethodToUse = "FlowSOM",
                        dimRedMethodToUse = "Mpacmap",
                        runQC = "None", useQC = "FALSE",
                        runScGate = "FALSE", downsampleTo = NA)),

  # ── QC paths ────────────────────────────────────────────────
  list(name = "FlowAI_noUse",
       size = "medium",
       overrides = list(runQC = "FlowAI", useQC = "FALSE",
                        clusteringMethodToUse = "FlowSOM",
                        dimRedMethodToUse = "UMAP",
                        runScGate = "FALSE", downsampleTo = NA)),

  list(name = "FlowAI_useQC",
       size = "medium",
       overrides = list(runQC = "FlowAI", useQC = "TRUE",
                        clusteringMethodToUse = "FlowSOM",
                        dimRedMethodToUse = "UMAP",
                        runScGate = "FALSE", downsampleTo = NA)),

  list(name = "PeacoQC_useQC",
       size = "large",
       overrides = list(runQC = "PeacoQC", useQC = "TRUE",
                        clusteringMethodToUse = "FlowSOM",
                        dimRedMethodToUse = "UMAP",
                        runScGate = "FALSE", downsampleTo = NA)),

  list(name = "PeacoQC_noUse",
       size = "large",
       overrides = list(runQC = "PeacoQC", useQC = "FALSE",
                        clusteringMethodToUse = "FlowSOM",
                        dimRedMethodToUse = "UMAP",
                        runScGate = "FALSE", downsampleTo = NA)),

  # ── QC skip paths (too few cells) ──────────────────────────
  list(name = "FlowAI_tooSmall",
       size = "small",
       overrides = list(runQC = "FlowAI", useQC = "TRUE",
                        clusteringMethodToUse = "FlowSOM",
                        dimRedMethodToUse = "UMAP",
                        runScGate = "FALSE", downsampleTo = NA)),

  list(name = "PeacoQC_tooSmall",
       size = "small",
       overrides = list(runQC = "PeacoQC", useQC = "TRUE",
                        clusteringMethodToUse = "FlowSOM",
                        dimRedMethodToUse = "UMAP",
                        runScGate = "FALSE", downsampleTo = NA)),

  # ── Data options ────────────────────────────────────────────
  list(name = "downsample",
       size = "medium",
       overrides = list(downsampleTo = "500",
                        clusteringMethodToUse = "FlowSOM",
                        dimRedMethodToUse = "UMAP",
                        runQC = "None", useQC = "FALSE",
                        runScGate = "FALSE")),

  list(name = "quantileNorm",
       size = "medium",
       overrides = list(quantileNormaliseAll = "TRUE",
                        clusteringMethodToUse = "FlowSOM",
                        dimRedMethodToUse = "UMAP",
                        runQC = "None", useQC = "FALSE",
                        runScGate = "FALSE", downsampleTo = NA)),

  list(name = "multiK",
       size = "medium",
       overrides = list(kValuesIWant = "10 20 30",
                        knn = "10",
                        clusteringMethodToUse = "FlowSOM",
                        dimRedMethodToUse = "UMAP",
                        runQC = "None", useQC = "FALSE",
                        runScGate = "FALSE", downsampleTo = NA)),

  # ── Output options ──────────────────────────────────────────
  list(name = "PDFs",
       size = "medium",
       overrides = list(gimmePDFs = "TRUE",
                        clusteringMethodToUse = "FlowSOM",
                        dimRedMethodToUse = "UMAP",
                        runQC = "None", useQC = "FALSE",
                        runScGate = "FALSE", downsampleTo = NA)),

  list(name = "greyscale",
       size = "medium",
       overrides = list(greyscalePlots = "TRUE",
                        clusteringMethodToUse = "FlowSOM",
                        dimRedMethodToUse = "UMAP",
                        runQC = "None", useQC = "FALSE",
                        runScGate = "FALSE", downsampleTo = NA)),

  # ── scGate ──────────────────────────────────────────────────
  list(name = "scGate",
       size = "medium",
       overrides = list(runScGate = "TRUE",
                        clusteringMethodToUse = "FlowSOM",
                        dimRedMethodToUse = "UMAP",
                        runQC = "None", useQC = "FALSE",
                        downsampleTo = NA)),

  # ── Kitchen sink ────────────────────────────────────────────
  list(name = "kitchen_sink",
       size = "large",
       overrides = list(clusteringMethodToUse = "FlowSOM",
                        dimRedMethodToUse = "UMAP",
                        runQC = "FlowAI", useQC = "TRUE",
                        runScGate = "TRUE",
                        quantileNormaliseAll = "TRUE",
                        downsampleTo = "1000",
                        kValuesIWant = "15 25",
                        knn = "15",
                        gimmePDFs = "FALSE"))
)

# ── Step 3: Run tests ────────────────────────────────────────────────────────

results <- data.frame(
  test = character(),
  status = character(),
  time_sec = numeric(),
  error = character(),
  stringsAsFactors = FALSE
)

cat(sprintf("Running %d pipeline permutations...\n\n", length(tests)))

for (tt in tests) {
  cat(sprintf("── [%s] (dataset: %s) ", tt$name, tt$size))

  ds <- datasets[[tt$size]]
  test_dir <- file.path(TEST_BASE, "runs", tt$name)
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)

  # Copy FCS files to test dir
  fcs_files <- list.files(ds$dir, pattern = "\\.fcs$", full.names = TRUE)
  file.copy(fcs_files, test_dir, overwrite = TRUE)

  # Write customised metadata
  meta_path <- file.path(test_dir, "test_metadata.xlsx")
  write_test_metadata(ds$metadata, meta_path, tt$overrides)

  t0 <- proc.time()
  err_msg <- ""
  status <- tryCatch({
    marmot(metadata = meta_path, name = tt$name, render = TRUE)
    "PASS"
  }, error = function(e) {
    err_msg <<- conditionMessage(e)
    "FAIL"
  })
  elapsed <- (proc.time() - t0)[["elapsed"]]

  if (status == "PASS") {
    cat(sprintf("✓ (%.0fs)\n", elapsed))
  } else {
    cat(sprintf("✗ FAIL (%.0fs)\n", elapsed))
    cat("    Error:", substr(err_msg, 1, 120), "\n")
  }

  results <- rbind(results, data.frame(
    test = tt$name, status = status,
    time_sec = round(elapsed, 1), error = err_msg,
    stringsAsFactors = FALSE
  ))

  # Clean up results dir to save disk (keep metadata for debugging)
  result_dirs <- list.dirs(test_dir, recursive = FALSE)
  result_dirs <- result_dirs[grepl("^Results_Files_", basename(result_dirs))]
  if (status == "PASS" && length(result_dirs) > 0) {
    unlink(result_dirs, recursive = TRUE)
  }
}

# ── Step 4: Summary ──────────────────────────────────────────────────────────

cat("\n═══════════════════════════════════════════════════\n")
cat("  RESULTS SUMMARY\n")
cat("═══════════════════════════════════════════════════\n\n")

n_pass <- sum(results$status == "PASS")
n_fail <- sum(results$status == "FAIL")
cat(sprintf("  %d / %d passed", n_pass, nrow(results)))
if (n_fail > 0) cat(sprintf("  (%d FAILED)", n_fail))
cat(sprintf("\n  Total time: %.0f seconds\n\n", sum(results$time_sec)))

print(results[, c("test", "status", "time_sec")], row.names = FALSE)

if (n_fail > 0) {
  cat("\n── FAILURES ──\n")
  fails <- results[results$status == "FAIL", ]
  for (i in seq_len(nrow(fails))) {
    cat(sprintf("\n  [%s]: %s\n", fails$test[i], fails$error[i]))
  }
}

cat("\nTest data at:", TEST_BASE, "\n")
