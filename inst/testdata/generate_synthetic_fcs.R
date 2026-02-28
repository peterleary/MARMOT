#!/usr/bin/env Rscript
#' Generate synthetic FCS files + metadata for MARMOT pipeline testing
#'
#' Usage:
#'   Rscript inst/testdata/generate_synthetic_fcs.R [output_dir] [n_cells]
#'
#' Defaults:
#'   output_dir = inst/testdata/synthetic/
#'   n_cells    = 10000 per sample (12 samples total)
#'
#' Produces 12 FCS files (4 conditions x 3 replicates) with a 21-marker
#' immune panel and known differential abundance, plus MARMOT_metadata.xlsx.

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) >= 1) args[1] else "inst/testdata/synthetic"
n_cells    <- if (length(args) >= 2) as.integer(args[2]) else 10000L

# Source the generator from testthat setup
source("tests/testthat/setup.R")

cat("Generating synthetic FCS data:\n")
cat("  Output dir:", output_dir, "\n")
cat("  Cells/sample:", n_cells, "\n")
cat("  Samples: 12 (4 conditions x 3 replicates)\n\n")

# Generate into a temp directory first
tmp_dir <- make_realistic_pipeline_data(n_cells = n_cells)

# Copy to the target output directory
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
files <- list.files(tmp_dir, full.names = TRUE)
file.copy(files, output_dir, overwrite = TRUE)
unlink(tmp_dir, recursive = TRUE)

cat("Done. Files written to:", output_dir, "\n")
cat("Contents:\n")
cat(paste(" ", list.files(output_dir)), sep = "\n")
