# Makes tests/testthat/fixtures/format2: a small, made-up results folder in
# results format 2, with the values it should load back as.
#
# This folder stands for every results folder users have made with MARMOT 2.
# Do NOT regenerate it when the saving code changes: a later MARMOT 2.x must
# still read this one as it is. Make a new folder beside it only when the
# format number goes up.
#
# Run from the repository root:
#   Rscript -e 'devtools::load_all(); source("tests/testthat/fixtures/make_format2_fixture.R")'

source("tests/testthat/setup.R")
out <- "tests/testthat/fixtures/format2"
unlink(out, recursive = TRUE)
dir.create(file.path(out, "R_files"), recursive = TRUE)

set.seed(2)
sce <- make_mock_sce(n_cells = 60, n_markers = 4, n_samples = 2)
n <- ncol(sce)

# Assays as the pipeline leaves them: three names for one matrix, two derived
main <- SummarizedExperiment::assay(sce, "exprsTransformed")
SummarizedExperiment::assays(sce) <- list(
  counts = main, exprs = main, exprsTransformed = main,
  exprsQuantNorm = SummarizedExperiment::assay(sce, "exprsQuantNorm"), norm = main / 2)
SingleCellExperiment::reducedDims(sce) <- list(
  PCA = matrix(rnorm(n * 3), ncol = 3), UMAP = matrix(rnorm(n * 2), ncol = 2), TSNE = matrix(rnorm(n * 2), ncol = 2))
# QC and downsampling leave gaps in the event numbers
SingleCellExperiment::int_colData(sce)$event_number <- as.integer(c(seq(2, 60, by = 2), seq(1, 90, by = 3)))

tab <- function(v) data.frame(cluster_id = c("c1", "c2"), logFC = c(v, -v), p_adj = c(0.01, 0.5), stringsAsFactors = FALSE)
env <- new.env(parent = emptyenv())
env$sce <- sce
env$md <- S4Vectors::metadata(sce)$experiment_info
env$smd <- data.frame(study = "made-up", stringsAsFactors = FALSE)
env$daList <- list("B+ over A" = tab(1), "B- over A" = tab(2))
env$dsList <- list("B+ over A" = tab(3), "B- over A" = tab(4))
env$selectedClustersList <- list("Contrast B+ over A: Up" = c("c1", "c3"), "Contrast B+ over A: Down" = character(0))
env$sourceFiles <- data.frame(file_name = c("Sample1.fcs", "Sample2.fcs"),
                              md5 = c("0123456789abcdef0123456789abcdef", "fedcba9876543210fedcba9876543210"),
                              events = c(61L, 90L), stringsAsFactors = FALSE)
env$clusteringMethodToUse <- "FlowSOM"
env$dimRedMethodToUse <- "UMAP"
env$kValuesIWant <- c(10, 20)
suppressWarnings(save_h5ad_data(file.path(out, "R_files"), envir = env))

plain <- function(m) unname(as.matrix(m))
expected <- list(
  assays = lapply(as.list(SummarizedExperiment::assays(sce)), plain),
  markers = rownames(sce),
  embeddings = lapply(as.list(SingleCellExperiment::reducedDims(sce)), plain),
  event_numbers = event_numbers(sce),
  sample_id = as.character(sce$sample_id),
  condition = as.character(sce$condition),
  cluster_id = as.character(sce$cluster_id),
  daList = env$daList, dsList = env$dsList,
  selected_up = c("c1", "c3"),
  sourceFiles = env$sourceFiles,
  kValuesIWant = c(10, 20)
)
saveRDS(expected, file.path(out, "expected.rds"), version = 2)
