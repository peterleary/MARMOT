# Builds the Explorer's built-in example: MARMOT run on the public PBMC data
# that ships with CATALYST (data(PBMC_fs)), from Bodenmiller et al. (2012),
# Multiplexed mass cytometry profiling of cellular states perturbed by
# small-molecule regulators, Nature Biotechnology 30(9):858-67.
# Eight samples: four patients, each unstimulated (Ref) and after BCR/FcR
# crosslinking (BCRXL); 10 surface and 14 signalling markers. It is mass
# cytometry, so the cofactor is 5.
#
# Run from the repository root with MARMOT installed:
#   Rscript inst/examples/make_example.R [output folder]
# then copy <output>/Results_Files_*/R_files into inst/examples/R_files, and
# <output>/Results_Files_*/Excel_Files/topMarkerTable.xlsx into inst/examples/Excel_Files.

args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1]] else file.path(tempdir(), "marmot_example")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({ library(CATALYST); library(flowCore) })
data(PBMC_fs, PBMC_panel, PBMC_md)

# FCS files that name each channel by its antigen, as gated exports do
for (i in seq_along(PBMC_fs)) {
  ff <- PBMC_fs[[i]]
  p <- parameters(ff)
  p$desc <- PBMC_panel$antigen[match(p$name, PBMC_panel$fcs_colname)]
  parameters(ff) <- p
  write.FCS(ff, file.path(out, PBMC_md$file_name[i]))
}

# The metadata sheet, from MARMOT's own template
meta <- file.path(out, "MARMOT_Metadata.xlsx")
file.copy(system.file("pipeline", "MARMOT_Metadata.xlsx", package = "MARMOT"), meta, overwrite = TRUE)
wb <- openxlsx::loadWorkbook(meta)

ps <- openxlsx::read.xlsx(wb, sheet = "Pipeline Settings")
set <- function(var, value) {
  row <- match(var, ps$Variable) + 1L
  if (is.na(value)) openxlsx::deleteData(wb, "Pipeline Settings", cols = 2, rows = row, gridExpand = TRUE)
  else openxlsx::writeData(wb, "Pipeline Settings", value, startCol = 2, startRow = row, colNames = FALSE)
}
set("downsampleTo", NA)            # every cell (about 5,500 per sample)
set("runQC", "None")
set("markersToDimRedBy", "type")
set("kValuesIWant", "10 20 30")
set("knn", "20")
set("nCores", "4")

sd_names <- names(openxlsx::read.xlsx(wb, sheet = "Study Data", check.names = FALSE, sep.names = " "))
n <- nrow(PBMC_panel)
study <- data.frame(matrix(NA_character_, nrow = n, ncol = length(sd_names), dimnames = list(NULL, sd_names)),
                    check.names = FALSE)
study$`Conditions Order`[1:2] <- c("Ref", "BCRXL")
study$`Conditions To Test`[1] <- "BCRXL over Ref"
study$`Markers to include` <- PBMC_panel$antigen
study$`Marker Type` <- PBMC_panel$marker_class
study$`Cofactors for markers to use` <- 5
study$`Marker Pairs`[1:2] <- c("B cells: CD20 HLA-DR", "T cells: CD3 CD4")
openxlsx::deleteData(wb, "Study Data", cols = seq_along(sd_names), rows = 2:200, gridExpand = TRUE)
openxlsx::writeData(wb, "Study Data", study, startRow = 2, colNames = FALSE)

fd_names <- names(openxlsx::read.xlsx(wb, sheet = "File Data"))
openxlsx::deleteData(wb, "File Data", cols = seq_len(max(4, length(fd_names))), rows = 1:200, gridExpand = TRUE)
openxlsx::writeData(wb, "File Data", PBMC_md[, c("file_name", "sample_id", "condition", "patient_id")])
openxlsx::saveWorkbook(wb, meta, overwrite = TRUE)

MARMOT::marmot(metadata = meta, name = "PBMC_example", render = TRUE)
cat("Results in:", list.dirs(out, recursive = FALSE)[grepl("Results_Files_", list.dirs(out, recursive = FALSE))], "\n")
