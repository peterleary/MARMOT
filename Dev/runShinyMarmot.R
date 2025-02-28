devtools::install_github("peterleary/MARMOT")
library(shinyMarmot)
flowsom_tsne <- "~/Desktop/FGCZ/MARMOT/files/Results_Files_2025-02-06_14.42.22/R_files/"
flowsom_umap <- "~/Desktop/FGCZ/MARMOT/files/Results_Files_2025-02-07_12.16.10/R_files/"
paper <- "~/Desktop/IMCR/MARMOT/For_Submission/MARMOT_Paper/Results_Files_2024-10-29_11.19.25/R_files/"
shinyMarmot(marmot_output = paper)
