devtools::install_github("peterleary/MARMOT")
library(MARMOT)
getMetadata("~/Desktop/IMCR/MARMOT/For_Submission/MARMOT_Paper")
metadata <- "~/Desktop/IMCR/MARMOT/For_Submission/MARMOT_Paper/MARMOT_Metadata.xlsx"
marmot(metadata, name = "Test", render = TRUE)
# results <- "~/Desktop/IMCR/MARMOT/For_Submission/MARMOT_Paper/Results_Files_2024-10-29_11.19.25/R_files_og/"
results <- "~/Desktop/IMCR/MARMOT/For_Submission/MARMOT_Paper/Results_Files_2025-03-31_17.16.12/R_files/"
shinyMarmot(marmot_output = results)
