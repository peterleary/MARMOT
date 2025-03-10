devtools::install_github("peterleary/MARMOT")
library(MARMOT)
metadata <- "~/Desktop/FGCZ/MARMOT/MARMOT_Metadata.xlsx"
metadata <- "~/Desktop/IMCR/MARMOT/For_Submission/MARMOT_Paper/FC_Metadata.xlsx"
marmot(metadata, name = "Test", render = TRUE)
results <- "~/Desktop/IMCR/MARMOT/For_Submission/MARMOT_Paper/Results_Files_2024-10-29_11.19.25/R_files_og/"
shinyMarmot(marmot_output = results)
