devtools::install_github("peterleary/MARMOT")
library(shinyMarmot)
metadata <- "~/Desktop/IMCR/MARMOT/For_Submission/MARMOT_Paper/FC_Metadata.xlsx"
metadata <- "MARMOT_Metadata.xlsx"
marmot(metadata, name = "Test", render = FALSE)
results <- "~/Desktop/IMCR/MARMOT/For_Submission/MARMOT_Paper/Results_Files_2024-10-29_11.19.25/R_files_og/"
shinyMarmot(marmot_output = results)
