#' start main marmot pipeline
#' @export
#' @import Rcpp
#' @examples
#' \dontrun{
#' marmot(metadata = "FC_metadata.xlsx", name = "Study Name", render = FALSE)
#' }
marmot <- function(metadata = NULL, name = "Title", render = FALSE) {
  suppressPackageStartupMessages({require(tidyverse)})
  if (is.null(metadata)) {
    stop("Oops! You left the metadata argument empty. Please tell me where the Excel file lives!")
  }
  
  # Get the directory name
  fp <- dirname(metadata)
  
  # Read Excel file
  params_df <- openxlsx::read.xlsx(metadata, sheet = "Pipeline Settings")
  params_df <- na.omit(params_df)
  
  # Get the list of options chosen
  params_list <- as.list(params_df[, 2]) |> setNames(params_df$Variable) 
  
  # Tidy up the params
  params_list$kValuesIWant <- strsplit(params_list$kValuesIWant, " ") %>% unlist %>% as.numeric
  params_list$removeFromQC <- strsplit(params_list$removeFromQC, " ") %>% unlist
  for (f in c("downsampleTo", "knn", "drCellCount", "nCores", "ramPerCore")) {
    if (f %in% names(params_list)) {
      params_list[[f]] <- as.numeric(params_list[[f]])
    }
  }
  for (f in c("runQC", "useQC", "gimmePDFs", "quantileNormaliseAll", "runInParallel")) {
    if (f %in% names(params_list)) {
      params_list[[f]] <- as.logical(params_list[[f]])
    }
  }
  params_list[["fp"]] <- fp
  
  # Import the template marmot file 
  rmd_content <- readLines("MARMOT_Pipeline.Rmd")
  
  # Replace the markdown title 
  rmd_content <- gsub("{{PIPELINE_NAME}}", name, rmd_content, fixed = TRUE)
  
  # Remap the variables in the template RMD
  var_name <- "fp"
  for (var_name in names(params_list)) {
    pattern <- paste0("^", var_name, "\\ <-\\ \\.*.*")
    
    if (length(params_list[[var_name]]) == 1) {
      if (is.numeric(params_list[[var_name]]) | is.logical(params_list[[var_name]])) {
        replacement <- paste0(var_name, " <- ", paste(params_list[[var_name]]))
      } else if (is.character(params_list[[var_name]])) {
        replacement <- paste0(var_name, " <- '", paste(params_list[[var_name]]), "'")
      }
    } else {
      if (is.numeric(params_list[[var_name]]) | is.logical(params_list[[var_name]])) {
        replacement <- paste0(var_name, " <- c(", paste(params_list[[var_name]], collapse = ","), ")")
      } else if (is.character(params_list[[var_name]])) {
        replacement <- paste0(var_name, " <- c('", paste(params_list[[var_name]], collapse = "','"), "')")
      }
    }
    rmd_content <- gsub(pattern, replacement, rmd_content)
  }
  
  output_rmd <- paste0(fp, "/MARMOT_Pipeline_", name, ".Rmd")
  writeLines(rmd_content, output_rmd)
  message("\nGenerated a modified copy of the MARMOT script to the folder. \n")
  if (!render) {
    message("\nYou chose not to render the HTML report. You can either Knit it yourself in RStudio, or run this function again with `render = TRUE`.\n")
  }
  if (render) {
    message("Now rendering the HTML report. This can take some time...")
    invisible(rmarkdown::render(output_rmd, output_format = "html_document", clean = TRUE))
    message("Finished rendering! Hopefully the marmots did a good job, and the data is now all ready.\n")
    unlink(file.path(fp, "Rplots.pdf"))
  }
  
}
