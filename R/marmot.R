#' start main marmot pipeline
#' @export
#' @import Rcpp
#' @examples
#' \dontrun{
#' marmot(metadatafile)
#' }
marmot <- function(metadata = NULL, name = "MARMOT Flow Cytometry Pipeline v1.0.0") {
  require(tidyverse)
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
  
  # Construct a regex pattern to find the variable assignment
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
  
  output_rmd <- paste0(fp, "/MARMOT_Pipeline_Modified.Rmd")
  writeLines(rmd_content, output_rmd)
  rmarkdown::render(output_rmd)
  
}
