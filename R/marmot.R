#' @title marmot
#' @description The main MARMOT pipeline. Use to generate a customised MARMOT pipeline script based on the edited metadata file, and render if required.
#' @return A results folder in the directory of the metadata containing an HTML report, and a folder with all resulting PDFs, Excel files, and R data files.
#' @author Peter Leary
#' @export
#' @import Rcpp
#' @importFrom Rcpp evalCpp
#' @examples
#' \dontrun{
#' marmot(metadata = "~/Desktop/Flow_Data/MARMOT_metadata.xlsx", name = "Study Name", render = FALSE)
#' }
marmot <- function(metadata = NULL, name = "Title", render = FALSE) {
  suppressPackageStartupMessages({require(tidyverse)})
  if (is.null(metadata)) {
    stop("Oops! You left the metadata argument empty. Please tell me where the Excel Metadata file lives!")
  }
  
  # if the user supplied a relative path, fullpathify it 
  make_absolute_path <- function(path) {
    if (grepl("^\\~|^\\/|^C\\:/", path)) return(path)
    return(normalizePath(file.path(getwd(), path), winslash = "/", mustWork = FALSE))
  }
  metadata <- make_absolute_path(metadata)
  
  # Get the directory name
  fp <- dirname(metadata)
  md_fp <- basename(metadata)
  
  # Read Metadata Excel file
  if(!any(grepl("pipeline settings", openxlsx::getSheetNames(metadata), ignore.case = T))) {
    stop("Oops! The marmots can't find a 'Pipeline Settings' tab in your Excel Metadata file. Please run the getMetadata function again.")
  }
  params_df <- openxlsx::read.xlsx(metadata, sheet = "Pipeline Settings")
  
  cantBeBlank <- c(
    "clusteringMethodToUse", "markersToClusterBy", "kValuesIWant", "knn", 
    "dimRedMethodToUse", "markersToDimRedBy", "runQC", "useQC", "gimmePDFs",
    "quantileNormaliseAll", "runInParallel", "nCores", "ramPerCore", "themeToUse",
    "viridisColour"
  )
  lapply(cantBeBlank, function(p) {
    if (is.na(params_df$Setting[params_df$Variable == p])) {
      stop(p , " is blank! Please enter a value in the Excel Metadata file.")
    }
  })
  
  params_df <- na.omit(params_df)
  
  # Get the list of options chosen
  params_list <- as.list(params_df[, 2]) |> setNames(params_df$Variable) 
  
  # Tidy up the params
  params_list$kValuesIWant <- strsplit(params_list$kValuesIWant, "\\ |\\,|\\,\\ ") %>% unlist %>% as.numeric
  for (f in c("downsampleTo", "knn", "drCellCount", "nCores", "ramPerCore")) {
    if (f %in% names(params_list)) {
      params_list[[f]] <- as.numeric(params_list[[f]])
    }
  }
  for (f in c("useQC", "gimmePDFs", "quantileNormaliseAll", "runInParallel", "runScGate")) {
    if (f %in% names(params_list)) {
      params_list[[f]] <- as.logical(params_list[[f]])
    }
  }
  params_list[["fp"]] <- fp
  params_list[["md_fp"]] <- md_fp
  
  # Import the template marmot file
  rmd_content <- readLines(system.file("pipeline", "MARMOT_Pipeline.qmd", package = "MARMOT"))
  
  # Replace the markdown title 
  rmd_content <- gsub("{{PIPELINE_NAME}}", name, rmd_content, fixed = TRUE)
  
  # Remap the variables in the template
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
  
  output_qmd <- paste0(fp, "/MARMOT_Pipeline_", name, ".qmd")
  writeLines(rmd_content, output_qmd)
  Sys.sleep(0.2)
  message("\nGenerated a modified copy of the MARMOT script to the folder. \n")
  if (!render) {
    message("\nYou chose not to render the HTML report. You can either render it yourself in RStudio, or run this function again with `render = TRUE`.\n")
  }
  if (render) {
    message("Now rendering the HTML report. This can take some time...")
    output_html <- paste0(fp, "/MARMOT_Pipeline_", name, ".html")
    quarto::quarto_render(input = output_qmd, output_file = basename(output_html))
    message("Finished rendering! Hopefully the marmots did a good job, and the data is now all ready.\n")
    unlink(file.path(fp, "Rplots.pdf"))
  }
  
}