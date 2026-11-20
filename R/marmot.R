#' @title marmot
#' @description The main MARMOT pipeline. Use to generate a customised MARMOT pipeline script based on the edited metadata file, and render if required.
#' @param metadata Path to the MARMOT metadata Excel file (.xlsx)
#' @param name Name for this analysis run (used in output filenames)
#' @param render Logical; if TRUE, render the pipeline report (default FALSE)
#' @return A results folder in the directory of the metadata containing an HTML report, and a folder with all resulting PDFs, Excel files, and R data files.
#' @author Peter Leary
#' @importFrom stats na.omit setNames
#' @export
#' @examples
#' \dontrun{
#' marmot(metadata = "~/Desktop/Flow_Data/MARMOT_metadata.xlsx", name = "Study Name", render = FALSE)
#' }
marmot <- function(metadata = NULL, name = "Title", render = FALSE) {
  if (is.null(metadata)) {
    stop("Oops! You left the metadata argument empty. Please tell me where the Excel Metadata file lives!")
  }
  
  # if the user supplied a relative path, fullpathify it 
  make_absolute_path <- function(path) {
    if (grepl("^\\~|^\\/|^[A-Za-z]\\:/", path)) return(path)
    return(normalizePath(file.path(getwd(), path), winslash = "/", mustWork = FALSE))
  }
  metadata <- make_absolute_path(metadata)
  
  # Get the directory name
  fp <- normalizePath(dirname(metadata), mustWork = TRUE, winslash = "/")
  md_fp <- file.path(fp, basename(metadata))

  # Create results directory early so QMD + header + HTML all live there
  timeRun <- format(Sys.time(), "%Y-%m-%d_%H.%M.%S")
  resultsDir <- file.path(fp, paste0("Results_Files_", timeRun))
  dir.create(resultsDir, showWarnings = FALSE, recursive = TRUE)
  
  # Read Metadata Excel file
  if(!any(grepl("pipeline settings", openxlsx::getSheetNames(metadata), ignore.case = TRUE))) {
    stop("Oops! The marmots can't find a 'Pipeline Settings' tab in your Excel Metadata file. Please run addMetadataToFCSFolder() again.")
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
      stop(p, " is blank! Please enter a value in the Excel Metadata file.")
    }
  })
  
  # Variables where NA in the Excel means "set to NULL" (not "use QMD default")
  nullable_vars <- c("downsampleTo", "RDataFolder", "excludeTheseSamples")
  nullable_nas <- params_df$Variable[params_df$Variable %in% nullable_vars &
                                       is.na(params_df$Setting)]

  params_df <- na.omit(params_df)

  # Get the list of options chosen
  params_list <- as.list(params_df[, 2]) |> setNames(params_df$Variable)

  # Explicitly set nullable variables to NULL when blank in Excel
  for (v in nullable_nas) params_list[[v]] <- NULL
  
  # Tidy up the params
  params_list$kValuesIWant <- strsplit(params_list$kValuesIWant, "\\ |\\,|\\,\\ ") |> unlist() |> as.numeric()
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
  params_list[["resultsDir"]] <- resultsDir
  
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
  
  # Point QMD at the installed header so we don't copy 329KB into the results folder
  header_src <- system.file("pipeline", "marmot_header.html", package = "MARMOT")
  if (nzchar(header_src)) {
    rmd_content <- gsub("- marmot_header.html", paste0("- ", header_src), rmd_content, fixed = TRUE)
  }

  # Copy metadata into the results folder for reproducibility
  file.copy(md_fp, file.path(resultsDir, basename(md_fp)), overwrite = TRUE)

  output_qmd <- file.path(resultsDir, paste0("MARMOT_Pipeline_", name, ".qmd"))
  writeLines(rmd_content, output_qmd)

  message("\nGenerated a modified copy of the MARMOT script to the folder. \n")
  if (!render) {
    message("\nYou chose not to render the HTML report. You can either render it yourself in RStudio, or run this function again with `render = TRUE`.\n")
  }
  if (render) {
    quarto_bin <- Sys.which("quarto")
    if (!nzchar(quarto_bin)) {
      stop(
        "Quarto is required to render the HTML report but was not found on your system.\n",
        "Install it from: https://quarto.org/docs/get-started/\n",
        "Then restart R and try again. You can verify with MARMOT::check_setup().\n",
        "Your pipeline script has been saved to:\n  ", output_qmd, "\n",
        "You can render it manually once Quarto is installed.",
        call. = FALSE
      )
    }
    message("Now rendering the HTML report. This can take some time...")
    # Condition and marker names may contain Greek letters, superscripts and
    # the like. Under a plain "C" language setting (the norm in Docker and in
    # scripts) R writes those into the report as <U+03B1>. Switch only the
    # character type to UTF-8 for the render: sorting order, and so the
    # results, stay exactly as they are.
    if (!isTRUE(l10n_info()[["UTF-8"]]) && !nzchar(Sys.getenv("LC_ALL"))) {
      utf8_ctype <- .find_utf8_ctype()
      if (nzchar(utf8_ctype)) {
        old_ctype <- Sys.getenv("LC_CTYPE", unset = NA)
        Sys.setenv(LC_CTYPE = utf8_ctype)
        on.exit(if (is.na(old_ctype)) Sys.unsetenv("LC_CTYPE") else Sys.setenv(LC_CTYPE = old_ctype),
                add = TRUE)
      }
    }
    # FlowAI writes its own QC reports with rmarkdown, which needs pandoc.
    # Outside RStudio and Positron there is often none on the search path, so
    # lend it the one that ships with Quarto for the length of the render.
    if (!nzchar(Sys.getenv("RSTUDIO_PANDOC")) && !nzchar(Sys.which("pandoc"))) {
      pandoc_dir <- .find_quarto_pandoc(quarto_bin)
      if (nzchar(pandoc_dir)) {
        Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
        on.exit(Sys.unsetenv("RSTUDIO_PANDOC"), add = TRUE)
      }
    }
    # Force --embed-resources on the command line as well as in the YAML.
    # Belt-and-braces: on some Quarto versions (notably 1.9.x on Windows), the
    # YAML `embed-resources: true` is silently ignored for certain htmlwidget
    # sidecars, producing a split report with a `*_files/` directory that
    # breaks when the .html is moved on its own.
    quarto::quarto_render(
      input       = output_qmd,
      output_file = paste0("MARMOT_Pipeline_", name, ".html"),
      quarto_args = c("--embed-resources")
    )
    message("Finished rendering! Hopefully the marmots did a good job, and the data is now all ready.\n")
    unlink(file.path(resultsDir, "Rplots.pdf"))
  }
  
}

# Find the folder holding the pandoc that ships with Quarto.
# Quarto keeps it in <quarto>/bin/tools/, or in a folder per processor type
# below that. Returns "" if none is found.
.find_quarto_pandoc <- function(quarto_bin) {
  if (!nzchar(quarto_bin) || !file.exists(quarto_bin)) return("")
  tools_dir <- file.path(dirname(normalizePath(quarto_bin)), "tools")
  found <- list.files(tools_dir, pattern = "^pandoc(\\.exe)?$",
                      recursive = TRUE, full.names = TRUE)
  if (length(found) == 0) return("")
  machine <- Sys.info()[["machine"]]
  if (machine == "arm64") machine <- "aarch64"
  same_machine <- found[basename(dirname(found)) == machine]
  dirname(if (length(same_machine) > 0) same_machine[1] else found[1])
}

# Find a UTF-8 character type this machine offers. Returns "" if there is none.
.find_utf8_ctype <- function(candidates = c("en_US.UTF-8", "C.UTF-8", "UTF-8")) {
  current <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", current)), add = TRUE)
  for (candidate in candidates) {
    set <- suppressWarnings(Sys.setlocale("LC_CTYPE", candidate))
    if (nzchar(set) && isTRUE(l10n_info()[["UTF-8"]])) return(candidate)
  }
  ""
}
