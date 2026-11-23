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
  
  # Settings that older versions of MARMOT had and this one no longer uses.
  # A sheet made for an older version still works; its extra rows are set aside.
  retired <- c("runInParallel", "themeToUse", "condaDir", "parcScriptDir", "drCellCount")
  retired_in_sheet <- intersect(params_df$Variable, retired)
  if (length(retired_in_sheet) > 0) {
    message("These settings are no longer used by MARMOT and were ignored: ",
            paste(retired_in_sheet, collapse = ", "),
            ". You can delete those rows from your Pipeline Settings sheet.")
    params_df <- params_df[!params_df$Variable %in% retired, , drop = FALSE]
  }

  # A setting whose row is missing altogether takes MARMOT's default. One that
  # is in the sheet but left blank is more likely a slip, so stop and say so.
  cantBeBlank <- c(
    "clusteringMethodToUse", "markersToClusterBy", "kValuesIWant", "knn",
    "dimRedMethodToUse", "markersToDimRedBy", "runQC", "useQC", "gimmePDFs",
    "quantileNormaliseAll", "nCores", "ramPerCore", "viridisColour", "figureDPI"
  )
  blank <- cantBeBlank[vapply(cantBeBlank, function(p) {
    value <- params_df$Setting[which(params_df$Variable == p)]
    length(value) > 0 && all(is.na(value))
  }, logical(1))]
  if (length(blank) > 0) {
    stop(paste(blank, collapse = ", "), " is blank! Please enter a value in the Excel Metadata file.")
  }
  
  # Settings where a blank cell means "none" (NULL), not "use MARMOT's default":
  # a blank downsampleTo is every cell, not the default cap.
  nullable_vars <- c("downsampleTo", "RDataFolder", "excludeTheseSamples", "batchColumn")
  nullable_nas <- params_df$Variable[params_df$Variable %in% nullable_vars &
                                       is.na(params_df$Setting)]

  # Keep every row that names a setting and gives it a value. (A blank Info
  # cell must not drop the setting: it would silently fall back to the default.)
  params_df <- params_df[!is.na(params_df$Variable) & !is.na(params_df$Setting), , drop = FALSE]

  # Get the list of options chosen
  params_list <- as.list(params_df[, 2]) |> setNames(params_df$Variable)

  # Tidy up the params
  if ("kValuesIWant" %in% names(params_list)) {
    kValues <- strsplit(as.character(params_list$kValuesIWant), "[ ,]+") |> unlist()
    params_list$kValuesIWant <- as.numeric(kValues[nzchar(kValues)])
  }
  # Sample IDs to leave out, separated by commas
  if ("excludeTheseSamples" %in% names(params_list)) {
    params_list$excludeTheseSamples <- trimws(strsplit(as.character(params_list$excludeTheseSamples), ",")[[1]])
  }
  for (f in c("downsampleTo", "knn", "nCores", "ramPerCore", "figureDPI")) {
    if (f %in% names(params_list)) {
      params_list[[f]] <- as.numeric(params_list[[f]])
    }
  }
  for (f in c("useQC", "gimmePDFs", "greyscalePlots", "quantileNormaliseAll", "runScGate")) {
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
  # Text values are written into the QMD as R strings, so a backslash or a
  # quote in one (a Windows path, say) has to be escaped to survive.
  as_r_string <- function(x) {
    x <- gsub("\\", "\\\\", x, fixed = TRUE)
    gsub("'", "\\'", x, fixed = TRUE)
  }
  not_a_setting <- character(0)
  for (var_name in names(params_list)) {
    pattern <- paste0("^", var_name, "\\ <-\\ \\.*.*")
    if (!any(grepl(pattern, rmd_content))) {
      not_a_setting <- c(not_a_setting, var_name)
      next
    }
    
    if (length(params_list[[var_name]]) == 1) {
      if (is.numeric(params_list[[var_name]]) | is.logical(params_list[[var_name]])) {
        replacement <- paste0(var_name, " <- ", paste(params_list[[var_name]]))
      } else if (is.character(params_list[[var_name]])) {
        replacement <- paste0(var_name, " <- '", as_r_string(paste(params_list[[var_name]])), "'")
      }
    } else {
      if (is.numeric(params_list[[var_name]]) | is.logical(params_list[[var_name]])) {
        replacement <- paste0(var_name, " <- c(", paste(params_list[[var_name]], collapse = ","), ")")
      } else if (is.character(params_list[[var_name]])) {
        replacement <- paste0(var_name, " <- c('", paste(as_r_string(params_list[[var_name]]), collapse = "','"), "')")
      }
    }
    # Replace whole lines by position: gsub() would read backslashes in the
    # replacement as its own escapes and strip them.
    rmd_content[grepl(pattern, rmd_content)] <- replacement
  }
  # Blank cells that mean "none": write NULL over whatever the default is
  for (v in nullable_nas) {
    rmd_content[grepl(paste0("^", v, "\\ <-\\ "), rmd_content)] <- paste0(v, " <- NULL")
  }
  if (length(not_a_setting) > 0) {
    warning("The marmots don't know these settings, so they were ignored: ",
            paste(not_a_setting, collapse = ", "),
            ". Please check the spelling in your Pipeline Settings sheet.", call. = FALSE)
  }
  
  # Point QMD at the installed header so we don't copy 329KB into the results folder
  header_src <- system.file("pipeline", "marmot_header.html", package = "MARMOT")
  if (nzchar(header_src)) {
    rmd_content <- gsub("- marmot_header.html", paste0("- ", header_src), rmd_content, fixed = TRUE)
  }
  # ... and at the installed toolbar and stylesheet
  toolbar_src <- system.file("pipeline", "marmot-toolbar.html", package = "MARMOT")
  if (nzchar(toolbar_src)) {
    rmd_content <- gsub("include-after-body: marmot-toolbar.html", paste0("include-after-body: ", toolbar_src), rmd_content, fixed = TRUE)
  }
  scss_src <- system.file("pipeline", "marmot.scss", package = "MARMOT")
  if (nzchar(scss_src)) {
    rmd_content <- gsub(", marmot.scss]", paste0(", ", scss_src, "]"), rmd_content, fixed = TRUE)
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
    # Quarto on the search path, or the copy inside RStudio or Positron
    quarto_bin <- .find_quarto()
    if (nzchar(quarto_bin) && !nzchar(Sys.which("quarto"))) {
      old_quarto_path <- Sys.getenv("QUARTO_PATH", unset = NA)
      Sys.setenv(QUARTO_PATH = quarto_bin)
      on.exit(if (is.na(old_quarto_path)) Sys.unsetenv("QUARTO_PATH") else Sys.setenv(QUARTO_PATH = old_quarto_path),
              add = TRUE)
    }
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
