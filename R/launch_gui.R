#' @title launch_gui
#' @description Launch the MARMOT desktop GUI application. On first use, downloads
#'   the pre-built binary for your platform from GitHub Releases (~5 MB, one-time).
#'   Subsequent launches use the cached binary.
#' @param version Character. Release tag to download (default: "latest").
#' @param force_download Logical. If TRUE, re-download even if cached binary exists.
#' @return Invisible NULL. The GUI application is launched as a background process.
#' @author Peter Leary
#' @export
#' @examples
#' \dontrun{
#' launch_gui()
#' }
launch_gui <- function(version = "latest", force_download = FALSE) {

  cache_dir <- tools::R_user_dir("MARMOT", "data")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  # Determine platform and binary name
  sys_info <- Sys.info()
  os <- tolower(sys_info["sysname"])
  arch <- sys_info["machine"]

  if (os == "darwin") {
    binary_name <- "marmot-gui"
    if (arch == "arm64") {
      asset_pattern <- "marmot-gui-macos-arm64"
    } else {
      asset_pattern <- "marmot-gui-macos-x86_64"
    }
  } else if (os == "windows") {
    binary_name <- "marmot-gui.exe"
    asset_pattern <- "marmot-gui-windows"
  } else {
    binary_name <- "marmot-gui"
    asset_pattern <- "marmot-gui-linux"
  }

  binary_path <- file.path(cache_dir, binary_name)

  # Download if needed
  if (!file.exists(binary_path) || force_download) {
    message("Downloading MARMOT GUI binary (one-time, ~5 MB)...")

    if (version == "latest") {
      api_url <- "https://api.github.com/repos/peterleary/MARMOT/releases/latest"
    } else {
      api_url <- paste0("https://api.github.com/repos/peterleary/MARMOT/releases/tags/", version)
    }

    response <- tryCatch(
      jsonlite::fromJSON(api_url),
      error = function(e) {
        stop("Failed to fetch release info from GitHub: ", e$message,
             "\nPlease check your internet connection and try again.")
      }
    )

    assets <- response$assets
    match_idx <- grep(asset_pattern, assets$name)

    if (length(match_idx) == 0) {
      stop("No matching binary found for your platform (", asset_pattern, ").\n",
           "Available assets: ", paste(assets$name, collapse = ", "))
    }

    download_url <- assets$browser_download_url[match_idx[1]]
    tmp <- tempfile()
    utils::download.file(download_url, tmp, mode = "wb", quiet = FALSE)

    # If it's a zip/tar, extract; otherwise copy directly
    if (grepl("\\.zip$", download_url)) {
      utils::unzip(tmp, exdir = cache_dir)
    } else if (grepl("\\.tar\\.gz$", download_url)) {
      utils::untar(tmp, exdir = cache_dir)
    } else {
      file.copy(tmp, binary_path, overwrite = TRUE)
    }

    unlink(tmp)

    # Make executable on Unix
    if (os != "windows") {
      Sys.chmod(binary_path, mode = "0755")
    }

    message("Downloaded successfully to: ", binary_path)
  }

  if (!file.exists(binary_path)) {
    stop("Binary not found at: ", binary_path,
         "\nTry launch_gui(force_download = TRUE)")
  }

  message("Launching MARMOT GUI...")

  # Launch as background process
  if (os == "windows") {
    shell(paste0("start \"\" \"", binary_path, "\""), wait = FALSE)
  } else if (os == "darwin") {
    system2(binary_path, wait = FALSE, stdout = FALSE, stderr = FALSE)
  } else {
    system2(binary_path, wait = FALSE, stdout = FALSE, stderr = FALSE)
  }

  invisible(NULL)
}
