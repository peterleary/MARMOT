# The original PARC and PaCMAP, set up with pixi
#
# pixi builds the Python environment from conda-forge with ready-made
# packages, from a lock file shipped with MARMOT (inst/python/pixi.lock), so
# every user gets exactly the same versions and nothing is compiled. pixi is a
# single program: if it is not installed, MARMOT fetches it into its own data
# folder. No conda installation is needed. The environment lives in MARMOT's
# data folder too, so it is the same for every project.

.pixi_version <- "0.84.2"

# MARMOT's folder for the Python environment (and pixi, if MARMOT fetched it)
.marmot_python_dir <- function() file.path(tools::R_user_dir("MARMOT", "data"), "python")

# The environment's Python, whether or not it exists yet
.pixi_python <- function(dir = .marmot_python_dir()) {
  if (.Platform$OS.type == "windows") file.path(dir, ".pixi", "envs", "default", "python.exe")
  else file.path(dir, ".pixi", "envs", "default", "bin", "python")
}

# pixi: on the search path, or the copy MARMOT fetched; "" if neither
.find_pixi <- function(dir = .marmot_python_dir()) {
  on_path <- Sys.which("pixi")
  if (nzchar(on_path)) return(unname(on_path))
  own <- file.path(dir, "bin", if (.Platform$OS.type == "windows") "pixi.exe" else "pixi")
  if (file.exists(own)) own else ""
}

# Fetch pixi into MARMOT's folder (one file, a few tens of MB)
.download_pixi <- function(dir = .marmot_python_dir()) {
  machine <- tolower(Sys.info()[["machine"]])
  arm <- grepl("arm64|aarch64", machine)
  target <- switch(Sys.info()[["sysname"]],
    Darwin  = if (arm) "aarch64-apple-darwin.tar.gz" else "x86_64-apple-darwin.tar.gz",
    Linux   = if (arm) "aarch64-unknown-linux-musl.tar.gz" else "x86_64-unknown-linux-musl.tar.gz",
    Windows = "x86_64-pc-windows-msvc.zip",
    stop("pixi is not available for this system: ", Sys.info()[["sysname"]])
  )
  url <- sprintf("https://github.com/prefix-dev/pixi/releases/download/v%s/pixi-%s", .pixi_version, target)
  bin_dir <- file.path(dir, "bin")
  dir.create(bin_dir, recursive = TRUE, showWarnings = FALSE)
  archive <- file.path(tempdir(), basename(url))
  message("Fetching pixi ", .pixi_version, " (one file) from GitHub...")
  utils::download.file(url, archive, mode = "wb", quiet = TRUE)
  if (grepl("\\.zip$", archive)) utils::unzip(archive, exdir = bin_dir) else utils::untar(archive, exdir = bin_dir)
  unlink(archive)
  pixi <- .find_pixi(dir)
  if (!nzchar(pixi)) stop("pixi was downloaded but could not be found in ", bin_dir)
  if (.Platform$OS.type != "windows") Sys.chmod(pixi, "755")
  pixi
}

# Build (or repair) the environment from the shipped lock file
.install_pixi_env <- function(dir = .marmot_python_dir(), force = FALSE) {
  pixi <- .find_pixi(dir)
  if (!nzchar(pixi)) pixi <- .download_pixi(dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (f in c("pixi.toml", "pixi.lock")) {
    src <- system.file("python", f, package = "MARMOT")
    if (!nzchar(src)) stop("MARMOT's ", f, " is missing from the installation.")
    file.copy(src, file.path(dir, f), overwrite = TRUE)
  }
  if (force) unlink(file.path(dir, ".pixi"), recursive = TRUE)
  message("Building the Python environment for PARC and PaCMAP (ready-made packages, nothing compiled)...")
  out <- system2(pixi, c("install", "--frozen", "--manifest-path", shQuote(file.path(dir, "pixi.toml"))),
                 stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) stop("pixi could not build the environment:\n", paste(utils::tail(out, 10), collapse = "\n"))
  invisible(.pixi_python(dir))
}

# Does a Python have PARC and PaCMAP? Checked in a separate process, so this
# R session never binds to that Python.
.python_has_parc_pacmap <- function(py) {
  if (!nzchar(py) || !file.exists(py)) return(FALSE)
  out <- tryCatch(suppressWarnings(system2(py, c("-c", shQuote("import parc; import pacmap",
                                    type = if (.Platform$OS.type == "windows") "cmd" else "sh")),
                                stdout = TRUE, stderr = TRUE)), error = function(e) structure("", status = 1L))
  is.null(attr(out, "status")) || identical(attr(out, "status"), 0L)
}
