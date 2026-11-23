# Plumbing shared by install_marmot_extras(), check_setup() and marmot()

# Where MARMOT's packages come from: Peter's r-universe (ready-built MARMOT,
# Rphenograph and the GitHub-only packages), then CRAN and Bioconductor
marmot_repos <- function() {
  bioc <- tryCatch(BiocManager::repositories(), error = function(e) c(CRAN = "https://cloud.r-project.org"))
  c(MARMOT = "https://peterleary.r-universe.dev", bioc)
}

# Every package MARMOT uses, read from its DESCRIPTION, so the installer and
# the setup check can never drift from what the package declares. Tools only
# needed to develop MARMOT are left out.
.marmot_dependencies <- function(desc = NULL) {
  if (is.null(desc)) {
    desc <- tryCatch(utils::packageDescription("MARMOT"), error = function(e) NULL)
    if (is.null(desc) || is.null(desc$Imports)) {
      desc <- as.list(read.dcf(system.file("DESCRIPTION", package = "MARMOT"))[1, ])
    }
  }
  parse <- function(field) {
    if (is.null(field) || is.na(field)) return(character())
    x <- trimws(strsplit(gsub("\\([^)]*\\)", "", field), ",")[[1]])
    x[nzchar(x)]
  }
  base_pkgs <- rownames(utils::installed.packages(priority = "base"))
  dev_only <- c("testthat", "withr", "pak", "remotes")
  list(
    imports  = setdiff(parse(desc$Imports), base_pkgs),
    suggests = setdiff(parse(desc$Suggests), c(base_pkgs, dev_only))
  )
}

# Find Quarto: on the search path, or the copy that ships inside RStudio or
# Positron, so users need not set up anything. Returns "" when none is found.
.find_quarto <- function() {
  on_path <- Sys.which("quarto")
  if (nzchar(on_path)) return(unname(on_path))
  set_by_user <- Sys.getenv("QUARTO_PATH")
  if (nzchar(set_by_user) && file.exists(set_by_user)) return(set_by_user)
  exe <- if (.Platform$OS.type == "windows") "quarto.exe" else "quarto"
  local_app <- Sys.getenv("LOCALAPPDATA")
  candidates <- c(
    "/Applications/Positron.app/Contents/Resources/app/quarto/bin/quarto",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto",
    "/Applications/RStudio.app/Contents/MacOS/quarto/bin/quarto",
    "/usr/lib/rstudio/resources/app/bin/quarto/bin/quarto",
    "/usr/lib/rstudio-server/bin/quarto/bin/quarto",
    "/usr/share/positron/resources/app/quarto/bin/quarto",
    file.path(Sys.getenv("ProgramFiles"), "RStudio", "resources", "app", "bin", "quarto", "bin", exe),
    file.path(local_app, "Programs", "Positron", "resources", "app", "quarto", "bin", exe),
    file.path(Sys.getenv("ProgramFiles"), "Positron", "resources", "app", "quarto", "bin", exe)
  )
  found <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(found)) found[[1]] else ""
}
