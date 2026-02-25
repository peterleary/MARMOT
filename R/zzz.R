.onLoad <- function(libname, pkgname) {
  # On arm64 macOS, ensure setup_python()'s conda_create fetches arm64 packages.
  if (identical(Sys.info()[["sysname"]], "Darwin") &&
      identical(Sys.info()[["machine"]], "arm64") &&
      !nzchar(Sys.getenv("CONDA_SUBDIR"))) {
    Sys.setenv(CONDA_SUBDIR = "osx-arm64")
  }
  # Suppress OpenMP informational messages from numba/llvmlite.
  if (!nzchar(Sys.getenv("KMP_WARNINGS"))) Sys.setenv(KMP_WARNINGS = "0")
  if (!nzchar(Sys.getenv("OMP_DISPLAY_ENV"))) Sys.setenv(OMP_DISPLAY_ENV = "FALSE")
}
