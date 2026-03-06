.onLoad <- function(libname, pkgname) {
  # Suppress OpenMP informational messages from numba/llvmlite.
  if (!nzchar(Sys.getenv("KMP_WARNINGS"))) Sys.setenv(KMP_WARNINGS = "0")
  if (!nzchar(Sys.getenv("OMP_DISPLAY_ENV"))) Sys.setenv(OMP_DISPLAY_ENV = "FALSE")
  # Suppress "omp_set_nested deprecated" info message from OpenMP runtime.
  if (!nzchar(Sys.getenv("OMP_MAX_ACTIVE_LEVELS"))) Sys.setenv(OMP_MAX_ACTIVE_LEVELS = "1")
}
