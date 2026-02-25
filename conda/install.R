# Install MARMOT and all dependencies
# Run this script in a fresh R session

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = "https://cran.rstudio.com")
}

pak::pkg_install("peterleary/MARMOT", ask = FALSE, dependencies = TRUE)

MARMOT::install_dependencies()
