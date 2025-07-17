install.packages("pak", repos = "https://cran.rstudio.com", type = "source")
pak::pkg_install(c("peterleary/MARMOT", "scGate"), ask = F, dependencies = T)