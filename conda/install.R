if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::pkg_install(c("peterleary/MARMOT", "scGate"), ask = F, dependencies = T)