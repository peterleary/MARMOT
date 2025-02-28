BiocManager::install("flowDensity")
library(flowDensity)
library(flowCore)
# https://rdrr.io/bioc/flowDensity/f/vignettes/flowDensity.Rmd
data_dir <- system.file("extdata", package = "flowDensity")
load(list.files(pattern = 'sampleFCS_1', data_dir, full = TRUE))
f
f1 <- frames$`export_A1 S001_Subcu Samples_Cleaned Leukocytes.fcs`
f1@parameters@data
sngl <- flowDensity::flowDensity(
  obj = f1,
  channels = c("FJComp-BUV496-A", "FJComp-FITC-A"),
  position = c(F,F),
  percentile =c(.99999,.99999),
  use.percentile = c(T,T),
  ellip.gate = T,
  scale = .99
  )
plotDens(f1,c(12,25))
lines(sngl@filter,type="l")
