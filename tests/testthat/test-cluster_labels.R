make_labels_sheet <- function(labels) {
  set.seed(1)
  cl <- factor(rep(c("1", "2", "3", "4"), each = 25))
  expr <- matrix(rnorm(3 * 100), nrow = 3, dimnames = list(c("CD3", "CD19", "CD11b"), NULL))
  expr["CD3", cl == "1"] <- expr["CD3", cl == "1"] + 5
  path <- tempfile(fileext = ".xlsx")
  write_cluster_labels_template(path, cl, expr)
  tab <- openxlsx::read.xlsx(path, sheet = "Cluster Labels", check.names = FALSE, sep.names = " ")
  tab$Label <- labels
  wb <- openxlsx::loadWorkbook(path)
  openxlsx::writeData(wb, "Cluster Labels", tab)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  path
}

test_that("the template lists every cluster with its size and distinctive markers", {
  path <- make_labels_sheet(rep(NA, 4))
  tab <- openxlsx::read.xlsx(path, sheet = "Cluster Labels", check.names = FALSE, sep.names = " ")
  expect_identical(tab$Cluster, c("1", "2", "3", "4"))
  expect_equal(tab$Cells, rep(25, 4))
  expect_match(tab$`Most distinctive markers`[1], "CD3")
  expect_null(read_cluster_labels(path, c("1", "2", "3", "4")))
})

test_that("labels merge, stay blank, drop, and are matched forgivingly", {
  path <- make_labels_sheet(c("T cells", "t-cells", NA, "Drop"))
  lab <- read_cluster_labels(path, c("1", "2", "3", "4"))
  expect_identical(unname(lab$labels), c("T cells", "T cells", "3", NA))
  expect_identical(lab$dropped, "4")
  expect_true(any(grepl("'t-cells' was taken as 'T cells'", lab$notes, fixed = TRUE)))
  expect_true(any(grepl("Clusters 1, 2 were merged into 'T cells'", lab$notes, fixed = TRUE)))
})

test_that("labels for clusters not in the results are ignored and reported", {
  path <- make_labels_sheet(c("B cells", NA, NA, NA))
  lab <- read_cluster_labels(path, c("2", "3", "4"))
  expect_identical(lab$unknown, "1")
  expect_true(any(grepl("not in these results", lab$notes)))
})
