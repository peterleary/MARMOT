g <- function(...) intToUtf8(c(...))   # Greek letters by code point (the file stays ASCII)

test_that("names with Greek letters become plotmath; others stay as they are", {
  x <- c(paste0("IFN-", g(0x3b3)), "CD4", paste0("TCR", g(0x3b3, 0x3b4)), NA)
  e <- greek_label(x)
  expect_true(is.expression(e))
  expect_length(e, 4)
  expect_identical(e[[1]], quote("IFN-" * gamma))
  expect_identical(e[[2]], "CD4")
  expect_identical(e[[3]], quote("TCR" * gamma * delta))
  expect_identical(greek_label(c("CD4", "CD8")), c("CD4", "CD8"))
  expect_identical(greek_label(g(0xb5)), as.expression(quote(mu)))   # the micro sign too
})

test_that("Greek letters are spelt out where plotmath cannot go", {
  expect_identical(spell_greek(c(paste0("CD8", g(0x3b1)), "CD4", NA)), c("CD8alpha", "CD4", NA))
  expect_identical(spell_greek(paste0(g(0x3b3, 0x3b4), " T cells")), "gammadelta T cells")
})

test_that("markers show their display names, Greek letters and all", {
  e <- marker_label(c("IFNg", "CD4", "CD8a"), display = c(IFNg = paste0("IFN-", g(0x3b3)), CD4 = "", CD8a = NA))
  expect_identical(e[[1]], quote("IFN-" * gamma))
  expect_identical(e[[2]], "CD4")
  expect_identical(e[[3]], "CD8a")
  expect_identical(marker_label(c("CD4", "CD19")), c("CD4", "CD19"))
})

test_that("the facet labeller and a PDF draw Greek letters without warnings", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = 1:2, y = 1:2, f = c(paste0(g(0x3b3, 0x3b4), " T cells"), "B cells"))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point() +
    ggplot2::facet_wrap(~f, labeller = label_greek) +
    ggplot2::labs(title = greek_label(paste0("IFN-", g(0x3b3))))
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  expect_no_warning(print(p))
  grDevices::dev.off()
  expect_gt(file.size(path), 1000)
})

test_that("display names as text, and prepared strip labels with two lines", {
  expect_identical(marker_display(c("IFNg", "CD4"), c(IFNg = "IFN-g")), c("IFN-g", "CD4"))
  expect_identical(marker_display("CD4"), "CD4")
  lk <- c(A = paste0("IFN-", g(0x3b3), " \n n = 9"), B = "B \n n = 3")
  e <- greek_label(lk)
  expect_identical(e[[1]], quote(atop("IFN-" * gamma, "n = 9")))
  lab <- greek_labeller(lk)(data.frame(condition = c("A", "B", "C")))
  expect_length(lab$condition, 3)
  expect_identical(greek_labeller(NULL)(data.frame(antigen = c("CD4", "CD8")))$antigen, c("CD4", "CD8"))
  expect_identical(greek_labeller(c(B = "B \n n = 3"))(data.frame(condition = c("B", "C")))$condition, c("B \n n = 3", "C"))
})
