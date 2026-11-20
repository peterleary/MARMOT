# Tests for R/safe_file_name.R

test_that("plain names are left alone", {
  expect_equal(safe_file_name(c("WT", "Ccr2-KO", "CD4+", "APE over AP", "Contrast_1")),
               c("WT", "Ccr2-KO", "CD4+", "APE over AP", "Contrast_1"))
})

test_that("Greek, subscripts and superscripts become plain letters", {
  expect_equal(safe_file_name("\u03b1-PD1 (high) 5mg/kg over IFN-\u03b3\u207a H\u2082O\u2082"),
               "alpha-PD1 (high) 5mg_kg over IFN-gamma+ H2O2")
  expect_equal(safe_file_name(c("TNF\u03b1", "\u0394Np63", "CO\u2082", "10 m\u00b2", "50 \u00b5g")),
               c("TNFalpha", "DeltaNp63", "CO2", "10 m2", "50 ug"))
})

test_that("characters a file system forbids become underscores", {
  expect_equal(safe_file_name("CD45RA/RO"), "CD45RA_RO")
  expect_equal(safe_file_name("a\\b:c*d?e\"f<g>h|i"), "a_b_c_d_e_f_g_h_i")
  expect_false(grepl("[/\\\\:*?\"<>|]", safe_file_name("x/\\:*?\"<>|y")))
})

test_that("anything else that is not plain ASCII becomes an underscore", {
  expect_equal(safe_file_name("A\u2192B"), "A_B")           # an arrow
  expect_equal(safe_file_name("caf\u00e9 cr\u00e8me"), "cafe creme")
  expect_true(all(utf8ToInt(safe_file_name("\u03b1\u2192\u65e5\u672c")) < 127))
})

test_that("the result is the same length, and never empty or ending in a dot", {
  expect_equal(safe_file_name(c("a", NA, "")), c("a", "NA", "unnamed"))
  expect_equal(safe_file_name("ends with dot. "), "ends with dot")
  expect_equal(safe_file_name(character(0)), character(0))
})

test_that("the result does not depend on R's language setting", {
  awkward <- "\u03b1-PD1 over IFN-\u03b3\u207a"
  utf8 <- safe_file_name(awkward)
  withr::with_locale(c(LC_CTYPE = "C"), expect_equal(safe_file_name(awkward), utf8))
})
