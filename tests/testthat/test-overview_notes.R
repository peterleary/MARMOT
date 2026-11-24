test_that("the Overview's warnings survive for a reload, callouts and all", {
  dir <- tempfile(); dir.create(dir)
  expect_null(read_overview_warnings(dir))
  w <- c("::: {.callout-warning title='RAM Cap Applied'}\nSome **bold** text, 'quotes' and ü.\n:::\n",
         "::: {.callout-note title='Names lined up'}\n\n- a\n- b\n\n:::\n")
  save_overview_warnings(dir, w)
  expect_identical(read_overview_warnings(dir), w)
  save_overview_warnings(dir, character())
  expect_identical(read_overview_warnings(dir), character())
})
