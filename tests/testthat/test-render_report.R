test_that("a failed render says where the real reason is, not a garbled one", {
  local_mocked_bindings(quarto_render = function(...) stop('could not find function "Mparc"'), .package = "quarto")
  err <- expect_error(.render_report("x.qmd", "x.html"), class = "error")
  expect_match(conditionMessage(err), "could not finish the report")
  expect_match(conditionMessage(err), "Quitting from")
  expect_no_match(conditionMessage(err), "Mparc")
})

test_that("a good render passes through quietly", {
  local_mocked_bindings(quarto_render = function(...) invisible(NULL), .package = "quarto")
  expect_true(.render_report("x.qmd", "x.html"))
})
