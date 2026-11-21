make_coordinate_tables <- function(n = 5) {
  cells <- data.frame(sample_id = "s1", file_name = "a.fcs", event_number = seq_len(n), sce_idx = seq_len(n),
                      x = seq(0.1, by = 0.1, length.out = n), y = 1, DRMethod = "UMAP", cluster_id = "1", CD4 = 0.5)
  list(
    cells = cells,
    arcsinh = data.frame(cells[, c("sample_id", "file_name", "event_number", "sce_idx")], CD4 = 2.5),
    colours = data.frame(used_for = "cluster_id", value = "1", colour = "#DC050CFF")
  )
}

test_that("the coordinates file has four sheets that read back", {
  tb <- make_coordinate_tables()
  f <- tempfile(fileext = ".xlsx")
  write_coordinates_file(f, tb$cells, tb$arcsinh, tb$colours, about = list("Clustering" = "MfastPG", "Skipped" = NULL))
  expect_equal(openxlsx::getSheetNames(f), c("Read me", "Cells", "Expression (arcsinh)", "Colours"))
  expect_equal(openxlsx::read.xlsx(f, "Cells"), tb$cells)
  expect_equal(openxlsx::read.xlsx(f, "Expression (arcsinh)"), tb$arcsinh)
  expect_equal(openxlsx::read.xlsx(f, "Colours"), tb$colours)
  readme <- openxlsx::read.xlsx(f, "Read me")
  expect_true(all(c("event_number", "x, y", "Clustering") %in% readme$Item))
  expect_false("Skipped" %in% readme$Item)
  expect_equal(readme$Description[readme$Item == "Clustering"], "MfastPG")
})

test_that("the read me describes only the columns that are there", {
  tb <- make_coordinate_tables()
  tb$cells$file_name <- NULL
  readme <- MARMOT:::.coordinates_readme(tb$cells, list(), as_csv = FALSE)
  expect_false("file_name" %in% readme$Item)
  expect_false("Why separate files" %in% readme$Item)
})

test_that("the two cell tables must hold the same cells", {
  tb <- make_coordinate_tables()
  expect_error(write_coordinates_file(tempfile(fileext = ".xlsx"), tb$cells, tb$arcsinh[1:2, ], tb$colours))
})

test_that("more cells than fit on an Excel sheet are saved as CSV files", {
  tb <- make_coordinate_tables()
  f <- file.path(tempfile("coords"), "UMAP_Coordinates.xlsx")
  dir.create(dirname(f))
  testthat::local_mocked_bindings(.excel_max_rows = 3L)
  expect_message(paths <- write_coordinates_file(f, tb$cells, tb$arcsinh, tb$colours), "CSV")
  expect_false(file.exists(f))
  expect_equal(basename(paths), c("UMAP_Coordinates_Read_me.csv", "UMAP_Coordinates_Cells.csv",
                                  "UMAP_Coordinates_Expression_arcsinh.csv", "UMAP_Coordinates_Colours.csv"))
  expect_equal(nrow(data.table::fread(paths[2])), 5)
  expect_true("Why separate files" %in% data.table::fread(paths[1])$Item)
})
