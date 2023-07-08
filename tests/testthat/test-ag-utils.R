test_that("test ag_coldef", {
  x <- aggrid(iris) |>
    ag_col_def(columns = c(Species, Sepal.Length), width = 100)
  coldef <- x$x$gridOptions$columnDefs
  expect_equal(coldef$Species$width, 100)
  expect_equal(coldef$Sepal.Length$width, 100)
})
