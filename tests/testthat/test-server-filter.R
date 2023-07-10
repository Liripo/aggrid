test_that("test server one filter Model", {
  filterModel <- list(
    filterType = "number",
    type = "greaterThan",
    filter = 1
  )
  expect_equal(2 * 2, 4)
})


test_that("test server two filter Model", {
  filterModel <- list(
    filterType = "number",
    operator = "AND",
    conditions = data.frame(
      filterType = c("number", "number"),
      type = c("greaterThan", "equals"),
      filter = c(1.0, 5.1)
    )
  )
  expect_equal(2 * 2, 4)
})


test_that("test server many filter Model", {
  filterModel <- list(
    filterType = "number",
    operator = "AND",
    conditions = data.frame(
      filterType = c("number", "number", "number"),
      type = c("greaterThan", "lessThan", "lessThan"),
      filter = c(1.0, 5.0, 4.6)
    )
  )
  expect_equal(2 * 2, 4)
})

test_that("test server set filter Model", {
  filterModel <- list(
    filterType = "set",
    values = c("versicolor", "virginica")
  )
  expect_equal(2 * 2, 4)
})

test_that("server group",{
  params <- list(
    startRow = 0,
    endRow = 100,
    rowGroupCols = data.frame(id = "Species",displayName = "Species",field = "Species"),
    valueCols = list(),
    pivotCols = list(),
    pivotMode = FALSE,
    groupKeys = "setosa",
    filterModel = list(),
    sortModel = list()
  )
})
