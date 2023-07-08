test_that("test sort", {
  # expect sort result
  sortModel <- data.frame(
    sort = c("asc", "desc"),
    colId = c("Sepal.Length", "Species")
  )
  true_index <- order(iris[["Sepal.Length"]], iris[["Species"]],
    decreasing = sortmodel$sort == "desc"
  )
  order_index <- do.call(
    function(...) {
      order(..., decreasing = sortModel$sort == "desc")
    },
    lapply(sortModel$colId, function(col) {
      iris[[col]]
    })
  )

  expect_equal(true_index, order_index)
})
