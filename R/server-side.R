# session$registerDataObj filter function
# TO DO: custom backend
filterFunc <- function(data, req) {
  params <- rawToChar(req$rook.input$read()) |>
    jsonlite::fromJSON()
  data <- agServer_filter(data,params)

  slice_data <- data |>
    dplyr::slice((params$startRow + 1):params$endRow)

  shiny::httpResponse(
    status = 200L,
    content_type = "application/json",
    content = jsonlite::toJSON(
      list(
        rowData = slice_data,
        lastRow = nrow(data)
      )
    )
  )
}

agServer_filter <- function(data,params) {
  # filter should in head
  # 过滤
  if (!rlang::is_empty(params$filterModel)) {
    for (i in seq_along(params$filterModel)) {
      colum_name <- names(params$filterModel)[i]
      filterModel <- params$filterModel[[i]]
      filter_expr <- rlang::parse_expr()
      data <- data |>
        dplyr::filter(!!filter_expr)
    }
  }

  # group_by
  if (!rlang::is_empty(params$rowGroupCols)) {
    if (rlang::is_empty(params$groupKeys)) {
      data <- data |>
        group_by(across(all_of(params$rowGroupCols$field))) |>
        summarise()
      return(data)
    } else {
      # groupKeys one length
      data <- data |>
        filter(.data[[params$rowGroupCols$field]] == params$groupKeys) |>
        select(-all_of(params$rowGroupCols$field))
    }
  }
  # arrange
  if (!rlang::is_empty(params$sortModel)) {
    ## 排序
    sortModel <- params$sortModel
    order_index <- do.call(
      function(...) {
        order(..., decreasing = sortModel$sort == "desc")
      },
      lapply(sortModel$colId, function(col) {
        data[[col]]
      })
    )

    data <- data[order_index, ]
  }
  data
}
