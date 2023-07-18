#' Create a HTML widget using the ag-grid library
#'
#' This function creates a HTML widget to display matrix or a data frame using ag-grid.
#' @param data data frame or matrix.
#' @param rowSelection three options：`multiple`,`single`.
#' @param suppressRowClickSelection When `TRUE`, disable row selection when clicking.
#' @param selectedRows set default selectedRows rows.
#' @param checkboxSelection Enable checkbox in first column?
#' @param pagination Enable pagination allows?
#' @param paginationPageSize default size is `10`.
#' @param ... For more options, please see <https://www.ag-grid.com/javascript-data-grid/grid-options>
#' @param theme Specify theme. Default is `ag-theme-balham`. please see <https://www.ag-grid.com/example/?theme=ag-theme-alpine>
#' @param community Enable to use community?
#' @param server server model
#' @param use_cdn Depends resource to use cdn?
#' @param width,height Width/Height in pixels (optional, defaults to automatic sizing)
#' @param elementId An id for the widget (a random string by default).
#'
#' @importFrom purrr imap
#' @importFrom utils modifyList
#'
#' @examples
#' aggrid(iris)
#'
#' ## use theme
#' aggrid(iris, theme = "ag-theme-alpine")
#'
#' ## auto size columns
#' aggrid(iris) |> auto_size_columns()
#'
#' ## select row
#' head(iris, n = 10) |>
#'   aggrid(checkboxSelection = T,selectedRows = 1:3)
#' @export
aggrid <- function(data,
                   rowSelection = c("multiple", "single"),
                   suppressRowClickSelection = FALSE,
                   selectedRows = NULL,
                   checkboxSelection = FALSE,
                   pagination = FALSE,
                   paginationPageSize = 10,
                   ...,
                   theme = "ag-theme-balham",
                   community = FALSE,
                   use_cdn = FALSE,
                   server = FALSE,
                   width = NULL,
                   height = NULL,
                   elementId = NULL) {
  # --------------- para check
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  stopifnot(is.data.frame(data))
  # data <- data.table::as.data.table(data)
  rowSelection <- match.arg(rowSelection)
  n_row <- nrow(data)

  # need to make row id
  # https://www.ag-grid.com/javascript-data-grid/server-side-model-configuration/#providing-row-ids
  # Need to include row numbers in data to be able to return correctly when row is selected in shiny
  if ("rowid" %in% colnames(data)) {
    stop("Temporary column name cannot have `rowid`.")
  }
  data$rowid <- seq_len(n_row)

  column_defs <- purrr::imap(data, function(x, i) {
    filterParams <- list(maxNumConditions = 5)
    if (is.factor(x) && isTRUE(server)) {
      filterParams$values <- levels(x)
    }
    if (inherits(x, "Date")) {
      filterParams$comparator <- date_comparator()
    }
    list(
      field = i,
      filter = to_aggrid_filter(class(x)),
      filterParams = filterParams,
      hide = i == "rowid",
      suppressColumnsToolPanel = i == "rowid"
    )
  })

  if (isTRUE(checkboxSelection)) {
    column_defs[[1]] <- c(
      column_defs[[1]],
      list(checkboxSelection = TRUE,
           headerCheckboxSelection = rowSelection != "single")
    )
  }

  # default rowSelected
  if (!is.null(selectedRows)) {
    if (rowSelection == "single" && length(selectedRows) > 1) {
      cli::cli_abort("`rowSelection` is single but `selectedRows` length > 1.")
    }
    if (server && is.numeric(selectedRows)) {
      # server  module use toggledNodes
      selectedRows = seq_len(n_row)[-selectedRows]
    }
    if (is.character(selectedRows) && selectedRows != "all") {
      cli::cli_abort("selectedRows only support set as `all` or `numeric` class.")
    }
  }

  domLayout <- if (pagination || nrow(data) <= 15) {
    "autoHeight"
  } else {
    NULL
  }


  statusBar <- list(
    statusPanels = list(
      list(statusPanel = "agTotalAndFilteredRowCountComponent", align = "left"),
      list(statusPanel = "agTotalRowCountComponent"),
      list(statusPanel = "agFilteredRowCountComponent", align = "center"),
      list(statusPanel = "agSelectedRowCountComponent", align = "center"),
      list(statusPanel = "agAggregationComponent", align = "center")
    )
  )

  deps <- aggrid_dependency(use_cdn, community)
  if (server) {
    session <- shiny::getDefaultReactiveDomain()
    outputId <- shiny::getCurrentOutputInfo(session = session)[["name"]]
    dataURL <- session$registerDataObj(
      outputId,
      data = data,
      filterFunc
    )
  } else {
    dataURL <- NULL
  }


  x <- list(
    gridOptions = list(
      columnDefs = column_defs,
      suppressRowClickSelection = suppressRowClickSelection,
      rowSelection = rowSelection,
      enableRangeSelection = TRUE,
      rowMultiSelectWithClick = TRUE,
      pagination = pagination,
      domLayout = domLayout,
      # It seems better to let users click ctrl and mouse?
      alwaysMultiSort = TRUE,
      statusBar = statusBar,
      paginationPageSize = paginationPageSize,
      defaultColDef = list(
        sortable = TRUE,
        resizable = TRUE,
        # for side bar
        enableRowGroup = TRUE,
        enableValue = TRUE,
        enablePivot = TRUE
      ),
      suppressFieldDotNotation = TRUE
    ),
    theme = theme,
    data = data,
    selectedRows = selectedRows,
    server = server,
    dataURL = dataURL,
    n_row = n_row
  )

  dot_list <- list(...)
  x$gridOptions <- c(x$gridOptions, dot_list)

  # Refer to the aggrid tutorial, the data needs to be converted by row.
  attr(x, "TOJSON_ARGS") <- list(dataframe = "rows")

  # create widget
  htmlwidgets::createWidget(
    name = "aggrid",
    x,
    width = width,
    height = height,
    package = "aggrid",
    elementId = elementId,
    dependencies = deps,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      knitr.defaultWidth = "100%",
      knitr.defaultHeight = 500,
      knitr.figure = FALSE
    ),
    preRenderHook = preRender_aggrid
  )
}

# preRenderHook function
# more please see <https://book.javascript-for-r.com/widgets-adv.html#>
preRender_aggrid <- function(x) {
  # when shiny serve, don't send data to javascript
  if (x$x$server) {
    x$x$data <- NULL
  }
  # group support
  if (!is.null(x$x$group)) {
    cols <- lapply(x$x$group, function(group) group$children) |>
      unlist()
    dup_cols <- cols[duplicated(cols)]
    if (length(dup_cols) > 0) {
      cli::cli_abort("{dup_cols} columns has duplicate headerName.Currently not supported.IF you want to multi headerName, please see <https://www.ag-grid.com/javascript-data-grid/column-groups/#advanced-grouping-example>. And modify `aggird` Object.")
    }
    group_columnDefs <- lapply(x$x$group, function(group) {
      group_children <- x$x$gridOptions$columnDefs[group$children]
      names(group_children) <- NULL
      group$children <- group_children
      group
    })
    x$x$gridOptions$columnDefs[cols] <- NULL
    x$x$gridOptions$columnDefs <- c(group_columnDefs, x$x$gridOptions$columnDefs)
  }
  names(x$x$gridOptions$columnDefs) <- NULL
  return(x)
}

# session$registerDataObj filter function
# TO DO: custom backend
# filterFunc <- function(data, req) {
#   params <- rawToChar(req$rook.input$read()) |>
#     jsonlite::fromJSON()
#
#   if (!rlang::is_empty(params$sortModel)) {
#     ## 排序
#     sortModel <- params$sortModel
#     order_index <- do.call(
#       function(...) {
#         order(..., decreasing = sortModel$sort == "desc")
#       },
#       lapply(sortModel$colId, function(col) {
#         data[[col]]
#       })
#     )
#
#     data <- data[order_index, ]
#   }
#   # 过滤
#   if (!rlang::is_empty(params$filterModel)) {
#     for (i in seq_along(params$filterModel)) {
#       colum_name <- names(params$filterModel)[i]
#       filterModel <- params$filterModel[[i]]
#       filter_expr <- rlang::parse_expr(filter_to_expr(colum_name, filterModel))
#       print(filter_expr)
#       data <- data |>
#         dplyr::filter(!!filter_expr)
#     }
#   }
#   if (!rlang::is_empty(params$rowGroupCols)) {
#     # cli::cli_abort("Server model don't support.Welcome to contribute.")
#     data <- data |>
#       dplyr::group_by(Species) |>
#       dplyr::summarise()
#   }
#
#   slice_data <- data |>
#     dplyr::slice((params$startRow + 1):params$endRow)
#
#   shiny::httpResponse(
#     status = 200L,
#     content_type = "application/json",
#     content = jsonlite::toJSON(
#       list(
#         rowData = slice_data,
#         lastRow = nrow(data)
#       )
#     )
#   )
# }

#' Shiny bindings for aggrid
#'
#' Output and render functions for using aggrid within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a aggrid
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name aggrid-shiny
#'
#' @export
aggridOutput <- function(outputId, width = "100%", height = "500px") {
  htmlwidgets::shinyWidgetOutput(outputId, "aggrid", width, height, package = "aggrid")
}

#' @rdname aggrid-shiny
#' @export
renderAggrid <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, aggridOutput, env, quoted = TRUE)
}
