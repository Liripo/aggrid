#' Create a HTML widget using the ag-grid library
#'
#' This function creates a HTML widget to display matrix or a dataframe using ag-grid.
#' @param data dataframe
#' @param columnDefs custom every column
#' @param defaultColDef custom default column
#' @param sortable Enable sorting? Defaults to `TRUE`.
#' @param filter Enable column filtering? Defaults to `TRUE`.
#' @param resizable Enable column resizable?
#' @param suppressRowClickSelection Enable to suppress column select?
#' @param checkboxSelection Enable checkbox in first column?
#' @param pagination Enable pagination allows?
#' @param paginationPageSize default size is `10`.
#' @param domLayout please see <https://www.ag-grid.com/javascript-data-grid/grid-size/#dom-layout>
#' @param sideBar Include sidebar.
#' @param statusBar
#' @param ... For more options, please see <https://www.ag-grid.com/javascript-data-grid/grid-options>
#' @param theme theme, please see <https://www.ag-grid.com/example/?theme=ag-theme-alpine>
#' @param Community Enable to use Community?
#' @param width,height Width/Height in pixels (optional, defaults to automatic sizing)
#' @param elementId An id for the widget (a random string by default).
#'
#' @import htmlwidgets
#' @importFrom purrr imap
#' @importFrom utils modifyList
#'
#' @examples
#' aggrid(iris)
#'
#' ## use theme
#' aggrid(iris,theme = "ag-theme-alpine")
#'
#' ## auto size columns
#' aggrid(iris) |> auto_size_columns()
#' @export
aggrid <- function(data,
  columnDefs = NULL,
  defaultColDef = NULL,
  sortable = TRUE,
  filter = TRUE,
  resizable = FALSE,
  suppressRowClickSelection = FALSE,
  rowSelection = c('multiple','single'),
  checkboxSelection = FALSE,
  pagination = FALSE,
  paginationPageSize = 10,
  domLayout = NULL,
  sideBar = FALSE,
  statusBar = TRUE,
  ...,
  theme = "ag-theme-balham",
  Community = FALSE,
  width = NULL,
  height = NULL,
  elementId = NULL) {

  # --------------- para check
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  stopifnot(is.data.frame(data))
  rowSelection <- match.arg(rowSelection)

  column_defs <- purrr::imap(data,function(x,i) {
    list(field = i)
  })

  if (isFALSE(suppressRowClickSelection) && isTRUE(checkboxSelection)) {
    column_defs[[1]] <- c(
      column_defs[[1]],
      list(checkboxSelection = TRUE,headerCheckboxSelection = rowSelection == "multiple")
    )
  }

  if (!is.null(columnDefs)) {
    column_defs <- modifyList(
      column_defs,
      columnDefs
    )
  }

  if (is.null(domLayout)) {
    domLayout <- if (pagination || nrow(data) <= 10) {
      "autoHeight"
    } else NULL
  }
  default_coldef <- list(sortable = sortable,filter = filter,resizable = resizable)
  if (!is.null(defaultColDef)) {
    default_coldef <- modifyList(
      default_coldef,defaultColDef
    )
  }

  #
  if (isTRUE(statusBar)) {
    statusBar <- list(
      statusPanels = list(
        list(statusPanel = 'agTotalAndFilteredRowCountComponent',align = 'left'),
        list(statusPanel = 'agTotalRowCountComponent', align = 'center'),
        list(statusPanel = 'agFilteredRowCountComponent'),
        list(statusPanel = 'agSelectedRowCountComponent'),
        list(statusPanel = 'agAggregationComponent')
      )
    )
  } else if (isFALSE(statusBar)) {
    statusBar <- NULL
  }
  deps <- aggrid_dependency(use_cdn = FALSE,Community = Community)


  # toJSON should not has names
  names(column_defs) <- NULL
  x <- list(
    gridOptions = list(
      columnDefs = column_defs,
      suppressRowClickSelection = suppressRowClickSelection,
      rowSelection = rowSelection,
      rowMultiSelectWithClick = TRUE,
      pagination = pagination,
      domLayout = domLayout,
      statusBar = statusBar,
      paginationPageSize = paginationPageSize,
      defaultColDef = default_coldef,
      suppressFieldDotNotation = TRUE,
      rowData = data
    ),
    theme = theme
  )

  dot_list <- list(...)
  x$gridOptions <- c(x$gridOptions,dot_list)

  # Refer to the aggrid tutorial, the data needs to be converted by row.
  attr(x, 'TOJSON_ARGS') <- list(dataframe = "rows")

  # create widget
  htmlwidgets::createWidget(
    name = 'aggrid',
    x,
    width = width,
    height = height,
    package = 'aggrid',
    elementId = elementId,
    dependencies = deps,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%"
    )
  )
}

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
aggridOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'aggrid', width, height, package = 'aggrid')
}

#' @rdname aggrid-shiny
#' @export
renderAggrid <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, aggridOutput, env, quoted = TRUE)
}
