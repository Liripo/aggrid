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
#' @param enableRangeSelection Enable Cells range select?
#' @param statusBar
#' @param ... For more options, please see <https://www.ag-grid.com/javascript-data-grid/grid-options>
#' @param theme Specify theme. Default is `ag-theme-balham`. please see <https://www.ag-grid.com/example/?theme=ag-theme-alpine>
#' @param community Enable to use community?
#' @param server server model
#' @param use_cdn Depends resource to use cdn?
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
  resizable = TRUE,
  suppressRowClickSelection = FALSE,
  rowSelection = c('multiple','single'),
  rowMultiSelectWithClick = FALSE,
  checkboxSelection = FALSE,
  pagination = FALSE,
  paginationPageSize = 10,
  domLayout = NULL,
  sideBar = FALSE,
  enableRangeSelection = TRUE,
  statusBar = TRUE,
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
  rowSelection <- match.arg(rowSelection)

  column_defs <- purrr::imap(data,function(x,i) {
    list(field = i,
         filter = to_aggrid_filter(class(x)),
         filterParams = list(maxNumConditions = 5)
    )
  })

  if (isTRUE(checkboxSelection)) {
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
        list(statusPanel = 'agTotalRowCountComponent'),
        list(statusPanel = 'agFilteredRowCountComponent',align = 'center'),
        list(statusPanel = 'agSelectedRowCountComponent',align = 'center'),
        list(statusPanel = 'agAggregationComponent',align = 'center')
      )
    )
  } else if (isFALSE(statusBar)) {
    statusBar <- NULL
  }
  deps <- aggrid_dependency(use_cdn,community)
  if (server) {
    session <- shiny::getDefaultReactiveDomain()
    outputId <- shiny::getCurrentOutputInfo(session = session)[["name"]]
    filterFunc <- function(data, req) {
      params <- rawToChar(req$rook.input$read()) |>
        jsonlite::fromJSON()
      slice_data <- data[params$startRow:params$endRow,]

      shiny::httpResponse(
        status = 200L,
        content_type = "application/json",
        content = jsonlite::toJSON(
          list(rowData = slice_data, nrow = nrow(data))
        )
      )
    }
    dataURL <- session$registerDataObj(
      outputId,
      data = data,
      filterFunc
    )
    data = NULL
  } else {
    dataURL <- NULL
  }


  x <- list(
    gridOptions = list(
      columnDefs = column_defs,
      suppressRowClickSelection = suppressRowClickSelection,
      rowSelection = rowSelection,
      enableRangeSelection = enableRangeSelection,
      rowMultiSelectWithClick = TRUE,
      pagination = pagination,
      domLayout = domLayout,
      statusBar = statusBar,
      paginationPageSize = paginationPageSize,
      defaultColDef = default_coldef,
      suppressFieldDotNotation = TRUE
    ),
    theme = theme,
    data = data,
    server = server,
    dataURL = dataURL
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
      defaultWidth = "100%",
      knitr.defaultWidth = "100%",
      knitr.defaultHeight = 500,
      knitr.figure = FALSE
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
aggridOutput <- function(outputId, width = '100%', height = '500px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'aggrid', width, height, package = 'aggrid')
}

#' @rdname aggrid-shiny
#' @export
renderAggrid <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, aggridOutput, env, quoted = TRUE)
}
