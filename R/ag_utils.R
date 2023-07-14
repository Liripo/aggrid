#------------------------------------------------------------------------------#
#
#  This file is define aggrid utils function.
#
#------------------------------------------------------------------------------#

#' Resize all column by container width
#'
#' Example from <https://www.ag-grid.com/javascript-data-grid/column-sizing/#resizing-example>
#' @param x aggrid Object
#' @export

ag_size_fit <- function(x) {
  check_aggrid(x)
  x$x$gridOptions$onGridReady <- JS("
    (gridOptions) => {
      gridOptions.api.sizeColumnsToFit();
    }
  ")
  x
}

#' Resize all column by columns content
#' @param x aggrid Object
#' @param skipHeader Enable skip Header?
#'
#' @examples
#'
#' aggrid(iris) |>
#'   ag_size_columns()
#' @export
ag_size_columns <- function(x, skipHeader = FALSE) {
  check_aggrid(x)
  x$x$gridOptions$onGridReady <- JS(sprintf("
    (gridOptions) => {
      const allColumnIds = [];
      gridOptions.columnApi.getColumns().forEach((column) => {
        allColumnIds.push(column.getId());
      });
      gridOptions.columnApi.autoSizeColumns(allColumnIds, %s);
    }
  ", tolower(skipHeader)))
  x
}

#' Column Definitions
#'
#' It is best to directly view the original document, please see <https://www.ag-grid.com/javascript-data-grid/column-properties/>.
#' @param x aggrid Object
#' @param columns Specify the column to modify, Default is all columns.
#' @param field The field of the row object to get the cell's data from.  Deep references into a row object is supported via dot notation, i.e 'address.firstLine'.
#' @param colId The unique ID to give the column. This is optional. If missing, the ID will default to the field.   If both field and colId are missing, a unique ID will be generated.   This ID is used to identify the column in the API for sorting, filtering etc.
#' @param type A comma separated string or array of strings containing ColumnType keys which can be used as a template for a column.  This helps to reduce duplication of properties when you have a lot of common column properties.
#' @param cellDataType The data type of the cell values for this column.  Can either infer the data type from the row data (true - the default behaviour),  define a specific data type (string), or have no data type (false).    If setting a specific data type (string value),  this can either be one of the pre-defined data types  'text', 'number',  'boolean',  'date',  'dateString' or  'object',  or a custom data type that has been defined in the dataTypeDefinitions grid option.     Data type inference only works for the Client-Side Row Model, and requires non-null data.  It will also not work if the valueGetter, valueParser or refData properties are defined,  or if this column is a sparkline.
#' @param valueGetter Function or expression. Gets the value from your data for display.
#' @param valueFormatter A function or expression to format a value, should return a string. Not used for CSV export or copy to clipboard, only for UI cell rendering.
#' @param refData Provided a reference data map to be used to map column values to their respective value from the map.
#' @param keyCreator Function to return a string key for a value.  This string is used for grouping, Set filtering, and searching within cell editor dropdowns.  When filtering and searching the string is exposed to the user, so make sure to return a human-readable value.
#' @param equals Custom comparator for values, used by renderer to know if values have changed. Cells whose values have not changed don't get refreshed.  By default the grid uses === which should work for most use cases.
#' @param checkboxSelection Set to true (or return true from function) to render a selection checkbox in the column.
#' @param showDisabledCheckboxes Set to true to display a disabled checkbox when row is not selectable and checkboxes are enabled.
#' @param toolPanelClass CSS class to use for the tool panel cell. Can be a string, array of strings, or function.
#' @param suppressColumnsToolPanel Set to true if you do not want this column or group to appear in the Columns Tool Panel.
#' @param columnGroupShow Whether to only show the column when the group is open / closed. If not set the column is always displayed as part of the group.
#' @param icons Icons to use inside the column instead of the grid's default icons. Leave undefined to use defaults.
#' @param suppressNavigable Set to true if this column is not navigable (i.e. cannot be tabbed into), otherwise false.  Can also be a callback function to have different rows navigable.
#' @param suppressKeyboardEvent Allows the user to suppress certain keyboard events in the grid cell.
#' @param suppressPaste Pasting is on by default as long as cells are editable (non-editable cells cannot be modified, even with a paste operation).  Set to true turn paste operations off.
#' @param suppressFillHandle Set to true to prevent the fillHandle from being rendered in any cell that belongs to this column.
#' @param ... please see <https://www.ag-grid.com/javascript-data-grid/column-properties/>.
#' @seealso [ag_col_width()]
#' @export

ag_col_def <- function(x,
                       columns = NULL,
                       field = NULL,
                       colId = NULL,
                       type = NULL,
                       cellDataType = NULL,
                       valueGetter = NULL,
                       valueFormatter = NULL,
                       refData = NULL,
                       keyCreator = NULL,
                       equals = NULL,
                       checkboxSelection = NULL,
                       showDisabledCheckboxes = NULL,
                       toolPanelClass = NULL,
                       suppressColumnsToolPanel = NULL,
                       columnGroupShow = NULL,
                       icons = NULL,
                       suppressNavigable = NULL,
                       suppressKeyboardEvent = NULL,
                       suppressPaste = NULL,
                       suppressFillHandle = NULL,
                       ...) {
  check_aggrid(x)
  columns <- tidyselect::eval_select(rlang::enquo(columns), data = x$x$data) |>
    names()
  if (length(columns) == 0) {
    columns <- colnames(x$x$data)
  }
  columns <- columns[columns != "rowid"]

  # get all params as columnDefs
  columnDefs <- list(
    field = field,
    colId = colId,
    type = type,
    cellDataType = cellDataType,
    valueGetter = valueGetter,
    valueFormatter = valueFormatter,
    refData = refData,
    keyCreator = keyCreator,
    equals = equals,
    checkboxSelection = checkboxSelection,
    showDisabledCheckboxes = showDisabledCheckboxes,
    toolPanelClass = toolPanelClass,
    suppressColumnsToolPanel = suppressColumnsToolPanel,
    columnGroupShow = columnGroupShow,
    icons = icons,
    suppressNavigable = suppressNavigable,
    suppressKeyboardEvent = suppressKeyboardEvent,
    suppressPaste = suppressPaste,
    suppressFillHandle = suppressFillHandle
  )
  args <- list(...)
  columnDefs <- c(columnDefs, args)
  # to do: check parameters
  columnDefs <- dropNulls(columnDefs)

  for (column in columns) {
    x$x$gridOptions$columnDefs[[column]] <- utils::modifyList(
      x$x$gridOptions$columnDefs[[column]],
      columnDefs
    )
  }
  x
}

#' Columns: Header
#' @inheritParams ag_col_def
#' @param headerName The name to render in the column header. If not specified and field is specified, the field name will be used as the header name.
#' @param headerValueGetter Function or expression. Gets the value for display in the header.More details
#' @param headerTooltip Tooltip for the column header
#' @param headerClass CSS class to use for the header cell. Can be a string, array of strings, or function. More details
#' @param headerComponent The custom header group component to be used for rendering the component header. If none specified the default AG Grid is used.See: Header Component
#' @param headerComponentParams The parameters to be passed to the headerComponent.
#' @param wrapHeaderText If enabled then column header names that are too long for the column width will wrap onto the next line. Default false
#' @param autoHeaderHeight If enabled then the column header row will automatically adjust height to accommodate the size of the header cell.  This can be useful when using your own headerComponent or long header names in conjunction with wrapHeaderText.
#' @param menuTabs Set to an array containing zero, one or many of the following options: 'filterMenuTab' | 'generalMenuTab' | 'columnsMenuTab'.  This is used to figure out which menu tabs are present and in which order the tabs are shown.
#' @param columnsMenuParams Params used to change the behaviour and appearance of the Columns Menu tab. More details
#' @param suppressMenu Set to true if no menu should be shown for this column header.
#' @param suppressHeaderKeyboardEvent Suppress the grid taking action for the relevant keyboard event when a header is focused. More details
#' @param headerCheckboxSelection If true or the callback returns true, a 'select all' checkbox will be put into the header. More details
#' @param headerCheckboxSelectionFilteredOnly If true, the header checkbox selection will only select filtered items.
#' @param headerCheckboxSelectionCurrentPageOnly If true, the header checkbox selection will only select nodes on the current page.
#' @export

ag_col_header <- function(x,
                          columns = NULL,
                          headerName = NULL,
                          headerValueGetter = NULL,
                          headerTooltip = NULL,
                          headerClass = NULL,
                          headerComponent = NULL,
                          headerComponentParams = NULL,
                          wrapHeaderText = NULL,
                          autoHeaderHeight = NULL,
                          menuTabs = NULL,
                          columnsMenuParams = NULL,
                          suppressMenu = NULL,
                          suppressHeaderKeyboardEvent = NULL,
                          headerCheckboxSelection = NULL,
                          headerCheckboxSelectionFilteredOnly = NULL,
                          headerCheckboxSelectionCurrentPageOnly = NULL,
                          ...) {
  ag_col_def(x,
    columns = {{ columns }},
    headerName = headerName,
    headerValueGetter = headerValueGetter,
    headerTooltip = headerTooltip,
    headerClass = headerClass,
    headerComponent = headerComponent,
    headerComponentParams = headerComponentParams,
    wrapHeaderText = wrapHeaderText,
    autoHeaderHeight = autoHeaderHeight,
    menuTabs = menuTabs,
    columnsMenuParams = columnsMenuParams,
    suppressMenu = suppressMenu,
    suppressHeaderKeyboardEvent = suppressHeaderKeyboardEvent,
    headerCheckboxSelection = headerCheckboxSelection,
    headerCheckboxSelectionFilteredOnly = headerCheckboxSelectionFilteredOnly,
    headerCheckboxSelectionCurrentPageOnly = headerCheckboxSelectionCurrentPageOnly,
    ...
  )
}

#' Modify columns use vectorized.
#' @inherit ag_col_def
#' @seealso [ag_col_def()]
#' @export
ag_col_defs <- function(x, columns = NULL, ...) {
  check_aggrid(x)
  columns <- tidyselect::eval_select(rlang::enquo(columns), data = x$x$data) |>
    names()
  if (length(columns) == 0) {
    columns <- colnames(x$x$data)
  }
  columns <- columns[columns != "rowid"]
  args <- list(...)
  n_col <- length(columns)
  args_tbl <- tibble::as_tibble(args)
  if (nrow(args_tbl) != n_col) {
    cli::cli_abort("`ag_col_defs` all parameters length must is {n_col}.")
  }
  purrr::iwalk(args_tbl, function(x, name) {
    if (is.list(x) && !is.null(names(x))) {
      cli::cli_abort("{name} should not named list.")
    }
  })
  columnDefs <- purrr::pmap(args_tbl, function(...) {
    list(...)
  })
  names(columnDefs) <- columns
  x$x$gridOptions$columnDefs <- utils::modifyList(
    x$x$gridOptions$columnDefs,
    columnDefs
  )
  x
}

#' Columns: Filter
#' @inheritParams ag_col_def
#' @param filter Filter component to use for this column. Set to true to use the default filter. Set to the name of a Provided Filter or set to a IFilterComp.See: Column Filters
#' @param filterParams Params to be passed to the filter component specified in filter.
#' @param filterValueGetter Function or expression. Gets the value for filtering purposes.More details
#' @param getQuickFilterText A function to tell the grid what Quick Filter text to use for this column if you don't want to use the default (which is calling toString on the value). More details
#' @param floatingFilter Whether to display a floating filter for this column.
#' @param floatingFilterComponent The custom component to be used for rendering the floating filter.  If none is specified the default AG Grid is used.
#' @param floatingFilterComponentParams Params to be passed to floatingFilterComponent.
#' @param suppressFiltersToolPanel Set to true if you do not want this column (filter) or group (filter group) to appear in the Filters Tool Panel.
#' @export
ag_col_filter <- function(x,
                          columns = NULL,
                          filter = NULL,
                          filterParams = NULL,
                          filterValueGetter = NULL,
                          getQuickFilterText = NULL,
                          floatingFilter = NULL,
                          floatingFilterComponent = NULL,
                          floatingFilterComponentParams = NULL,
                          suppressFiltersToolPanel = NULL,...) {
  ag_col_def(x,
    columns = {{ columns }},
    filter = filter,
    filterParams = filterParams,
    filterValueGetter = filterValueGetter,
    getQuickFilterText = getQuickFilterText,
    floatingFilter = floatingFilter,
    floatingFilterComponent = floatingFilterComponent,
    floatingFilterComponentParams = floatingFilterComponentParams,
    suppressFiltersToolPanel = suppressFiltersToolPanel,
    ...
  )
}

#' Columns: Pinned
#' @inheritParams ag_col_def
#' @param pinned Pin a column to one side: right or left.
#' @param initialPinned Same as pinned, except only applied when creating a new column. Not applied when updating column definitions.
#' @param lockPinned Set to true to block the user pinning the column, the column can only be pinned via definitions or API.
#' @export
ag_col_pinned <- function(x,
                          columns = NULL,
                          pinned = NULL,
                          initialPinned = NULL,
                          lockPinned = NULL,
                          ...) {
  ag_col_def(x,
    columns = {{ columns }},
    pinned = pinned,
    initialPinned = initialPinned,
    lockPinned = lockPinned,
    ...
  )
}

#-------------------------------------------------------------------------------
#
# Set aggrid Columns cellRender
#
#-------------------------------------------------------------------------------


# sparkline data type not number
# So need convert
convertToNumberArray <- function() {
  jscode <- 'function convertToNumberArray(params) {
    const field = params.colDef.field;
    var value = params.data[field];
    if (typeof value === "number") {
      return [value];
    }
    return value;
  }'
}

#' Columns cell Render
#' @param cellStyle An object of css values / or function returning an object of css values for a particular cell. More details
#' @param cellClass Class to use for the cell. Can be string, array of strings, or function that returns a string or array of strings. More details
#' @param cellClassRules Rules which can be applied to include certain CSS classes. More details
#' @param cellRenderer Provide your own cell Renderer component for this column's cells.See: Cell Renderer
#' @param cellRendererParams Params to be passed to the cellRenderer component.
#' @param cellRendererSelector Callback to select which cell renderer to be used for a given row within the same column. More details
#' @param autoHeight Set to true to have the grid calculate the height of a row based on contents of this column.
#' @param wrapText Set to true to have the text wrap inside the cell - typically used with autoHeight.
#' @param enableCellChangeFlash Set to true to flash a cell when it's refreshed.
#' @param suppressCellFlash Set to true to prevent this column from flashing on changes. Only applicable if cell flashing is turned on for the grid.
#' @export
ag_col_render <- function(x,
                          columns = NULL,
                          cellStyle = NULL,
                          cellClass = NULL,
                          cellClassRules = NULL,
                          cellRenderer = NULL,
                          cellRendererParams = NULL,
                          cellRendererSelector = NULL,
                          autoHeight = NULL,
                          wrapText = NULL,
                          enableCellChangeFlash = NULL,
                          suppressCellFlash = NULL,
                          ...) {
  spark_value_gatter <- NULL
  if (is.character(cellRenderer) &&
      cellRenderer == "agSparklineCellRenderer") {
    if (!is.null(cellRendererParams$sparklineOptions$type) &&
        cellRendererParams$sparklineOptions$type == "bar") {
      cli::cli_warn("By default htmlwidgets set auto_unbox = TRUE so the 1 length atomic elements are not send as array. So default set `valueGetter` to convert Number to Array.")
      spark_value_gatter <- JS(convertToNumberArray())
      # if (is.null(cellRendererParams$sparklineOptions$valueAxisDomain)) {
      #   data <- x$x$data %>% pull({{ columns }})
      #   cellRendererParams$sparklineOptions$valueAxisDomain <- range(data)
      # }
    }
  }

  ag_col_def(x,
    {{ columns }},
    cellStyle = cellStyle,
    cellClass = cellClass,
    cellClassRules = cellClassRules,
    cellRenderer = cellRenderer,
    cellRendererParams = cellRendererParams,
    cellRendererSelector = cellRendererSelector,
    autoHeight = autoHeight,
    wrapText = wrapText,
    enableCellChangeFlash = enableCellChangeFlash,
    suppressCellFlash = suppressCellFlash,
    valueGetter = spark_value_gatter,
    ...
  )
}

#' Custom columns sort
#' @inheritParams ag_col_def
#' @param sortable Set to true to allow sorting on this column.
#' @param sort If sorting by default, set it here. Set to asc or desc. More details
#' @param initialSort Same as sort, except only applied when creating a new column. Not applied when updating column definitions. More details
#' @param sortIndex If sorting more than one column by default, specifies order in which the sorting should be applied.
#' @param initialSortIndex Same as sortIndex, except only applied when creating a new column. Not applied when updating column definitions.
#' @param sortingOrder Array defining the order in which sorting occurs (if sorting is enabled). An array with any of the following in any order ['asc','desc',null].
#' @param comparator Override the default sorting order by providing a custom sort comparator.  valueA, valueB are the values to compare.  nodeA,  nodeB are the corresponding RowNodes. Useful if additional details are required by the sort.  isDescending - true if sort direction is desc. Not to be used for inverting the return value as the grid already applies asc or desc ordering.
#' @param unSortIcon Set to true if you want the unsorted icon to be shown when no sort is applied to this column.
#' @export
ag_col_sort <- function(x,
                        columns = NULL,
                        sortable = NULL,
                        sort = NULL,
                        initialSort = NULL,
                        sortIndex = NULL,
                        initialSortIndex = NULL,
                        sortingOrder = NULL,
                        comparator = NULL,
                        unSortIcon = NULL,
                        ...) {
  ag_col_def(x,
    columns = {{ columns }},
    sortable = sortable,
    sort = sort,
    initialSort = initialSort,
    sortIndex = sortIndex,
    initialSortIndex = initialSortIndex,
    sortingOrder = sortingOrder,
    comparator = comparator,
    unSortIcon = unSortIcon,
    ...
  )
}

#' Columns: Spanning
#' @inheritParams ag_col_def
#' @param colSpan By default, each cell will take up the width of one column. You can change this behaviour to allow cells to span multiple columns.
#' @param rowSpan By default, each cell will take up the height of one row. You can change this behaviour to allow cells to span multiple rows.
#' @export
ag_col_span <- function(x,
                        columns = NULL,
                        colSpan = NULL,
                        rowSpan = NULL,
                        ...) {
  ag_col_def(x,
    columns = {{ columns }},
    colSpan = colSpan,
    rowSpan = rowSpan,
    ...
  )
}


#' Custom columns cell tooltip
#' @inheritParams ag_col_def
#' @param tooltipField The field of the tooltip to apply to the cell.
#' @param tooltipValueGetter Callback that should return the string to use for a tooltip, `tooltipField` takes precedence if set. If using a custom `tooltipComponent` you may return any custom value to be passed to your tooltip component.
#' @param tooltipComponent Provide your own tooltip component for the column.
#' @param The params used to configure `tooltipComponent`.
#' @export
ag_col_tooltip <- function(x,
                           columns = NULL,
                           tooltipField = NULL,
                           tooltipValueGetter = NULL,
                           tooltipComponent = NULL,
                           tooltipComponentParams = NULL,
                           ...) {
  ag_col_def(x,
    columns = {{ columns }},
    tooltipField = tooltipField,
    tooltipValueGetter = tooltipValueGetter,
    tooltipComponent = tooltipComponent,
    tooltipComponentParams = tooltipComponentParams,
    ...
  )
}

#' Custom colums width
#'
#' More message: <https://www.ag-grid.com/javascript-data-grid/column-properties/#reference-width>
#' @inheritParams ag_col_def
#' @param width Initial width in pixels for the cell.
#' @param initialWidth Same as width, except only applied when creating a new column. Not applied when updating column definitions.
#' @param minWidth Minimum width in pixels for the cell.
#' @param maxWidth Maximum width in pixels for the cell.
#' @param flex Used instead of width when the goal is to fill the remaining empty space of the grid.
#' @param initialFlex Same as flex, except only applied when creating a new column. Not applied when updating column definitions.
#' @param resizable Set to true to allow this column should be resized.
#' @param suppressSizeToFit Set to true if you want this column's width to be fixed during 'size to fit' operations.
#' @param suppressAutoSize Set to true if you do not want this column to be auto-resizable by double clicking it's edge.
#'
#' @seealso [ag_col_def()]
#' @export
ag_col_width <- function(x,
                         columns = NULL,
                         width = NULL,
                         initialWidth = NULL,
                         minWidth = NULL,
                         maxWidth = NULL,
                         flex = NULL,
                         initialFlex = NULL,
                         resizable = NULL,
                         suppressSizeToFit = NULL,
                         suppressAutoSize = NULL,
                         ...) {
  ag_col_def(x,
    columns = {{ columns }},
    width = width,
    initialWidth = initialWidth,
    minWidth = minWidth,
    maxWidth = maxWidth,
    flex = flex,
    initialFlex = initialFlex,
    resizable = resizable,
    suppressSizeToFit = suppressSizeToFit,
    suppressAutoSize = suppressAutoSize,
    ...
  )
}

#' add aggrid header
#'
#' @inheritParams ag_col_def
#'
#' @export

ag_col_group <- function(x,
                         columns,
                         headerName,
                         groupId = NULL,
                         marryChildren = NULL,
                         suppressStickyLabel = NULL,
                         openByDefault = NULL,
                         columnGroupShow = NULL,
                         toolPanelClass = NULL,
                         suppressColumnsToolPanel = NULL,
                         suppressFiltersToolPanel = NULL,
                         suppressSpanHeaderHeight = NULL,
                         tooltipComponent = NULL,
                         tooltipComponentParams = NULL,
                         headerClass = NULL,
                         headerTooltip = NULL,
                         headerGroupComponent = NULL,
                         headerGroupComponentParams = NULL) {
  check_aggrid(x)
  columns <- tidyselect::eval_select(rlang::enquo(columns), data = x$x$data) |>
    names()
  columns <- columns[columns != "rowid"]
  if (length(columns) == 0) {
    cli::cli_abort("Need to specify required columns.")
  }
  if (!is.character(headerName) && length(headerName) != 1) {
    cli::cli_abort("`headerName` must be character and length is 1.")
  }
  args <- list(
    groupId = groupId,
    marryChildren = marryChildren,
    suppressStickyLabel = suppressStickyLabel,
    openByDefault = openByDefault,
    columnGroupShow = columnGroupShow,
    toolPanelClass = toolPanelClass,
    suppressColumnsToolPanel = suppressColumnsToolPanel,
    suppressFiltersToolPanel = suppressFiltersToolPanel,
    suppressSpanHeaderHeight = suppressSpanHeaderHeight,
    tooltipComponent = tooltipComponent,
    tooltipComponentParams = tooltipComponentParams,
    headerName = headerName,
    headerClass = headerClass,
    headerTooltip = headerTooltip,
    headerGroupComponent = headerGroupComponent,
    headerGroupComponentParams = headerGroupComponentParams
  )
  args <- dropNulls(args)
  x$x$group[[headerName]] <- c(list(children = columns), args)
  x
}


#-------------------------------------------------------------------------------
#
# Set aggrid gridOptions
#
#-------------------------------------------------------------------------------

#' Set aggrid gridOptions
#'
#' More options see <https://www.ag-grid.com/javascript-data-grid/grid-options/>
#' @export
ag_gridOptions <- function(x, ...) {
  check_aggrid(x)
  args <- list(...)
  x$x$gridOptions <- modifyList(
    x$x$gridOptions,
    args
  )
  x
}


#-------------------------------------------------------------------------------
#
# Set aggrid Columns format
#
#-------------------------------------------------------------------------------

#' Set aggrid Columns format
#'
#' @inheritParams ag_col_def
#' @param format use [numeraljs](http://numeraljs.com/#custom-formats) format
#' @examples
#' data.frame(
#'   price = c(9603.01, 100, 98322),
#'   percent = c(0.9525556, 0.5, 0.112),
#'   exponential = c(123456789, 0.0001314, 0.52),
#'   bytes = c(1024^3, 1024^2, 1024)
#' ) |>
#'   aggrid() |>
#'   ag_col_format(price, "$0,0.000") |>
#'   ag_col_format(percent, "0.0%") |>
#'   ag_col_format(exponential, "0,0e+0") |>
#'   ag_col_format(bytes, "0 b")
#'
#' # custom valueFormatter
#' data.frame(
#'   Area = c(1000, 200, 40)
#' ) |>
#'   aggrid() |>
#'   ag_col_def(valueFormatter = JS('
#'     (params) => {return params.value + " mi\u00b2";};
#'   '))
#' @export
ag_col_format <- function(x, columns, format) {
  dep <- Numeral_dependency()
  x$dependencies <- unique(append(x$dependencies, list(dep)))
  valueFormatter <- JS(sprintf(
    '(params) => {return numeral(params.value).format("%s");};', format
  ))
  x <- ag_col_def(x, {{ columns }},
    valueFormatter = valueFormatter
  )
  x
}

#-------------------------------------------------------------------------------
#
# Enable aggrid charts
#
#-------------------------------------------------------------------------------

#' Enable aggrid charts
#'
#' @inheritParams ag_col_def
#' @export
enable_charts <- function(x) {
  check_aggrid(x)
  x$x$gridOptions$enableCharts <- TRUE
  if (!isTRUE(x$x$gridOptions$enableRangeSelection)) {
    cli::cli_warn("enable Charts must set enableRangeSelection to `TRUE`.")
    x$x$gridOptions$enableRangeSelection <- TRUE
  }
  x$x$gridOptions$columnDefs[["rowid"]]$chartDataType <- "excluded"

  for (i in seq_along(x$x$data)) {
    name <- colnames(x$x$data)[i]
    type <- class(x$x$data[[i]])
    if (name == "rowid") {
      next
    } else {
      chart_type <- switch(type,
       "numeric" = "series",
       "factor" = "category",
       "character" = "category",
       "integer" = "series",
       "logical" = "category",
       "Date" = "time",
       "excluded"
      )
      x$x$gridOptions$columnDefs[[name]]$chartDataType <- chart_type
    }
  }
  x
}


#-------------------------------------------------------------------------------
#
# sparkline
#
#-------------------------------------------------------------------------------

#' #' @param type
#' ag_sparkline <- function(x,column,type) {
#'
#' }
