#' Resize all column by container width
#'
#' Example from <https://www.ag-grid.com/javascript-data-grid/column-sizing/#resizing-example>
#' @param x aggrid Object
#' @export

size_to_fit <- function(x) {
  x$x$gridOptions$onGridReady <- JS('
    (gridOptions) => {
      gridOptions.api.sizeColumnsToFit();
    }
  ')
  x
}

#' Resize all column by columns content
#' @param x aggrid Object
#' @param skipHeader Enable skip Header?
#'
#' @examples
#'
#' aggrid(iris) |>
#'   auto_size_columns()
#' @export
auto_size_columns <- function(x,skipHeader = FALSE) {
  x$x$gridOptions$onGridReady <- JS(sprintf('
    (gridOptions) => {
      const allColumnIds = [];
      gridOptions.columnApi.getColumns().forEach((column) => {
        allColumnIds.push(column.getId());
      });
      gridOptions.columnApi.autoSizeColumns(allColumnIds, %s);
    }
  ', tolower(skipHeader)))
  x
}
