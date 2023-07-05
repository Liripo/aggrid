`%||%` <- function(x,y) {
  if (is.null(x))
    y
  else
    x
}

#' Is aggrid Object
#' @export
is.aggrid <- function(x) {
  inherits(x,"aggrid")
}

#' R column class to aggrid filter
#' @source <https://www.ag-grid.com/javascript-data-grid/filtering-overview/>

to_aggrid_filter <- function(type) {
  switch(type,
    "numeric" = "agNumberColumnFilter",
    "factor" = "agSetColumnFilter",
    "character" = "agTextColumnFilter",
    "integer" = "agNumberColumnFilter",
    "logical" = "agSetColumnFilter",
    "Date" = "agDateColumnFilter"
  )
}
