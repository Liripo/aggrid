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
