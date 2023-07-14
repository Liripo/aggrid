#' aggrid Dependencies
#'
#' @importFrom htmltools htmlDependency
#' @export

aggrid_dependency <- function(use_cdn = FALSE, community = TRUE) {
  if (use_cdn) {
    if (community) {
      dep <- htmltools::htmlDependency(
        name = "aggrid",
        version = "30.0.3",
        src = c(href = "https://cdn.jsdelivr.net/npm/ag-grid-community/dist/"),
        script = "ag-grid-community.min.js"
      )
    } else {
      dep <- htmltools::htmlDependency(
        name = "aggrid",
        version = "30.0.3",
        src = c(href = "https://cdn.jsdelivr.net/npm/ag-grid-enterprise@30.0.3/dist/"),
        script = "ag-grid-enterprise.min.js"
      )
    }
  } else {
    if (community) {
      dep <- htmltools::htmlDependency(
        name = "aggrid",
        version = "30.0.3",
        src = c(file = "htmlwidgets/lib"),
        script = "ag-grid-community.min.js",
        package = "aggrid"
      )
    } else {
      dep <- htmltools::htmlDependency(
        name = "aggrid",
        version = "30.0.3",
        src = c(file = "htmlwidgets/lib"),
        script = "ag-grid-enterprise.min.js",
        package = "aggrid"
      )
    }
  }
  dep
}

#' aggrid format Dependencies use Numeral-js
#'
#' @export

Numeral_dependency <- function() {
  htmltools::htmlDependency(
    name = "Numeral",
    version = "2.0.6",
    src = c(file = "htmlwidgets/lib"),
    script = "numeral.min.js",
    package = "aggrid"
  )
}
