#' aggrid Dependencies
#'
#' @importFrom htmltools htmlDependency
#' @export

aggrid_dependency <- function(use_cdn = FALSE, Community = TRUE) {
  if (use_cdn) {
    if (Community) {
      dep <- htmltools::htmlDependency(
        name = "aggrid",
        version = "30.0.2",
        src = c(herf = "https://cdn.jsdelivr.net/npm/ag-grid-community/dist/"),
        script = "ag-grid-community.min.js"
      )
    } else {
      dep <- htmltools::htmlDependency(
        name = "aggrid",
        version = "30.0.2",
        src = c(herf = "https://cdn.jsdelivr.net/npm/ag-grid-enterprise@30.0.2/dist/"),
        script = "ag-grid-enterprise.min.js"
      )
    }
  } else {
    if (Community) {
      dep <- htmltools::htmlDependency(
        name = "aggrid",
        version = "30.0.2",
        src = c(file = "htmlwidgets/lib"),
        script = "ag-grid-community.min.js",
        package = "aggrid"
      )
    } else {
      dep <- htmltools::htmlDependency(
        name = "aggrid",
        version = "30.0.2",
        src = c(file = "htmlwidgets/lib"),
        script = "ag-grid-enterprise.min.js",
        package = "aggrid"
      )
    }
  }
  dep
}
