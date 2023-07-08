`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

dropNulls <- function(lst) {
  base::Filter(Negate(is.null), lst)
}

date_comparator <- function() {
  JS("(filterLocalDateAtMidnight, cellValue) => {
    if (cellValue == null) {
      return 0;
    }
    var cellDate = new Date(Date.parse(cellValue));
    cellDate = new Date(cellDate.setHours(0,0,0,0));
    if (filterLocalDateAtMidnight.getTime() === cellDate.getTime()) {
      return 0;
    }

    if (cellDate < filterLocalDateAtMidnight) {
       return -1;
    } else if (cellDate > filterLocalDateAtMidnight) {
       return 1;
    }
    return 0;
  };")
}

#' Is aggrid Object
#' @export
is.aggrid <- function(x) {
  inherits(x, "aggrid")
}

check_aggrid <- function(x) {
  if (!is.aggrid(x)) {
    stop("`x` must be aggrid class.", call. = F)
  }
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

#' filter model to R expr

filter_to_expr <- function(column, filterModel) {
  # only one operator
  if (!("conditions" %in% names(filterModel))) {
    filter_items <- list(filterModel)
  } else {
    filter_items <- purrr::transpose(filterModel$conditions)
  }
  Mapper_fun <- switch(filterModel$filterType,
    "number" = numberFilterMapper,
    "set" = setFilterMapper,
    "text" = textFilterMapper,
    "date" = dateFilterMapper,
    stop("unkown filterType", filterModel$filterType)
  )
  filter_exprs <- purrr::map_chr(filter_items, function(item) {
    Mapper_fun(column, item)
  })
  if (!is.null(filterModel$operator)) {
    if (filterModel$operator == "AND") {
      filter_exprs <- paste0(filter_exprs, collapse = " & ")
    } else if (filterModel$operator == "OR") {
      filter_exprs <- paste0(filter_exprs, collapse = " | ")
    } else {
      stop("unknown operator", filterModel$operator)
    }
  }
  filter_exprs
}


numberFilterMapper <- function(column, item) {
  switch(item$type,
    "equals" = glue("{column} == {item$filter}"),
    "notEqual" = glue("{column} != {item$filter}"),
    "greaterThan" = glue("{column} > {item$filter}"),
    "greaterThanOrEqual" = glue("{column} >= {item$filter}"),
    "lessThan" = glue("{column} < {item$filter}"),
    "lessThanOrEqual" = glue("{column} <= {item$filter}"),
    "inRange" = glue("{column} >= {item$filter} & {column} <= {item$filterTo}"),
    "blank" = glue("is.na({column})"),
    "notBlank" = glue("!is.na({column})"),
    stop("unknown number filter type: ", item$type)
  )
}

setFilterMapper <- function(column, item) {
  if (length(item$values) == 1) {
    glue('{column} %in% "{item$values}"')
  } else {
    glue("{column} %in% {as.character(list(item$values))}")
  }
}

textFilterMapper <- function(column, item) {
  switch(item$type,
    "equals" = glue("{column} == '{item$filter}'"),
    "notEqual" = glue("{column} != '{item$filter}'"),
    "contains" = glue("grepl('{item$filter}',{column})"),
    "notContains" = glue("!grepl('{item$filter}',{column})"),
    "startsWith" = glue("grepl('^{item$filter}',{column})"),
    "endsWith" = glue("grepl('{item$filter}$',{column})"),
    "blank" = glue("is.na({column}) & {column} == ''"),
    "notBlank" = glue("!is.na({column}) & {column} != ''"),
    stop("unknown number filter type: ", item$type)
  )
}

dateFilterMapper <- function(column, item) {
  item$dateFrom <- as.Date(item$dateFrom)
  item$dateTo <- as.Date(item$dateTo)
  switch(item$type,
    "equals" = glue("{column} == '{item$dateFrom}'"),
    "notEqual" = glue("{column} != '{item$dateFrom}'"),
    "greaterThan" = glue("{column} > '{item$dateFrom}'"),
    "lessThan" = glue("{column} < '{item$dateFrom}'"),
    "inRange" = glue("{column} >= '{item$dateFrom}' & {column} <= '{item$dateTo}'"),
    "blank" = glue("is.na({column})"),
    "notBlank" = glue("!is.na({column})"),
    stop("unknown number filter type: ", item$type)
  )
}
