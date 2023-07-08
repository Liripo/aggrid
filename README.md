# aggrid
R interface to javascript ag-grid

## INSTALLATION
You can also choose to install the development version of `aggrid` from GitHub:
```R
devtools::install_github("Liripo/aggrid")
```

## Basic Usage

```R
library(aggrid)
aggrid(iris)
```

<a href="https://liripo.github.io/aggrid/articles/Intro.html"><img src="https://s1.ax1x.com/2023/07/07/pCcGSsg.png" alt="pCcGSsg.png" border="0" /></a>

### pagination

```R
aggrid(iris,pagination = TRUE)
```

<a href="https://liripo.github.io/aggrid/articles/Intro.html"><img src="https://s1.ax1x.com/2023/07/07/pCcGJSK.png" alt="pCcGSsg.png" border="0" /></a>

### select rows with checkbox

```R
aggrid(iris,checkboxSelection = TRUE)
```

<a href="https://liripo.github.io/aggrid/articles/Intro.html"><img src="https://s1.ax1x.com/2023/07/07/pCcGrfP.png" alt="pCcGSsg.png" border="0" /></a>

## Server

For big data, e.g. millions of rows,you should use `server` model.

```R
purrr::map_dfr(seq_len(10000), ~iris) |> 
    aggrid()
```

Although the above code can be used to render within 5~6s, operations such as sorting become slowly. So we best to use `server` model, it only work in shiny.

```R
library(shiny)

ui <- fluidPage(
  aggridOutput("test")
)

server <- function(input, output, session) {
  output$test <- renderAggrid({
    purrr::map_dfr(seq_len(10000), ~iris) |> 
      aggrid(server = T)
  })
  observe({
    print(input$test_rows_selected)
  })
}

shinyApp(ui, server)
```

