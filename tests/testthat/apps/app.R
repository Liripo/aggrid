library(shiny)
devtools::load_all()
ui <- fluidPage(
  aggridOutput("agid")
)

server <- function(input, output, session) {
  output$agid <- renderAggrid({
    aggrid(iris)
  })
  observe({
    print(input$agid_rows_selected)
  })
}

shinyApp(ui, server)
