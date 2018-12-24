library(shiny)
library(DT)
shinyApp(
  ui = fluidPage(DT::dataTableOutput('tableId'),
                 textOutput("celltext")),
  server = function(input, output) {
    output$tableId = DT::renderDataTable(
      imp_data_final, selection = list(target = 'cell')
    )
    
    output$celltext <- renderText({
      cell <- input$tableId_cells_selected
      imp_data_final <- imp_data_final[cell]
    })
  }
)
