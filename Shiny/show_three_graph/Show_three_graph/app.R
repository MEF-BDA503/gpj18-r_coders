library(shiny)
library(ggplot2)
library(gridExtra)

u <- shinyUI(fluidPage(
  titlePanel("title panel"),
  sidebarLayout(position = "left",
                sidebarPanel("sidebar panel",
                             checkboxInput("donum1", "Import", value = T),
                             checkboxInput("donum2", "Export", value = F),
                             checkboxInput("donum3", "Inflation", value = F),
                             sliderInput("wt1","Weight 1",min=1,max=10,value=1),
                             sliderInput("wt2","Weight 2",min=1,max=10,value=1),
                             sliderInput("wt3","Weight 3",min=1,max=10,value=1)
                ),
                mainPanel("main panel",
                          column(6,ggplot() + 
                                   geom_line(data = imp_data_final, aes(x = datadate, y = Total_Amount), color = "blue") +
                                   geom_line(data = exp_data_final, aes(x = datadate, y = Total_Amount), color = "red") +
                                   xlab('datadate') +
                                   ylab('Total Amount')+
                                   scale_x_date(date_labels="%b %y",date_breaks  ="3 month")+
                                   theme_classic() + xlab("Date") + 
                                   theme(axis.text.x = element_text(face="bold", color="#993333", 
                                                                    size=12, angle=90),
                                         axis.text.y = element_text(face="bold", color="#993333", 
                                                                    size=12, angle=90))))
                ))))

s <- shinyServer(function(input, output) 
{
  set.seed(123)
  pt1 <- reactive({
    if (!input$donum1) return(NULL)
    qplot(rnorm(500),fill=I("red"),binwidth=0.2,main="Import")
  })
  pt2 <- reactive({
    if (!input$donum2) return(NULL)
    qplot(rnorm(500),fill=I("blue"),binwidth=0.2,main="Export")
  })
  pt3 <- reactive({
    if (!input$donum3) return(NULL)
    qplot(rnorm(500),fill=I("green"),binwidth=0.2,main="Inflation")
  })
  output$plotgraph = renderPlot({
    ptlist <- list(pt1(),pt2(),pt3())
    wtlist <- c(input$wt1,input$wt2,input$wt3)
    # remove the null plots from ptlist and wtlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    wtlist <- wtlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    
    grid.arrange(grobs=ptlist,widths=wtlist,ncol=length(ptlist))
  })
})

shinyApp(u,s)