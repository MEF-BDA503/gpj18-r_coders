#ürün yelpazesi

library(shiny)
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(rsconnect)
library(plyr)

require(devtools)
install_github("rCharts", "ramnathv")

#Download rds files
#get import data
tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Rds/imp_data.rds?raw=true",destfile=tmp,mode = 'wb')
imp_data<-read_rds(tmp)
file.remove(tmp)

imp_data

#get export data
tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Rds/exp_data.rds?raw=true",destfile=tmp,mode = 'wb')
exp_data<-read_rds(tmp)
file.remove(tmp)

exp_data

colnames(imp_data)[which(colnames(imp_data) %in% c("Date") )] <- c("Import_Date")
colnames(exp_data)[which(colnames(exp_data) %in% c("Date") )] <- c("Export_Date")

library("tidyverse")
exp_data_sektor <- exp_data %>%
  select(Sector_Name)%>%distinct()
exp_data

imp_data_sektor <- imp_data %>%
  select(Sector_Name)%>%distinct()
imp_data_sektor


#add second column by regex

exp_data_sektor$First_Word<- word(exp_data_sektor$Sector_Name,1)
imp_data_sektor$First_Word<- word(imp_data_sektor$Sector_Name,1)

exp_data_sektor%>%mutate(Type="Export")
imp_data_sektor%>%mutate(Type="Import")

all_sektors<-union_all(exp_data_sektor, imp_data_sektor)
all_sektors

library(shiny)

library(ggplot2)
#install.packages("treemapify")
library(treemapify)   


#plotOutput("distPlot", hover = "plot_hover", hoverDelay = 0)


u <- shinyUI(fluidPage(
  titlePanel("Choose Metrics"),
  sidebarLayout(position = "left",
                sidebarPanel("Choose for Product Scale",
                             checkboxInput("donum1", "Export", value = T),
                             checkboxInput("donum2", "Import", value = F),
                             checkboxInput("donum3", "All", value = F),
                             sliderInput("wt1","Weight 1",min=1,max=10,value=1),
                             sliderInput("wt2","Weight 2",min=1,max=10,value=1),
                             sliderInput("wt3","Weight 3",min=1,max=10,value=1)
                ),
                
                mainPanel())
  
))





s <- shinyServer(function(input, output) 
{
  
  pt1 <- reactive({
    if (!input$donum1) return(NULL)
    # plot
    treeMapCoordinates <- treemapify(exp_data_sektor,
                                     area = "value",
                                     fill = "First_Word",
                                     label = "id",
                                     group = "First_Word")
    
    treeMapPlot <- ggplotify(treeMapCoordinates) + 
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_brewer(palette = "Dark2")
    
    print(treeMapPlot)
  })
  
  pt2 <- reactive({
    if (!input$donum2) return(NULL)
    treeMapCoordinates <- treemapify(imp_data_sektor,
                                     area = "value",
                                     fill = "parent",
                                     label = "id",
                                     group = "First_Word")
    
    treeMapPlot <- ggplotify(treeMapCoordinates) + 
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_brewer(palette = "Dark2")
    
    print(treeMapPlot)
  })
  pt3 <- reactive({
    treeMapCoordinates <- treemapify(exp_data_sektor,
                                     area = "value",
                                     fill = "First_Word",
                                     label = "id",
                                     group = "parent")
    
    treeMapPlot <- ggplotify(treeMapCoordinates) + 
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_brewer(palette = "Dark2")
    
    print(treeMapPlot)
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
