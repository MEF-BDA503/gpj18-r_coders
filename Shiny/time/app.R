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
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Rds/imp_data_final.rds?raw=true",destfile=tmp,mode = 'wb')
imp_data_final<-read_rds(tmp)
file.remove(tmp)

imp_data_final

#get export data
tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Rds/exp_data_final.rds?raw=true",destfile=tmp,mode = 'wb')
exp_data_final<-read_rds(tmp)
file.remove(tmp)

exp_data_final

#get export data
tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Rds/exp_data.rds?raw=true",destfile=tmp,mode = 'wb')
exp_data<-read_rds(tmp)
file.remove(tmp)

#get inflation data
tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Rds/Consumer_Inflation.rds?raw=true",destfile=tmp,mode = 'wb')
Inflation_data<-read_rds(tmp)
file.remove(tmp)

#Get $ data
tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Rds/US_Dollar_Montly_Rate.rds?raw=true",destfile=tmp,mode = 'wb')
US_Dollar_data<-read_rds(tmp)
file.remove(tmp)



colnames(imp_data)[which(colnames(imp_data) %in% c("Date") )] <- c("Import_Date")
colnames(exp_data)[which(colnames(exp_data) %in% c("Date") )] <- c("Export_Date")

(imp_data)
head(exp_data)
Inflation_data
US_Dollar_data
imp_data_final
exp_data_final
Export_Import_union_sektor_data

#a nes column type
imp_data_final<- mutate(imp_data_final,Type="Import")
exp_data_final<- mutate(exp_data_final,Type="Export")
Export_Import_union_data <- rbind.fill(imp_data_final,exp_data_final)
print.data.frame(Export_Import_union_sektor_data)
#change column name as amount
Export_Total_Amount
names(Export_Import_union_data)[names(Export_Import_union_data) == "Export_Total_Amount"] <- "Total_Amount"

names(imp_data_final)[names(imp_data_final) == "Export_Total_Amount"] <- "Total_Amount"

names(exp_data_final)[names(exp_data_final) == "Export_Total_Amount"] <- "Total_Amount"


names(imp_data_final)[names(imp_data_final) == "Date"] <- "datadate"

names(exp_data_final)[names(exp_data_final) == "Date"] <- "datadate"

names(Inflation_data)[names(Inflation_data) == "Consumer_Price_Index_Montly_Change_%"] <- "Consumer_Price_Index_Montly_Change"


Export_Import_union_data

imp_data_final
exp_data_final
Inflation_data




library(shiny)


ui <- navbarPage("R Coders",
                 tabPanel("Import/Export Time Analysis",
                          sidebarLayout(
                           sidebarPanel(
                             selectInput("Export_Import_union_sektor_data$Type", label="Choose Type", choices = c("All",Export_Import_union_sektor_data$Type))
                             
                           ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Plot", plotOutput("distPlot")),
                                tabPanel("Summary", verbatimTextOutput("selected_var")),
                                tabPanel("Table", tableOutput("table"))
                              )
                            ))),
                 tabPanel("Import/Export Time Analysis"),
                 navbarMenu("More",
                            tabPanel("Export_Import_union_data",tableOutput("Export_Import_union_data"))
))

library('dplyr')
library(ggplot2)
#install.packages("plotly")
#install.packages("ggplot2")
library(plotly)
packageVersion('plotly')
library(gridExtra)


## Server Part ##
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    

    p = ggplot() + 
      geom_line(data = imp_data_final, aes(x = datadate, y = Total_Amount), color = "blue") +
      geom_line(data = exp_data_final, aes(x = datadate, y = Total_Amount), color = "red") +
      scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) +
     
     
      xlab('datadate') +
      ylab('Total Amount')+
    scale_x_date(date_labels="%b %y",date_breaks  ="3 month")+
    theme_classic() + xlab("Date") + 
   theme(axis.text.x = element_text(face="bold", color="#993333", 
                                           size=12, angle=90),
                axis.text.y = element_text(face="bold", color="#993333", 
                                           size=12, angle=90))
      
    

    
   
    #df <- reshape2::melt(outer(1:4, 1:4), varnames = c("X1", "X2"))
    
#    p1 <- ggplot(df, aes(X1, X2)) + geom_tile(aes(fill = value))
 #   p2 <- p1 + geom_point(aes(size = value))
    
    # Basic form
  #  p1 + scale_fill_continuous(guide = guide_legend())
    


    
  })
  
  output$selected_var <- renderText({
    paste("You have selected",input$Number)
  })
  
  output$table <- renderTable({
    head(import_data, 10)
  })
  
  output$table_import <- renderTable({
    head(import_data, 10)
  })
  
  output$table_export <- renderTable({
    head(import_data, 10)
  })
  
}


# Create Shiny app ----
shinyApp(ui, server)


