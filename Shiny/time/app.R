library(shiny)
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(rsconnect)
library(plyr)

#Download rds files
#get import data
tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources(Rds)/imp_data.rds?raw=true",destfile=tmp,mode = 'wb')
imp_data<-read_rds(tmp)
file.remove(tmp)

tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources(Rds)/imp_data_final.rds?raw=true",destfile=tmp,mode = 'wb')
imp_data_final<-read_rds(tmp)
file.remove(tmp)

tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources(Rds)/exp_data_final.rds?raw=true",destfile=tmp,mode = 'wb')
exp_data_final<-read_rds(tmp)
file.remove(tmp)


#get export data
tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources(Rds)/exp_data.rds?raw=true",destfile=tmp,mode = 'wb')
exp_data<-read_rds(tmp)
file.remove(tmp)

#get inflation data
tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources(Rds)/Consumer_Inflation.rds?raw=true",destfile=tmp,mode = 'wb')
Inflation_data<-read_rds(tmp)
file.remove(tmp)

#Get $ data
tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources(Rds)/US_Dollar_Montly_Rate.rds?raw=true",destfile=tmp,mode = 'wb')
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

Export_Import_union_data

library(shiny)
#library(tm)
#library(wordcloud)
#library(memoise)
#library(NLP)

ui <- navbarPage("R Coders",
                 tabPanel("Import/Export Time Analysis",
                          sidebarLayout(
                            sidebarPanel(
                             selectInput("Export_Import_union_sektor_data$Type", label="Choose Type", choices = c("All",Export_Import_union_sektor_data$Type))
                              
                              #sliderInput("votes","Min Votes",min=min(shiny_movie_set$votes),max=max(shiny_movie_set$votes),value = min(shiny_movie_set$votes))
                              # Show a plot of the generated distribution
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





## Server Part ##
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # Line plot with multiple groups
      ggplot(data=Export_Import_union_data, aes(x=Date, y=Total_Amount, group=Date)) +
      geom_line()+
      geom_point()
    # Change line types
    ggplot(data=Export_Import_union_data, aes(x=Date, y=Total_Amount, group=Date)) +
      geom_line(linetype="dashed", color="blue", size=1.2)+
      geom_point(color="red", size=3)
    
   
     
    
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


