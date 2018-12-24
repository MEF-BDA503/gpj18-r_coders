4library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(rsconnect)

## Import Analysis

tmp<-tempfile(fileext=".xls")

download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources(Excel)/import_1996_2018.xls?raw=true",mode = 'wb',destfile=tmp)

import_data<-readxl::read_excel(tmp,skip=7,col_names=FALSE)

file.remove(tmp)


#import_data<-readxl::read_excel("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources(Excel)/import_1996_2018.xls",skip=7,col_names=FALSE)
#raw_data<-readxl::read_excel("/Users/macboookair/Desktop/bda/gpj18-r_coders/Data_Sources(Excel)/import_1996_2018.xls",skip=7,col_names=FALSE)

head(import_data)
tail(import_data)

#Define Colnames
colnames(import_data) <- c("Year","Sector_Type_Code","Sector_Name",	"Total_Amount",	"January",	"February",	"March",	"April",	"May",	"June",	"July","August",	"September",	"October"	,"November","December")

import_data %>% drop_na()

select(import_data,everything())

na.omit(import_data)

apply(import_data,1,function(x)any(!is.na(x)))

na.omit(import_data, cols=c("Year", "Sector_Type_Code"))

cols = c(4:15);    
import_data[,cols] = suppressWarnings(apply(import_data[,cols], 2, function(x) as.numeric(as.character(x))));
str(import_data)

import_data %>% select(Sector_Name) %>% mutate(VADiff = import_data$January + import_data$February)


import_data %>% select(Sector_Name) %>% mutate(VADiff = import_data$January + import_data$February + import_data$March ) %>% filter(is.na(VADiff)) %>% distinct()
print("1--------1")
print(import_data)
print("--------")
##

tmp<-tempfile(fileext=".xls")

download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources(Excel)/import_1996_2018.xls?raw=true",mode = 'wb',destfile=tmp)

raw_data<-readxl::read_excel(tmp,skip=7,col_names=FALSE)

file.remove(tmp)

#raw_data<-readxl::read_excel("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources(Excel)/import_1996_2018.xls",skip=7,col_names=FALSE)

head(raw_data)
tail(raw_data)


colnames(raw_data) <- c("Year","Sector_Type_Code","Sector_Name",	"Total_Amount",	"January",	"February",	"March",	"April",	"May",	"June",	"July","August",	"September",	"October"	,"November","December")

raw_data %>% drop_na()

select(raw_data,everything())

apply(raw_data,1,function(x)any(!is.na(x)))

na.omit(raw_data, cols=c("Year", "Sector_Type_Code"))

head(raw_data)

raw_data <- raw_data[rowSums(is.na(raw_data)) != ncol(raw_data),]

as.numeric(raw_data$January)

head(raw_data)

as.numeric(raw_data$January)
as.numeric(raw_data$February)


cols = c(4:15);    
raw_data[,cols] = suppressWarnings(apply(raw_data[,cols], 2, function(x) as.numeric(as.character(x))));
str(raw_data)

raw_data %>% select(Sector_Name) %>% mutate(VADiff = raw_data$January + raw_data$February)

for (row in 1:nrow(raw_data)) {
  year <- raw_data[row, "Year"]
  if(!is.na(year) & year == 2017){
    print(paste("YEAR-ROW",year))
    print(paste("row-i",i))
    break
  }
  raw_data[row, "Year"] <- 2018
}

v_year <- 2017
for (row in 1:nrow(raw_data)) {
  year <- raw_data[row, "Year"]
  if(!is.na(year) & year == v_year){
    print(paste("YEAR-ROW",year))
    print(paste("row-i",i))
    v_year <- v_year - 1
  }
  raw_data[row, "Year"] <- v_year + 1
  if (v_year==2008){
    break
  }
}

exp_data_v2 <- raw_data %>%
  slice(6:391)%>% filter(Sector_Name != "Toplam -Total")
head(exp_data_v2)
tail(exp_data_v2)

Months <- c("January","February","March","April","May","June","July","August","September","October","November","December")

Values <- c(1000,1200,1100,1600,1800,1000,1200,1300,2000,1300,1200,1100)

Randoms <- c(1020,1300,1130,1500,1080,2000,2200,1350,2500,1350,1220,1101)

## UI Part ##

ui <- navbarPage("R Coders",
                 tabPanel("Import/Export Main Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("Number",
                                          "Participants:",
                                          min = 2010,
                                          max = 2018,
                                          value = c(2015),sep ="",step=1),
                              
                              selectInput("exp_data_v2$Sector_Name", label="Kırılımlar", choices = c("All",exp_data_v2$Sector_Name))
                              
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
                 tabPanel("Import/Export Change Over Time"),
                 navbarMenu("More",
                            tabPanel("Import-Details",tableOutput("table_import")),
                            tabPanel("Export-Details",tableOutput("table_export")))
)




## Server Part ##
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    ggplot(exp_data_v2,aes(x=exp_data_v2$Sector_Name,y=exp_data_v2$Total_Amount,color = exp_data_v2$Sector_Type_Code))+geom_point()
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


