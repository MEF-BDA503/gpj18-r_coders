library(shiny)
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(rsconnect)


#Download rds files
#get import data
tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources(Rds)/imp_data.rds?raw=true",destfile=tmp,mode = 'wb')
imp_data<-read_rds(tmp)
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


(imp_data)
head(exp_data)
Inflation_data
US_Dollar_data

#import_export_data<- inner_join(exp_data,imp_data, by=c("Date" = "Date"))
#Need union all
library(plyr)
#export_import_union_data <- rbind.fill(exp_data,imp_data)
#.-saveRDS(import_export_data, file = "import_export_data.rds")

Sector_Name_Export<-exp_data %>% select(Sector_Name) %>% distinct 
Sector_Name_Export<- mutate(Sector_Name_export,Type="Export")
Sector_Name_Import<-imp_data %>% select(Sector_Name) %>% distinct 
Sector_Name_Import<- mutate(Sector_Name_Import,Type="Import")
Export_Import_union_sektor_data <- rbind.fill(Sector_Name_Export,Sector_Name_Import)
print(Sector_Name)
#print all columns
print.data.frame(Sector_Name_Export)
print.data.frame(Sector_Name_Import)
print.data.frame(Export_Import_union_sektor_data)




## UI Part ##
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("memoise")
#install.packages("NLP")
library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(NLP)

ui <- navbarPage("R Coders",
                 tabPanel("Import/Export Time Analysis",
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

