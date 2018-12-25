#same panel three graphs

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

names(Inflation_data)[names(Inflation_data) == "Consumer_Price_Index_Montly_Change_%"] <- "Consumer_Price_Index"

names(Inflation_data)[names(Inflation_data) == "Consumer_Price_Index_Yearly_Change_%"] <- "Consumer_Price_Index_Yearly_Change"

Export_Import_union_data

imp_data_final
exp_data_final
str(Inflation_data)
Inflation_data


#exp_data_final<-qplot(data = exp_data_final, aes(x = datadate, y = Total_Amount), color = "red") 

#imp_data_final<-geom_line(data = imp_data_final, aes(x = datadate, y = Total_Amount), color = "blue") 
#Inflation_data<-geom_line(data = Inflation_data, aes(x = Date, y = Consumer_Price_Index_Montly_Change), color = "green") 


library(shiny)


#plotOutput("distPlot", hover = "plot_hover", hoverDelay = 0)


u <- shinyUI(fluidPage(
  titlePanel("Choose Metrics"),
  sidebarLayout(position = "center",
                sidebarPanel("Compare Values",
                             checkboxInput("donum1", "Export", value = T),
                             checkboxInput("donum2", "Import", value = F),
                             checkboxInput("donum3", "Inflatıon", value = F),
                             sliderInput("wt1","Weight 1",min=1,max=10,value=1),
                             sliderInput("wt2","Weight 2",min=1,max=10,value=1),
                             sliderInput("wt3","Weight 3",min=1,max=10,value=1)),
                
                mainPanel(column(6,plotOutput(outputId="plotgraph", width="600px",height="500px"))
                ))))



s <- shinyServer(function(input, output) 
{

  
  set.seed(123)
  pt1 <- reactive({
    if (!input$donum1) return(NULL)
    qplot(datadate, Total_Amount, data=exp_data_final, geom="area",fill=I("lightblue"),binwidth=0.2,main="Export Trend By Time",xlab="Date", ylab='Amount') 
    })

  pt2 <- reactive({
    if (!input$donum2) return(NULL)
    qplot(datadate, Total_Amount, data=imp_data_final, geom="area",fill=I("red"),binwidth=0.2,main="Export Trend By Time") 
  })
  pt3 <- reactive({
    if (!input$donum3) return(NULL)
    qplot(Date, Consumer_Price_Index_Yearly_Change, data=Inflation_data, geom="area",fill=I("darkblue"),binwidth=0.2,main="Inflation Trend By Time") 
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
