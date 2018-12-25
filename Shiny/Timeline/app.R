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




#US_Dollar_Montly_Rate
#Download Raw Data
# Create a temporary file
tmp<-tempfile(fileext=".xlsx")
# Download file from repository to the temp file
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Excel/US_Dollar_Montly_Rate.xlsx?raw=true",mode="wb",destfile=tmp)
# Read that excel file using readxl package's read_excel function. You might need to adjust the parameters (skip, col_names) according to your raw file's format.
raw_data<-readxl::read_excel(tmp,skip=7,col_names=FALSE)
# Remove the temp file
file.remove(tmp)

colnames(raw_data) <- c("Date","Dollar")
US_Dollar_Montly_Rate<- raw_data 

saveRDS(US_Dollar_Montly_Rate, file = "US_Dollar_Montly_Rate.rds")


US_Dollar_Montly_Rate


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





tmp<-tempfile(fileext=".xlsx")
# Download file from repository to the temp file
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Excel/US_Dollar_Montly_Rate.xlsx?raw=true",mode="wb",destfile=tmp)
# Read that excel file using readxl package's read_excel function. You might need to adjust the parameters (skip, col_names) according to your raw file's format.
raw_data<-readxl::read_excel(tmp,skip=7,col_names=FALSE)
# Remove the temp file



colnames(raw_data) <- c("Date","Dollar")
# Now we replace NA values with 0 and label the time period with year and month, so when we merge the data we won't be confused.

saveRDS(US_Dollar_Montly_Rate, file = "US_Dollar_Montly_Rate.rds")



################################

tmp<-tempfile(fileext=".xls")

download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Excel/import_1996_2018.xls?raw=true",mode = 'wb',destfile=tmp)

import_data<-readxl::read_excel(tmp,skip=7,col_names=FALSE)

file.remove(tmp)

#Define Colnames
colnames(import_data) <- c("Year","Sector_Type_Code","Sector_Name",	"Total_Amount",	"January",	"February",	"March",	"April",	"May",	"June",	"July","August",	"September",	"October"	,"November","December")

cols = c(4:15);    
import_data[,cols] = suppressWarnings(apply(import_data[,cols], 2, function(x) as.numeric(as.character(x))));
str(import_data)

print("Find Maximum Values")
print("*******************")
print(import_data %>% select(Sector_Name,January,February,March)) %>% mutate(VATotal = import_data$January + import_data$February + import_data$March) %>% filter(VATotal > 3000000)
print("*******************")

## Print No Import Sectors
import_data %>% select(Sector_Name) %>% mutate(VADiff = import_data$January + import_data$February + import_data$March ) %>% filter(is.na(VADiff)) %>% distinct()
print("1--------1")
print(import_data %>% select(Sector_Name) %>% mutate(VADiff = import_data$January + import_data$February + import_data$March ) %>% filter(is.na(VADiff)) %>% filter(!(is.na(Sector_Name))) %>% distinct())
print("--------")
##

tmp<-tempfile(fileext=".xls")

download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Excel/import_1996_2018.xls?raw=true",mode = 'wb',destfile=tmp)

raw_data<-readxl::read_excel(tmp,skip=7,col_names=FALSE)

file.remove(tmp)

#raw_data<-readxl::read_excel("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources(Excel)/import_1996_2018.xls",skip=7,col_names=FALSE)

colnames(raw_data) <- c("Year","Sector_Type_Code","Sector_Name",	"Total_Amount",	"January",	"February",	"March",	"April",	"May",	"June",	"July","August",	"September",	"October"	,"November","December")

cols = c(4:15);    
raw_data[,cols] = suppressWarnings(apply(raw_data[,cols], 2, function(x) as.numeric(as.character(x))));

raw_data %>% select(Sector_Name) %>% mutate(VADiff = raw_data$January + raw_data$February)

for (row in 1:nrow(raw_data)) {
  year <- raw_data[row, "Year"]
  if(!is.na(year) & year == 2017){
    break
  }
  raw_data[row, "Year"] <- 2018
}

v_year <- 2017
for (row in 1:nrow(raw_data)) {
  year <- raw_data[row, "Year"]
  if(!is.na(year) & year == v_year){
    v_year <- v_year - 1
  }
  raw_data[row, "Year"] <- v_year + 1
  if (v_year==2008){
    break
  }
}

exp_data_v2 <- raw_data %>%
  slice(6:391)%>% filter(Sector_Name != "Toplam -Total")

print(exp_data_v2)

######


tmp<-tempfile(fileext=".xls")

download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Excel/export_1996_2018.xls?raw=true",mode = 'wb',destfile=tmp)

import_data<-readxl::read_excel(tmp,skip=7,col_names=FALSE)

file.remove(tmp)

#Define Colnames
colnames(import_data) <- c("Year","Sector_Type_Code","Sector_Name",	"Total_Amount",	"January",	"February",	"March",	"April",	"May",	"June",	"July","August",	"September",	"October"	,"November","December")

cols = c(4:15);    
import_data[,cols] = suppressWarnings(apply(import_data[,cols], 2, function(x) as.numeric(as.character(x))));
str(import_data)

print("Find Maximum Values")
print("*******************")
print(import_data %>% select(Sector_Name,January,February,March)) %>% mutate(VATotal = import_data$January + import_data$February + import_data$March) %>% filter(VATotal > 3000000)
print("*******************")

## Print No Import Sectors
import_data %>% select(Sector_Name) %>% mutate(VADiff = import_data$January + import_data$February + import_data$March ) %>% filter(is.na(VADiff)) %>% distinct()
print("1--------1")
print(import_data %>% select(Sector_Name) %>% mutate(VADiff = import_data$January + import_data$February + import_data$March ) %>% filter(is.na(VADiff)) %>% filter(!(is.na(Sector_Name))) %>% distinct())
print("--------")
##

tmp<-tempfile(fileext=".xls")

download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Excel/export_1996_2018.xls?raw=true",mode = 'wb',destfile=tmp)

raw_data<-readxl::read_excel(tmp,skip=7,col_names=FALSE)

file.remove(tmp)

#raw_data<-readxl::read_excel("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources(Excel)/import_1996_2018.xls",skip=7,col_names=FALSE)

colnames(raw_data) <- c("Year","Sector_Type_Code","Sector_Name",	"Total_Amount",	"January",	"February",	"March",	"April",	"May",	"June",	"July","August",	"September",	"October"	,"November","December")

cols = c(4:15);    
raw_data[,cols] = suppressWarnings(apply(raw_data[,cols], 2, function(x) as.numeric(as.character(x))));

raw_data %>% select(Sector_Name) %>% mutate(VADiff = raw_data$January + raw_data$February)

for (row in 1:nrow(raw_data)) {
  year <- raw_data[row, "Year"]
  if(!is.na(year) & year == 2017){
    break
  }
  raw_data[row, "Year"] <- 2018
}

v_year <- 2017
for (row in 1:nrow(raw_data)) {
  year <- raw_data[row, "Year"]
  if(!is.na(year) & year == v_year){
    v_year <- v_year - 1
  }
  raw_data[row, "Year"] <- v_year + 1
  if (v_year==2008){
    break
  }
}

exp_data_v2 <- raw_data %>%
  slice(6:391)%>% filter(Sector_Name != "Toplam -Total")

print(exp_data_v2)




################################





library(shiny)

library(ggplot2)
library(gridExtra)

#plotOutput("distPlot", hover = "plot_hover", hoverDelay = 0)


u <- shinyUI(fluidPage(
  titlePanel("Choose Metrics"),
  sidebarLayout(position = "left",
                sidebarPanel("Compare Values",
                             checkboxInput("donum1", "Export", value = T),
                             checkboxInput("donum2", "Import", value = F),
                             checkboxInput("donum3", "All", value = F),
                             sliderInput("wt1","Weight 1",min=1,max=10,value=1),
                             sliderInput("wt2","Weight 2",min=1,max=10,value=1),
                             sliderInput("wt3","Weight 3",min=1,max=10,value=1),
                              sliderInput("DatesMerge",
                                            "Dates:",
                                            min = as.Date("2010-01-01","%Y"),
                                            max = as.Date("2018-12-01","%Y"),
                                            value=as.Date("2018-12-01"),
                                            timeFormat="%Y")
                ),
                
                mainPanel(column=100,(plotOutput(outputId="plotgraph", width="900",height="600px"))))
  
))







s <- shinyServer(function(input, output) 
{
  set.seed(600)
  
  pt1 <- reactive({
    if (!input$donum1) return(NULL) and [month(datadate) == month(DatesMerge)]
    ggplot() + 
      geom_smooth(data = exp_data_final, aes(x = datadate, y = Total_Amount), color = "blue")+
      scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) +
      xlab('datadate') +
      ylab('Total Amount')+
      scale_x_date(date_labels="%b %y",date_breaks  ="3 month")+
      theme_classic() + xlab("Date") + 
      theme(axis.text.x = element_text(face="bold", color="black", 
                                       size=12, angle=90),
            axis.text.y = element_text(face="bold", color="black", 
                                       size=12, angle=0))
    
    
    
  })
  
  pt2 <- reactive({
    if (!input$donum2 ) return(NULL)
    
    ggplot() + 
      geom_smooth(data = imp_data_final, aes(x = datadate, y = Total_Amount), color = "red")+
      scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) +
      xlab('datadate') +
      ylab('Total Amount')+
      scale_x_date(date_labels="%b %y",date_breaks  ="3 month")+
      theme_classic() + xlab("Date") + 
      theme(axis.text.x = element_text(face="bold", color="black", 
                                       size=12, angle=90),
            axis.text.y = element_text(face="bold", color="black", 
                                       size=12, angle=90))
    
  })
  pt3 <- reactive({
    if (!input$donum3) return(NULL)
      ggplot() + 
        geom_smooth(data = imp_data_final, aes(x = datadate, y = Total_Amount), color = "blue") +
        geom_smooth(data = exp_data_final, aes(x = datadate, y = Total_Amount), color = "red") +
      scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) +
      
      
      xlab('datadate') +
      ylab('Total Amount')+
      scale_x_date(date_labels="%b %y",date_breaks  ="3 month")+
      theme_classic() + xlab("Date") + 
      theme(axis.text.x = element_text(face="bold", color="black", 
                                       size=12, angle=90),
            axis.text.y = element_text(face="bold", color="black", 
                                       size=12, angle=90))
    
    
    
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



