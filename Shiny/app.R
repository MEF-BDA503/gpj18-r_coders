library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(rsconnect)
library(plotly)
library(gapminder)
library(gridExtra)

## Import Analysis

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

Months <- c("January","February","March","April","May","June","July","August","September","October","November","December")

Values <- c(1000,1200,1100,1600,1800,1000,1200,1300,2000,1300,1200,1100)

Randoms <- c(1020,1300,1130,1500,1080,2000,2200,1350,2500,1350,1220,1101)

## Import/ Export Union Part

tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Rds/imp_data_final.rds?raw=true?raw=true",destfile=tmp,mode = 'wb')
imp_data_final <- read_rds(tmp)
file.remove(tmp)

tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Rds/exp_data_final.rds?raw=true?raw=true",destfile=tmp,mode = 'wb')
exp_data_final<-read_rds(tmp)
file.remove(tmp)

tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Rds/imp_data.rds?raw=true?raw=true",destfile=tmp,mode = 'wb')
imp_data<-read_rds(tmp)
file.remove(tmp)

tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Rds/exp_data.rds?raw=true?raw=true",destfile=tmp,mode = 'wb')
exp_data<-read_rds(tmp)
file.remove(tmp)

tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Rds/Producer_Inflation.rds?raw=true?raw=true",destfile=tmp,mode = 'wb')
producer_inf<-read_rds(tmp)
file.remove(tmp)

# Create a temporary file
tmp=tempfile(fileext=".xls")
# Download file from repository to the temp file
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Excel/export_import_sectors.xls?raw=true",destfile=tmp,mode='wb')
# Read that excel file.
sectors <- read_excel(tmp)
# Remove the temp file
file.remove(tmp)

tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Rds/US_Dollar_Montly_Rate.rds?raw=true?raw=true",destfile=tmp,mode = 'wb')
usd_rate<-read_rds(tmp)
file.remove(tmp)

names(exp_data_final)[names(exp_data_final) == 'Date'] <- 'Export_Date'
names(exp_data)[names(exp_data) == 'Date'] <- 'Export_Date'
names(imp_data_final)[names(imp_data_final) == 'Date'] <- 'Import_Date'
names(imp_data_final)[names(imp_data_final) == 'Export_Total_Amount'] <- 'Import_Total_Amount' #fix
names(imp_data)[names(imp_data) == 'Date'] <- 'Import_Date'

exp_data <- inner_join(exp_data,sectors, by=c("Sector_Type_Code"="Sub_Sector_Type_Code"))

imp_data <- inner_join(imp_data,sectors, by=c("Sector_Type_Code"="Sub_Sector_Type_Code"))

exp_data$Export_Year<-format(exp_data$Export_Date,"%Y")
exp_data$Export_Year_Month<-format(exp_data$Export_Date,"%Y-%m")
exp_data_final$Export_Year<-format(exp_data_final$Export_Date,"%Y")
exp_data_final$Export_Year_Month<-format(exp_data_final$Export_Date,"%Y-%m")

imp_data$Import_Year<-format(imp_data$Import_Date,"%Y")
imp_data$Import_Year_Month<-format(imp_data$Import_Date,"%Y-%m")
imp_data_final$Import_Year<-format(imp_data_final$Import_Date,"%Y")
imp_data_final$Import_Year_Month<-format(imp_data_final$Import_Date,"%Y-%m")

imp_data<- imp_data %>%
  select (Import_Date,Sector_Type_Code,Sector_Type_Code.y,Main_Sector_Flag,Sector_Name_Eng,Amount,Import_Year,Import_Year_Month)

exp_data<- exp_data %>%
  select (Export_Date,Sector_Type_Code,Sector_Type_Code.y,Main_Sector_Flag,Sector_Name_Eng,Amount,Export_Year,Export_Year_Month)  


colnames(imp_data)[colnames(imp_data) == 'Amount'] <- 'Import_Amount'
colnames(exp_data)[colnames(exp_data) == 'Amount'] <- 'Export_Amount'
colnames(imp_data)[colnames(imp_data) == 'Sector_Type_Code'] <- 'Sub_Sector_Type_Code'
colnames(exp_data)[colnames(exp_data) == 'Sector_Type_Code'] <- 'Sub_Sector_Type_Code'
colnames(imp_data)[colnames(imp_data) == 'Sector_Type_Code.y'] <- 'Sector_Type_Code'
colnames(exp_data)[colnames(exp_data) == 'Sector_Type_Code.y'] <- 'Sector_Type_Code'
imp_data$Import_Amount[is.na(imp_data$Import_Amount)] <- 0
imp_data_final$Import_Total_Amount[is.na(imp_data_final$Import_Total_Amount)] <- 0
exp_data$Export_Amount[is.na(exp_data$Export_Amount)] <- 0
exp_data_final$Export_Total_Amount[is.na(exp_data_final$Export_Total_Amount)] <- 0

exp_data_final <- exp_data_final %>%
  filter(Export_Date<'2018-11-01')

exp_data <- exp_data %>%
  filter(Export_Date<'2018-11-01')

imp_data_final <- imp_data_final %>%
  filter(Import_Date<'2018-11-01')

imp_data <- imp_data %>%
  filter(Import_Date<'2018-11-01')

saveRDS(imp_data,file="imp_data_v2.rds")
saveRDS(imp_data_final,file="imp_data_final_v2.rds")
saveRDS(exp_data,file="exp_data_v2.rds")
saveRDS(exp_data_final,file="exp_data_final_v2.rds")

imp_and_exp_data <- inner_join(exp_data, imp_data, by=c("Export_Date" = "Import_Date","Sub_Sector_Type_Code"="Sub_Sector_Type_Code"))

imp_and_exp_data_bymonth <- aggregate(cbind(Import_Amount, Export_Amount) ~ Export_Date, data = imp_and_exp_data, sum)
imp_and_exp_data_bymonth <- gather(imp_and_exp_data_bymonth,
                                   value = "value",
                                   key = "type",
                                   Export_Amount, Import_Amount)

imd_data_final_2 <- imp_data_final
#Rename column names
colnames(imp_and_exp_data_bymonth) <- c("Date","Type","Amount")

#Remove Empty Dates
imp_and_exp_data_bymonth <- imp_and_exp_data_bymonth %>%
  filter(Date<'2018-11-01')

df = data.frame("brand" = c("Tarım ve ormancılık","Tarım ve hayvancılık","Ormancılık ve tomrukçuluk","Balıkçılık","Madencilik ve taşocakçılığı","Maden kömürü, linyit"),
                "share" = c(.2090,.1580,.1210,.0930,.0860,.3320))

##--------------##
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

#Get export data
tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Rds/exp_data.rds?raw=true",destfile=tmp,mode = 'wb')
exp_data<-read_rds(tmp)
file.remove(tmp)

#Get inflation data
tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources_Rds/Consumer_Inflation.rds?raw=true",destfile=tmp,mode = 'wb')
Inflation_data<-read_rds(tmp)
file.remove(tmp)

colnames(imp_data)[which(colnames(imp_data) %in% c("Date") )] <- c("Import_Date")
colnames(exp_data)[which(colnames(exp_data) %in% c("Date") )] <- c("Export_Date")

#a nes column type
imp_data_final<- mutate(imp_data_final,Type="Import")
exp_data_final<- mutate(exp_data_final,Type="Export")
Export_Import_union_data <- rbind.fill(imp_data_final,exp_data_final)

names(imp_data_final)[names(imp_data_final) == "Date"] <- "datadate"

names(exp_data_final)[names(exp_data_final) == "Date"] <- "datadate"

## UI Part ##

ui <- navbarPage("R Coders",
                 tabPanel("Import/Export Main Analysis",
                          sidebarLayout(position = "left",
                                        sidebarPanel("Compare Values",
                                                     checkboxInput("donum1", "Export", value = T),
                                                     checkboxInput("donum2", "Import", value = F),
                                                     checkboxInput("donum3", "Inflatıon", value = F),
                                                     sliderInput("wt1","Weight 1",min=1,max=10,value=1),
                                                     sliderInput("wt2","Weight 2",min=1,max=10,value=1),
                                                     sliderInput("wt3","Weight 3",min=1,max=10,value=1)
                                        ),
                                        mainPanel((plotOutput(outputId="plotgraph", width="900",height="600px"))))
                 ),
                 tabPanel("Product Based Analysis",
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
                                tabPanel("Plot", plotOutput("distPlot"),h6("Episode IV", align = "center"),
                                         h4("Rebel spaceships, striking", align = "center"),
                                         h3("from a hidden base, have won", align = "center")),
                                tabPanel("Summary", verbatimTextOutput("selected_var"),verbatimTextOutput("summary")),
                                tabPanel("Table", tableOutput("table"))
                              )
                            ))),
                 tabPanel("Import/Export Change Over Time",mainPanel(tabsetPanel(tabPanel("Plot1",plotOutput("importExportPlot")),
                                                                                 tabPanel("Plot2",plotOutput("ExpoloratoryPlot")),
                                                                                 tabPanel("Plot3",plotOutput("UsdRatePlot"))))),
                 tabPanel("Pie Chart",plotOutput("pieChart")),
                 navbarMenu("More",
                            tabPanel("Import-Details",tabsetPanel(tableOutput("table_import"))),
                            tabPanel("Export-Details",tableOutput("table_export")))
)




## Server Part ##
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    ggplot(exp_data_v2,aes(x=exp_data_v2$Sector_Type_Code,y=exp_data_v2$Total_Amount,color = exp_data_v2$Sector_Type_Code))+geom_point()+theme(axis.text.x = element_text(angle = 60, hjust = 1))
  })
  
  output$importExportPlot <- renderPlot({
    ggplot(imp_and_exp_data_bymonth,
           aes(x=Date,
               y=Amount,
               color=Type)) +
      geom_line()+
      scale_size_area("Nitrogen") + 
      xlab("Import/Export Date") +
      ylab("Amount(1000$)") +
      ggtitle("Import & Export Amount")
  })
  
  output$ExpoloratoryPlot <- renderPlot({
    ggplot(exp_data_final,aes(x=USD_Rate, y = Export_Total_Amount, size = Consumer_Price_Index_Yearly_Change, color=datadate)) +
      geom_point() +
      scale_x_log10() +
      theme_bw()+
      scale_size_area("Nitrogen") + 
      xlab("USD Rate") +
      ylab("Export Amount(1000$)") +
      ggtitle("Export Amounts and Consumer Price Index")
  })
  
  output$pieChart <- renderPlot({
    ggplot(df, aes(x="", y=share, fill=brand)) + geom_bar(stat="identity", width=1) 
  })
  ##pie = ggplot(df, aes(x="", y=share, fill=brand)) + geom_bar(stat="identity", width=1)
  
  output$UsdRatePlot <- renderPlot({
    ggplot(imd_data_final_2,aes(x = USD_Rate, y = Import_Total_Amount, size = Consumer_Price_Index_Yearly_Change, color=Import_Year)) +
      geom_point() +
      scale_x_log10() +
      theme_bw()+
      scale_size_area("Nitrogen") + 
      xlab("USD Rate") +
      ylab("Import Amount(1000$)") +
      ggtitle("Import Amounts and Consumer Price Index")
  })
  
  output$selected_var <- renderText({
    paste("You have selected",input$Number)
  })
  
  output$table <- renderTable({
    head(import_data %>% select(Sector_Name,Total_Amount), 10)
  })
  
  output$summary <- renderPrint({
    dataset <- import_data %>% select(Sector_Name,Sector_Type_Code)
    summary(dataset)
  })
  
  output$table_import <- renderTable({
    head(import_data %>% select(Sector_Name) %>% mutate(VADiff = import_data$January + import_data$February + import_data$March ) %>% filter(is.na(VADiff)) %>% distinct() %>%  filter(!(is.na(Sector_Name))), 10)
  })
  
  output$table_export <- renderTable({
    head(import_data %>% select(Sector_Name) %>% mutate(VADiff = import_data$January + import_data$February) %>% filter(VADiff>1000000 & Sector_Name != 'Toplam -Total') %>% distinct(Sector_Name), 10)
  })
  
  set.seed(600)
  
  pt1 <- reactive({
    if (!input$donum1) return(NULL)
    qplot(datadate, Export_Total_Amount, data=exp_data_final, geom="area",fill=I("lightblue"),binwidth=0.2,main="Export Trend By Time",xlab="Date", ylab='Amount') 
  })
  
  pt2 <- reactive({
    if (!input$donum2) return(NULL)
    qplot(datadate, Import_Total_Amount, data=imp_data_final, geom="area",fill=I("red"),binwidth=0.2,main="Export Trend By Time",xlab="Date", ylab='Amount') 
  })
  pt3 <- reactive({
    if (!input$donum3) return(NULL)
    qplot(Date, Consumer_Price_Index_Yearly_Change, data=Inflation_data, geom="area",fill=I("darkblue"),binwidth=0.2,main="Inflation Trend By Time",xlab="Date", ylab='Consumer_Price_Index_Yearly_Change') 
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
}

# Create Shiny app ----
shinyApp(ui, server)
