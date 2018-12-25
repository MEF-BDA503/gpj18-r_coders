

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


#install.packages("ggvis")
library(ggvis)
library(shiny)

ui <- fluidPage(
  ggvisOutput("plot")
)

server <- function(input, output) {
  
  exp_data_final %>%
    ggvis(datadate, Total_Amount) %>%
    geom_line (data = exp_data_final, aes(x = datadate, y = Total_Amount), color = "red")  %>%
    add_tooltip(function(df) { paste0("Petal.Width: ", df$Petal.Width) }) %>%
    bind_shiny("plot")
}

shinyApp(ui = ui, server = server)
