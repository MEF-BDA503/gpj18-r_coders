install.packages("readxl")
install.packages("plyr")
install.packages("tidyverse", repos = "https://cran.r-project.org")

library(tidyverse)
library(plyr)
library(janitor)
library("readxl")
library(tidyverse)

setwd("C:\\Users\\birikiPC\\Documents\\GitHub\\gpj18-r_coders")
export_data <- read_excel("export_1996_2018.xls")
import_data <- read_excel("import_1996_2018.xls")

#2 şekilde de rds dosyaları alınabilir.
githubURL_data <- ("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/import_data/Enflasyon_Data.rds")
download.file(githubURL_data,"Enflasyon_Data.rds", method="curl")
enflasyon_data <- readRDS("Enflasyon_Data.rds")

githubURL_data <- ("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/import_data/Consumer_Inflation.rds")
download.file(githubURL_data,"Consumer_Inflation.rds", method="curl")
cons_inf_data <- readRDS("Consumer_Inflation.rds")


tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/import_data/Consumer_Inflation.rds?raw=true",destfile=tmp,mode = 'wb')
cons_inf_data<-read_rds(tmp)
file.remove(tmp)

tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/import_data/Enflasyon_Data?raw=true",destfile=tmp,mode = 'wb')
enflasyon_data<-read_rds(tmp)
file.remove(tmp)

##

colnames(export_data) <- c("Year","Sector_Type_Code","Sector_Name","Total_Amount","January","February","March","April","May","June","July","August","September","October","November","December")
colnames(import_data) <- c("Year","Sector_Type_Code","Sector_Name","Total_Amount","January","February","March","April","May","June","July","August","September","October","November","December")


export_data_without_NA <- export_data[rowSums(is.na(export_data)) != ncol(export_data),]
import_data_without_NA <- export_data[rowSums(is.na(export_data)) != ncol(export_data),]

#analyze data
class(export_data_without_NA)
dim(export_data_without_NA)
summary(export_data_without_NA)

#Total_Amount is not numeric!!!
hist(export_data_without_NA$Total_Amount)

#Drop Total Amount Data
export_data_without_NA_Total <- within(export_data_without_NA, rm(Total_Amount))
#Drop Total Amount Data
import_data_without_NA_Total <- within(import_data_without_NA, rm(Total_Amount))

#Convert char data types to numeric data types, test
str(export_data_without_NA_Total)
x <-suppressWarnings(as.numeric(export_data_without_NA_Total$January))
x
str(x)
sapply(export_data_without_NA_Total, is.numeric) 
hist(as.numeric(export_data_without_NA_Total$January))

#Convert char data types to numeric data types
cols = c(4:15);    
export_data_without_NA_Total[,cols] = suppressWarnings(apply(export_data_without_NA_Total[,cols], 2, function(x) as.numeric(as.character(x))));
import_data_without_NA_Total[,cols] = suppressWarnings(apply(import_data_without_NA_Total[,cols], 2, function(x) as.numeric(as.character(x))));
str(export_data_without_NA_Total)
str(import_data_without_NA_Total)

#Some tests
install.packages("stringr")
library(stringr)

export_data_without_NA_Total$Year <- str_trim(export_data_without_NA_Total$Year)
head(export_data_without_NA_Total)

#outlier detection. -- We don't use it.
out_export_data <- boxplot.stats(export_data_without_NA_Total$January)$out;
out_export_data

#Check for the total number of missing values in the entire data
sum(is.na(export_data_without_NA_Total)) #1586
sum(is.na(import_data_without_NA_Total)) #1586

#Num kolonlardaki NA alanlara 0 atamak icin inceleme. Test
export_data_without_NA_Total(is.na(export_data_without_NA_Total[4:15]))

#Shorter data name
exp_data <- export_data_without_NA_Total
str(exp_data)
imp_data <- import_data_without_NA_Total
str(imp_data)



v_year <- 2017
for (row in 1:nrow(exp_data)) {
  year <- exp_data[row, "Year"]
  if(!is.na(year) & year == v_year){
    print(paste("YEAR-ROW",year))
    print(paste("row-i",row))
    v_year <- v_year - 1
  }
  exp_data[row, "Year"] <- v_year + 1
  if (v_year==2008){
    break
  }
}

v_year <- 2017
for (row in 1:nrow(imp_data)) {
  year <- imp_data[row, "Year"]
  if(!is.na(year) & year == v_year){
    print(paste("YEAR-ROW",year))
    print(paste("row-i",row))
    v_year <- v_year - 1
  }
  imp_data[row, "Year"] <- v_year + 1
  if (v_year==2008){
    break
  }
}



#Toplam kısımları silinmiş data
exp_data_v2 <- exp_data %>%
  slice(6:391)%>% filter(Sector_Name != "Toplam -Total")

imp_data_v2 <- imp_data %>%
  slice(6:391)%>% filter(Sector_Name != "Toplam -Total")

tail(exp_data_v2)
tail(imp_data_v2)

install.packages("tidyverse")
install.packages("dplyr")
library("dplyr")
library("tidyverse")
exp_data_v3 <-
  exp_data_v2 %>%
  gather(key=Month,value=Amount,-Year,-Sector_Type_Code,-Sector_Name)

imp_data_v3 <-
  imp_data_v2 %>%
  gather(key=Month,value=Amount,-Year,-Sector_Type_Code,-Sector_Name)

exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="January","01")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="February","02")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="March","03")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="April","04")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="May","05")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="June","06")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="July","07")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="August","08")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="September","09")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="October","10")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="November","11")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="December","12")


imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="January","01")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="February","02")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="March","03")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="April","04")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="May","05")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="June","06")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="July","07")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="August","08")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="September","09")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="October","10")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="November","11")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="December","12")

exp_data_v4 <- exp_data_v3 %>% mutate(Date = lubridate::as_date(paste(Year, 
                                                                      as.integer(Month), as.integer("01"), sep = "-")))

imp_data_v4 <- imp_data_v3 %>% mutate(Date = lubridate::as_date(paste(Year, 
                                                                      as.integer(Month), as.integer("01"), sep = "-")))

exp_data_v5 <- exp_data_v4 %>% select ( Date, Sector_Type_Code, Sector_Name, Amount)
imp_data_v5 <- imp_data_v4 %>% select ( Date, Sector_Type_Code, Sector_Name, Amount)

str(exp_data_v5)
str(imp_data_v5)




saveRDS(exp_data_v5,file="exp_data.rds")
#export_data_rds <- readRDS("exp_data.rds")
saveRDS(imp_data_v5,file="imp_data.rds")
#import_data_rds <- readRDS("imp_data.rds")

#--------------------------ANALYSIS-----------------------
#Make some analysis
install.packages("tidyverse")
library("tidyverse")

#Bar Chart - Total Export Amount in Monhts
export_data_rds %>% select(January:December) %>% gather(key = Month, 
                                                 value = Exp_Amount) %>% group_by(Month) %>% summarise(Total_Exp_Amount = round(sum(Exp_Amount, 
                                                                                                                                    na.rm = T)),digits = 3) %>% ggplot(data = ., aes(x = Month, y = Total_Exp_Amount, 
                                                                                                                                                                                     fill = Month)) + geom_bar(stat = "identity")	

#Bar Chart - Average Export Amount in Monhts
exp_data %>% select(January:December) %>% gather(key = Month, 
                                                 value = Exp_Amount) %>% group_by(Month) %>% summarise(Avg_Exp_Amount = mean(Exp_Amount, 
                                                                                                                             na.rm = T)) %>% ggplot(data = ., aes(x = Month, y = Avg_Exp_Amount, 
                                                                                                                                                                  fill = Month)) + geom_bar(stat = "identity")	


exp_data_min_mean_max <- summarise_each(group_by(exp_data_v5,Sector_Name),
                                        funs(min(.,na.rm=TRUE),
                                             mean(.,na.rm=TRUE),
                                             max(.,na.rm=TRUE)),
                                        Amount)


#the same result as above
exp_data_min_mean_max_same <- exp_data_v5 %>% 
  group_by(Sector_Name) %>% 
  summarise_each(funs(min(.,na.rm=TRUE), mean(.,na.rm=TRUE), max(.,na.rm=TRUE)),Amount)

#round the mean values and add the sector type codes
exp_data_min_mean_max_same <- exp_data_v5 %>% 
  group_by(Sector_Type_Code,Sector_Name) %>% 
  summarise_each(funs(min(.,na.rm=TRUE), round(mean(.,na.rm=TRUE),digits = 3), max(.,na.rm=TRUE)),Amount)


exp_data_v5 %>% rowwise()%>%
  select(Amount,Date)%>% ggplot(data = ., aes(x = Date, y = Amount, 
                                              color = Date)) + geom_line()

ggplot(data = exp_data_v5, aes(x = 1:nrow(exp_data_v5), 
                               y = Date)) + geom_line()

exp_data_v5 %>% rowwise()%>% 
  select(Amount,Sector_Type_Code)%>% 
  filter(!is.na(Amount) & ((Sector_Type_Code %in% "A")))%>% 
  ggplot(data = ., aes(x = Sector_Type_Code, y = Amount, 
                       color = Sector_Type_Code)) + geom_line()

#Add sub sector type codes
exp_data_v5$Sub_Sector_Type_Code <- exp_data_v5$Sector_Type_Code

