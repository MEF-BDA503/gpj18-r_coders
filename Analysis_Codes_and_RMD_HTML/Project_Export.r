install.packages("readxl")
install.packages("plyr")
install.packages("tidyverse", repos = "https://cran.r-project.org")

library(tidyverse)
library(plyr)
library(janitor)
library("readxl")

setwd("C:\\Users\\birikiPC\\Documents\\GitHub\\gpj18-r_coders")
export_data <- read_excel("export_1996_2018.xls")
#import_data <- read_excel("import_1996_2018.xls")

colnames(export_data) <- c("Year","Sector_Type_Code","Sector_Name","Total_Amount","January","February","March","April","May","June","July","August","September","October","November","December")

#export_data3 <- Filter(function(x) !all(is.na(x)), export_data)
#export_data3 <- export_data[ , ! apply(export_data , 2 , function(x) all(is.na(x)) ) ]

#install.packages("janitor")
#library(janitor)
#export_data3 <- remove_empty_cols(export_data)
#help("Deprecated")

#row.has.na <- apply(export_data, 1, function(x){any(is.na(x))})
export_data_without_NA <- export_data[rowSums(is.na(export_data)) != ncol(export_data),]

class(export_data_without_NA)
dim(export_data_without_NA)
summary(export_data_without_NA)

#Total_Amount is not numeric!!!
hist(export_data_without_NA$Total_Amount)

#Drop Total Amount Data
export_data_without_NA_Total <- within(export_data_without_NA, rm(Total_Amount))

str(export_data_without_NA_Total)
x <-suppressWarnings(as.numeric(export_data_without_NA_Total$January))
x
str(x)
sapply(export_data_without_NA_Total, is.numeric) 
hist(as.numeric(export_data_without_NA_Total$January))

cols = c(4:15);    
export_data_without_NA_Total[,cols] = suppressWarnings(apply(export_data_without_NA_Total[,cols], 2, function(x) as.numeric(as.character(x))));
str(export_data_without_NA_Total)

export_data_without_NA_Total

install.packages("stringr")
library(stringr)

export_data_without_NA_Total$Year <- str_trim(export_data_without_NA_Total$Year)
head(export_data_without_NA_Total)

#outlier detection. -- We don't use it.
out_export_data <- boxplot.stats(export_data_without_NA_Total$January)$out;
out_export_data

#Check for the total number of missing values in the entire data
sum(is.na(export_data_without_NA_Total)) #1586

#Num kolonlardaki NA alanlara 0 atamak için inceleme. Çalışmadı
export_data_without_NA_Total(is.na(export_data_without_NA_Total[4:15]))

#Shorter data name
exp_data <- export_data_without_NA_Total
str(exp_data)

install.packages("dplyr")
library(dplyr)

v_year <- 2017
for (row in 1:nrow(exp_data)) {
  year <- exp_data[row, "Year"]
  if(!is.na(year) & year == v_year){
    print(paste("YEAR-ROW",year))
    print(paste("row-i",i))
    v_year <- v_year - 1
  }
  exp_data[row, "Year"] <- v_year + 1
  if (v_year==2008){
    break
  }
}

exp_data %>% select(Sector_Name) %>% mutate(VADiff = exp_data$January + exp_data$February) %>% filter(is.na(VADiff)) %>% distinct()

str(exp_data)

test <- group_by(exp_data, Sector_Type_Code,Sector_Name)
test

# To get the number of flights per day
per_sector <- summarize(exp_data, Sector_Name = n())
per_sector

exp_sectors_analyze <- exp_data %>% 
  select(Sector_Type_Code,Sector_Name,January)%>% 
  group_by(Sector_Type_Code,Sector_Name) %>% 
  summarise(Count_Jan = count(January, na.rm = T))

library(dplyr)
exp_data_sectors <- exp_data %>% 
  select(Sector_Name,January) %>%
  group_by(Sector_Name) %>%
  filter (!is.na(January))%>%
  summarise(Sector_Count= n(), na.rm = T)

exp_data_sectors
summary(rlang::last_error())

exp_sectors_analyze

exp_sectors <- exp_data %>% select(Sector_Type_Code,Sector_Name)%>% distinct()


#Bar Chart - Total Export Amount in Monhts
exp_data %>% select(January:December) %>% gather(key = Month, 
                                                 value = Exp_Amount) %>% group_by(Month) %>% summarise(Total_Exp_Amount = sum(Exp_Amount, 
                                                                                                                              na.rm = T)) %>% ggplot(data = ., aes(x = Month, y = Total_Exp_Amount, 
                                                                                                                                                                   fill = Month)) + geom_bar(stat = "identity")	

#Bar Chart - Average Export Amount in Monhts
exp_data %>% select(January:December) %>% gather(key = Month, 
                                                 value = Exp_Amount) %>% group_by(Month) %>% summarise(Avg_Exp_Amount = mean(Exp_Amount, 
                                                                                                                             na.rm = T)) %>% ggplot(data = ., aes(x = Month, y = Avg_Exp_Amount, 
                                                                                                                                                                  fill = Month)) + geom_bar(stat = "identity")	


#Toplam k�s�mlar� silinmi� data
exp_data_v2 <- exp_data %>%
  slice(6:391)%>% filter(Sector_Name != "Toplam -Total")

tail(exp_data_v2)

install.packages("tidyverse")
install.packages("dplyr")
library("dplyr")
library("tidyverse")
exp_data_v3 <-
  exp_data_v2 %>%
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



exp_data_v4 <- exp_data_v3 %>% mutate(Date = lubridate::as_date(paste(Year, 
                                                                      as.integer(Month), as.integer("01"), sep = "-")))

exp_data_v5 <- exp_data_v4 %>% select ( Date, Sector_Type_Code, Sector_Name, Amount)

str(exp_data_v5)

#Make some analysis
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

setwd("C:\\Users\\birikiPC\\Documents\\GitHub\\gpj18-r_coders")
library(tidyverse)
enflasyon_data<-readRDS(file = "C:\\Users\\birikiPC\\Documents\\GitHub\\gpj18-r_coders\\Enflasyon_Data.rds")

#Or load from github
githubURL_enflasyon <- ("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/import_data/Enflasyon_Data.rds")
download.file(githubURL_enflasyon,"Enflasyon_Data.rds", method="curl")
enflasyon_data <- readRDS("Enflasyon_Data.rds")


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

v_Sector_Type_Code <- "A"
for (row in 1:nrow(exp_data)) {
  year <- exp_data[row, "Year"]
  if(!is.na(year) & year == v_year){
    print(paste("YEAR-ROW",year))
    print(paste("row-i",i))
    v_Sector_Type_Code <- v_Sector_Type_Code - 1
  }
  exp_data_v5[row, "Year"] <- v_year + 1
  if (v_year==2008){
    break
  }
}
