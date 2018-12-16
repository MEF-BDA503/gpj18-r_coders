install.packages("readxl")
install.packages("plyr")
install.packages("tidyverse", repos = "https://cran.r-project.org")

library(tidyverse)
library(plyr)
library("readxl")


setwd("C:\\Users\\birikiPC\\Documents\\GitHub\\gpj18-r_coders")

load("travel_weather.RData")

tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/import_data/Consumer_Inflation.rds?raw=true",destfile=tmp,mode = 'wb')
cons_inf_data<-read_rds(tmp)
file.remove(tmp)

tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/import_data/Producer_Inflation.rds?raw=true",destfile=tmp,mode = 'wb')
producer_inf_data<-read_rds(tmp)
file.remove(tmp)

tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/import_data/Enflasyon_Data.rds?raw=true",destfile=tmp,mode = 'wb')
enflasyon_data<-read_rds(tmp)
file.remove(tmp)

tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/imp_data.rds?raw=true",destfile=tmp,mode = 'wb')
imp_data<-read_rds(tmp)
file.remove(tmp)

tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/exp_data.rds?raw=true",destfile=tmp,mode = 'wb')
exp_data<-read_rds(tmp)
file.remove(tmp)

tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/import_data/US_Dollar_Montly_Rate.rds?raw=true",destfile=tmp,mode = 'wb')
us_rate_data<-read_rds(tmp)
file.remove(tmp)


imp_and_exp_data <- inner_join(exp_data, imp_data, by=c("Date" = "Date","Sector_Type_Code"="Sector_Type_Code"))

imp_and_exp_data_bymonth <- aggregate(cbind(Amount.x, Amount.y) ~ Date, data = imp_and_exp_data, sum)
imp_exp_data_bymonth <- gather(imp_and_exp_data_bymonth,
                               value = "value",
                               key = "type",
                               Amount.x, Amount.y)
ggplot(imp_exp_data_bymonth,
       aes(x=Date,
           y=value,
           color=type)) +
  geom_line()



ggplot(data = imp_and_exp_data_bymonth, mapping = aes(x = Date, y = n)) +
  geom_line()

imp_and_exp_data_without_NA <- imp_and_exp_data
imp_and_exp_data_without_NA[is.na(imp_and_exp_data_without_NA <- imp_and_exp_data)] <- 0
imp_and_exp_data_without_NA

imp_and_exp_data_bymonth <- aggregate(cbind(Amount.x, Amount.y) ~ Date, data = imp_and_exp_data_without_NA, sum)

imp_and_exp_data_reshape<- reshape2::melt(imp_and_exp_data_bymonth, id.var='Date')
head(imp_and_exp_data_reshape)

ggplot(imp_and_exp_data_reshape, aes(x=Date, y=value, col=variable)) + geom_line()

ggplot(imp_exp_data_bymonth, aes(x=Date, y=value, col=type)) + geom_line()



#-----------------------Data Transformation--------------------
exp_data_without_NA <- exp_data
exp_data_without_NA[is.na(exp_data_without_NA <- exp_data)] <- 0
exp_data_without_NA



#export and other variables
library("tidyverse")
exp_data_v2 <-
exp_data_without_NA %>%
  group_by(Date) %>%
  summarise(Export_Total_Amount=sum(Amount))


str(enflasyon_data)

enflasyon_data$Yil_Ay <- as.Date(as.character(as.POSIXct(enflasyon_data$Yil_Ay)))
str(enflasyon_data)

#♦exp_data_v3 <- inner_join(exp_data_v2,enflasyon_data, by=c("Date" = "Yil_Ay"))

str(cons_inf_data)
cons_inf_data$Date <- as.Date(as.character(as.POSIXct(cons_inf_data$Date)))
str(cons_inf_data)

exp_data_v3 <- inner_join(exp_data_v2,cons_inf_data, by=c("Date" = "Date"))

str(us_rate_data)
us_rate_data$Date <- as.Date(as.character(as.POSIXct(us_rate_data$Date)))
str(us_rate_data)

exp_data_v4 <- inner_join(exp_data_v3,us_rate_data, by=c("Date" = "Date"))
colnames(exp_data_v4) <- c("Date","Export_Total_Amount","Consumer_Price_Index_Yearly_Change","Consumer_Price_Index_Monthly_Change","USD_Rate")


str(producer_inf_data)
producer_inf_data$Date <- as.Date(as.character(as.POSIXct(producer_inf_data$Date)))
str(producer_inf_data)

#exp_data_v5 <- inner_join(exp_data_v4,producer_inf_data, by=c("Date" = "Date"))


#import and other variables
imp_data_without_NA <- exp_data
imp_data_without_NA[is.na(imp_data_without_NA <- imp_data)] <- 0
imp_data_without_NA



library("tidyverse")
imp_data_v2 <-
  imp_data_without_NA %>%
  group_by(Date) %>%
  summarise(Import_Total_Amount=sum(Amount))


imp_data_v3 <- inner_join(imp_data_v2,cons_inf_data, by=c("Date" = "Date"))

imp_data_v4 <- inner_join(imp_data_v3,us_rate_data, by=c("Date" = "Date"))

colnames(imp_data_v4) <- c("Date","Export_Total_Amount","Consumer_Price_Index_Yearly_Change","Consumer_Price_Index_Monthly_Change","USD_Rate")

saveRDS(imp_data_v4,file='imp_data_final.rds')
saveRDS(exp_data_v4,file='exp_data_final.rds')
