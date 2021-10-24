#Case 1

#Business Understanding:
#the project objective is to build a predictive model
#to forecast future passenger demand for next 12 months for supporting 
#management decisions on a variety of application areas.


#Data Preparation:
#first, I downloaded data as .xls from the URL provided. I eliminated non-desired
#rows in MSExcel, and saved it as Passengers.xlsx to import it to RStudio

#setwd("C:/Users/alexrecouso/Desktop") #set desired working directory
#now load the libraries needed
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
#and load data
data = read_xlsx('Passengers.xlsx')
data %>% head

#let's transform columns DOMESTIC and INTERNATIONAL to numeric
data$DOMESTIC = data %>% select(DOMESTIC) %>% unlist %>%  
  str_replace_all(",", "") %>% as.numeric
data$INTERNATIONAL = data %>% select(INTERNATIONAL) %>% unlist %>%  
  str_replace_all(",", "") %>% as.numeric
str(data) #done;)
#now rename columns for consistency
colnames(data)=c('Year', 'Month', 'Domestic', 'International', 'Total')


#Data Understanding:
#create subsets to make plotting easier
data_yearly = subset(data, Month=='TOTAL',
                  select=c(Year, Domestic, International)) #only total data per year
data_monthly = subset(data, Month!='TOTAL',
                     select=c(Year, Month, Domestic, International)) #only monthly data
#we create a Date feature
data_monthly = data_monthly %>% mutate(Date = str_c(Year, Month, '1', sep='-'))
data_monthly$Date = as.POSIXlt(data_monthly$Date, format="%Y-%m-%d")
#convert Month to factor & Date to date
data_monthly$Month = factor(data_monthly$Month)
data_monthly$Date = as.Date(data_monthly$Date)
data_monthly %>% head
#divide by 1M to make graphs more appealing
data_yearly$Domestic = data_yearly$Domestic/1000000
data_yearly$International = data_yearly$International/1000000
data_monthly$Domestic = data_monthly$Domestic/1000000
data_monthly$International = data_monthly$International/1000000

#plot domestic flights per year
data_monthly %>% ggplot(aes(x = Date, y = Domestic)) +
  geom_line(color = 'blue') + geom_point() + scale_x_date(date_labels = "%Y-%m-%d") +
  ggtitle('Domestic Flights per Year (in Millions)')
#plot international flights per year
data_monthly %>% ggplot(aes(x = Date, y = International)) +
  geom_line(color = 'blue') + geom_point() + scale_x_date(date_labels = "%Y-%m-%d") +
  ggtitle('International Flights per Year (in Millions)')
#The first thing we see is that number of flights, both domestic and international,
# experienced an enormous decrease in 2020 with respect to 2019 and their previous
# trend, which was bullish. In 2021 we see another decrease with respect to 2020,
# but we should highlight that data for 2021 is still incomplete.
#In 2008&2009 we also see a much smaller disruption of the trend, mostly for 
# domestic flights, but which recovered in the following years.


#plot domestic flights per year
data_monthly %>% ggplot(aes(x = Date, y = Domestic, color = Month)) +
  geom_line(color = 'blue') + geom_point() + scale_x_date(date_labels = "%Y-%m-%d") +
  ggtitle('Domestic Flights per Year (in Millions)')
#plot international flights per year
data_monthly %>% ggplot(aes(x = Date, y = International, color = Month)) +
  geom_line(color = 'blue') + geom_point() + scale_x_date(date_labels = "%Y-%m-%d") +
  ggtitle('International Flights per Year (in Millions)')
