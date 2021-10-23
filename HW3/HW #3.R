#Case 1

#Business Understanding:
#the project objective is to build a predictive model
#to forecast future passenger demand for next 12 months for supporting 
#management decisions on a variety of application areas.

#Data Preparation:
#first, I downloaded data as .xls from the URL provided. I eliminated non-desired
#rows in MSExcel, and saved it as Passengers.xlsx to import it to RStudio
library(readxl)
data = read_xlsx('Passengers.xlsx')
data %>% head
#let's transform columns DOMESTIC and INTERNATIONAL to numeric
data$DOMESTIC = data %>% select(DOMESTIC) %>% unlist %>%  
  str_replace_all(",", "") %>% as.numeric
data$INTERNATIONAL = data %>% select(INTERNATIONAL) %>% unlist %>%  
  str_replace_all(",", "") %>% as.numeric
str(data) #done

#Data Understanding:
library(ggplot2)
data_yearly = subset(data, Month=='TOTAL',
                  select=c(Year, DOMESTIC, INTERNATIONAL))
data_yearly %>% head
data_yearly %>% ggplot(aes(x = Year, y = DOMESTIC)) +
  geom_line(color = 'magenta') +
  geom_line(aes(x=Year,y=DOMESTIC),color='blue') +
  theme_bw()
