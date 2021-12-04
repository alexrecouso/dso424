library(forecast)
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
library(dygraphs)

data = read.csv("lobster.csv", sep = ';')
data
data = data %>% mutate(Day = c(1:31, 1:31))
data
data = data %>% mutate(Month = c(rep(7, 31), rep(8, 31)))
data
data = data %>% mutate(Date = str_c('2021', Month, Day, sep='-'))
data$Date = as.POSIXlt(data$Date, format="%Y-%m-%d")
data$Date = as.Date(data$Date)
data

#we create mail feature to deal with mail impact
data = data %>% mutate(Mail = c(rep(0, 7), 1, rep(0, 6), 1, rep(0, 13), 1,
                                rep(0, 6), 1, rep(0, 13), 1, rep(0, 6), 1, rep(0, 5)))
data
#we create day feature to deal with weekly seasonality
# we have monthly data with weekly seasonality (7 points per cycle)
# let's use dummy variable
data = data %>% mutate(Weekday_dummy = c(rep(1:7, 8), 1, 2, 3, 4, 5, 6))
data
data$D1 = 0
data$D1[data$Weekday_dummy=="1"] = 1
data
data$D2 = 0
data$D2[data$Weekday_dummy=="2"] = 2
data$D3 = 0
data$D3[data$Weekday_dummy=="3"] = 3
data$D4 = 0
data$D4[data$Weekday_dummy=="4"] = 4
data$D5 = 0
data$D5[data$Weekday_dummy=="5"] = 5
data$D6 = 0
data$D6[data$Weekday_dummy=="6"] = 6
data$D7 = 0
data$D7[data$Weekday_dummy=="7"] = 7
data


#Lets model Orders first
data %>% ggplot(aes(x = Date, y = Orders))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = Orders), col = "blue") +
  scale_x_date(date_labels = "%Y-%m-%d")

M1_orders = lm(Orders ~ Mail + D1 + D2 + D3 + D4 + D5 + D6, data = data) #not including intercept
summary(M1_orders)
data$M1_orders = predict(M1_orders, newdata = data)
data
data$M1_orders_residuals = data$Orders - data$M1_orders
data %>% ggplot(aes(x = Date, y = Orders))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M1_orders), col = "blue") +
  scale_x_date(date_labels = "%Y-%m-%d")
data %>% ggplot(aes(x = Date, y = M1_orders_residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Date, y = M1_orders_residuals), col = "blue") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%Y-%m-%d")
tsdisplay(data$M1_orders_residuals,lag.max=60)
#We see it's not perfect and could be improved, maybe by including a lag, but
#I would say for the time assigned to the task it's fair enough


#Lets model Repeat Orders
data %>% ggplot(aes(x = Date, y = Repeat.Orders))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = Repeat.Orders), col = "blue") +
  scale_x_date(date_labels = "%Y-%m-%d")

M1_reporders = lm(Repeat.Orders ~ Mail + D1 + D2 + D3 + D4 + D5 + D6, data = data) #not including intercept
summary(M1_reporders)
data$M1_reporders = predict(M1_reporders, newdata = data)
data
data$M1_reporders_residuals = data$Repeat.Orders - data$M1_reporders
data %>% ggplot(aes(x = Date, y = Repeat.Orders))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M1_reporders), col = "blue") +
  scale_x_date(date_labels = "%Y-%m-%d")
data %>% ggplot(aes(x = Date, y = M1_reporders_residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Date, y = M1_reporders_residuals), col = "blue") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%Y-%m-%d")
tsdisplay(data$M1_reporders_residuals,lag.max=60)
#We see again it's not perfect and could be improved, maybe by including lags on 1-7-14, but
#I would say for the time assigned to the task it's fair enough


#Lets model Shipments
data %>% ggplot(aes(x = Date, y = Shipments))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = Shipments), col = "blue") +
  scale_x_date(date_labels = "%Y-%m-%d")

M1_ship = lm(Shipments ~ Mail + D1 + D2 + D3 + D4 + D5 + D6, data = data) #not including intercept
summary(M1_ship)
data$M1_ship = predict(M1_ship, newdata = data)
data
data$M1_ship_residuals = data$Shipments - data$M1_ship
data %>% ggplot(aes(x = Date, y = Shipments))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M1_ship), col = "blue") +
  scale_x_date(date_labels = "%Y-%m-%d")
data %>% ggplot(aes(x = Date, y = M1_ship_residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Date, y = M1_ship_residuals), col = "blue") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%Y-%m-%d")
tsdisplay(data$M1_ship_residuals,lag.max=60)
#Again, we see we are missing a component that could be modeled (second peak of seasonality),
#and that could be modeled via lags. But for the time given to complete the assignment 
#I would say this is not too bad


#Lets model New Customers
data %>% ggplot(aes(x = Date, y = New.Customers))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = New.Customers), col = "blue") +
  scale_x_date(date_labels = "%Y-%m-%d")

M1_cus = lm(New.Customers ~ Mail + D1 + D2 + D3 + D4 + D5 + D6, data = data) #not including intercept
summary(M1_ship)
data$M1_cus = predict(M1_cus, newdata = data)
data
data$M1_cus_residuals = data$New.Customers - data$M1_cus
data %>% ggplot(aes(x = Date, y = New.Customers))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M1_cus), col = "blue") +
  scale_x_date(date_labels = "%Y-%m-%d")
data %>% ggplot(aes(x = Date, y = M1_cus_residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Date, y = M1_cus_residuals), col = "blue") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%Y-%m-%d")
tsdisplay(data$M1_cus_residuals,lag.max=60)
#Again, we see we are missing a component that could be modeled (second peak of seasonality),
#and that could be modeled via lags. But for the time given to complete the assignment 
#I would say this is not too bad

data
write.csv(data,"/Users/alexrecouso/Documents/GitHub/dso424/HW4\\lobster_pred.csv", row.names = FALSE)