library(tidyverse)
library(forecast)
library(lubridate)

setwd("C:/Users/gabrys/Desktop")

# Import data
data = read.csv("example.csv")


head(data) # first = 2012-01-02 00:00:00
tail(data) # first = 2012-12-31 23:30:00

# Visualize data

data %>% ggplot(aes(x=X,y=value)) + geom_line()+
  theme_bw()

data %>% ggplot(aes(x=X,y=value)) + geom_line()+
  theme_bw() + facet_wrap(type~.,scales = "free_y")

data %>% ggplot(aes(x=X,y=value)) + geom_line()+
  theme_bw() + facet_grid(type~.,scales = "free_y")

data %>% ggplot(aes(x=date,y=value)) + geom_line()+
  theme_bw() + facet_grid(type~.,scales = "free_y")

str(data)
# change date column format from character to date
data$date = ymd(data$date)

str(data)

data %>% ggplot(aes(x=date,y=value)) + geom_line()+
  theme_bw() + facet_grid(type~.,scales = "free_y")

# let's analyze type =="education and build a model 
# to predict  load for education

# Training = first data point - end of october
# Testing set = November and December

data=data[data$type == "Education",]

head(data)
# Remove column X
data = data[,-1] # remove 1st column from data table
head(data)

# Let's build a regression model using the lm function
# Task: look at data and idteify patterns in data
# create features (columns) in data (feature engineering) and 
# use lm function to build models

data %>% ggplot(aes(x=date,y=value)) + geom_line()+
  theme_bw() 

# to better understand the data, let's plot a small subset
# use slice function to subset data

data %>%  slice(1:48) %>% ggplot(aes(x=date,y=value)) + geom_line()+
  theme_bw() 

# Let's crreat ea time column:
data$time = 1: (dim(data)[1])

# One day
data %>%  slice(1:48) %>% ggplot(aes(x=time,y=value)) + geom_line()+
  geom_point() + theme_bw() 

# two daya
data %>%  slice(1:(48*2)+48) %>% ggplot(aes(x=time,y=value)) + geom_line()+
  geom_point() + theme_bw() 


# One week
data %>%  slice(1:(48*14)) %>% ggplot(aes(x=time,y=value)) + geom_line()+
  geom_point() + theme_bw() 

# Two weeks
data %>%  slice(1:(48*7)) %>% ggplot(aes(x=time,y=value)) + geom_line()+
  geom_point() + theme_bw() 

# one month

# Oner month
data %>%  slice(1:(48*31)) %>% ggplot(aes(x=time,y=value)) + geom_line()+
  geom_point() + theme_bw() 

# Patterns in data:
# Trend = not significantly pronounced
# Seasonal 1 = daily seasonality (48 points per one day)
# Seasonal 2 = workday/weekend seasonality

head(data)
str(data)

# To model day of week seasonality (mondays are similar, 
# tuesday are similar) column week must be a factor

data$week = factor(data$week)
str(data)

##########################################################
#          Divide data into training and testing 
##########################################################
# we want to predict load which is in column value
data$Actual = data$value
# replace values in value for the testing set

data$date[1:72]>"2012-01-02" # example

data$value[data$date>="2012-11-01"] = NA # exclude testing set

head(data)
tail(data)

data$trend = data$time

##########################################################
#                          M1 
##########################################################

M1 = lm(value ~ trend + week, data=data)
summary(M)

# Save predictions in a column M1 in data

# MUST BE M1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
data$M1 = predict(M1,newdata=data)

# plot data and overplay predictions
data %>% slice(1:(48*7*2)) %>%  ggplot(aes(x=time,y=value)) +geom_line() +
  geom_point() + geom_line(aes(x=time,y=M1),col="blue")+
  geom_point(aes(x=time,y=M1),col="blue") + theme_bw()

# Metrics of accuracy

# Training RMSE and MAPE:
test="2012-11-01"

# RMSE training 
sqrt(mean((data$Actual[data$date<test] - data$M1[data$date<test])^2))
# how good is your RMSE?
# You may use CV = Coefficient of Variation = RMSE / mean(value)
# Rule of thumb: if CV is less than 10% then predictions are accurate

sqrt(mean((data$Actual[data$date<test] - data$M1[data$date<test])^2))/
  mean(data$Actual[data$date<test]) * 100

# MAPE training 
mean(abs((data$Actual[data$date<test] - data$M1[data$date<test])/
  data$Actual[data$date<test])) *100

#RMSE test
sqrt(mean((data$Actual[data$date>=test] - data$M1[data$date>=test])^2))
# MAPE testing
mean(abs((data$Actual[data$date>=test] - data$M1[data$date>=test])/
           data$Actual[data$date>=test])) *100


##########################################################
#                          M2
##########################################################
head(data)

# how many days are in the data set?
dim(data)[1]/48

# create column halfhour = rep(1,2,3,...,48, 365 times)
# and convert halfhour to factor
data$halfhour = factor(rep(1:48,365))
str(data)

M2 = lm(value ~ trend + week + halfhour, data=data)
summary(M2)

# Save predictions in a column M2 in data

data$M2 = predict(M2,newdata=data)

# plot data and overplay predictions
data %>% slice(1:(48*7*2)) %>%  ggplot(aes(x=time,y=value)) +geom_line() +
  geom_point() + geom_line(aes(x=time,y=M1),col="blue")+
  geom_point(aes(x=time,y=M1),col="blue") + 
  geom_line(aes(x=time,y=M2),col="red")+
  geom_point(aes(x=time,y=M2),col="red")
  theme_bw()

  
# RMSE training 
sqrt(mean((data$Actual[data$date<test] - data$M2[data$date<test])^2))
#RMSE test
sqrt(mean((data$Actual[data$date>=test] - data$M2[data$date>=test])^2))
  
# MAPE training 
mean(abs((data$Actual[data$date<test] - data$M2[data$date<test])/
             data$Actual[data$date<test])) *100
# MAPE testing
mean(abs((data$Actual[data$date>=test] - data$M2[data$date>=test])/
           data$Actual[data$date>=test])) *100
