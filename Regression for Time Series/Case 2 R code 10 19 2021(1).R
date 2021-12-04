# https://www.census.gov/data/developers/data-sets.html
# Load libraries
library(tidyverse)
library(forecast)

# Import data Case2Data.csv
# dont forget to set working directory
setwd("C:/Users/gabrys/Desktop")

data = read.csv("Case2Data.csv")

# Display first and last few rows of data
# to create pipe %>% presss ctrl(or command) +shift + m
data %>% head(2)
is.null(tail(data,4)[2,4])
is.na(tail(data,4)[2,4])
tail(data,4)[2,4]==" "
tail(data,4)[2,4]==""

# rename columns 5 and 6:
colnames(data)
colnames(data)[5:6]=c("Cost","Profit")
colnames(data)

# create a data frame that contains Year, Quarter and profit only:
data %>% select(Year,Quarter,Profit)  %>% head
data = data %>% select(Year,Quarter,Profit)

# what are the types of each column?
str(data)
# or 
glimpse(data)

# Problem with Profit: in order to convert column Profit to
# numeric format, we need to remove $ and , signs from column Profit

str_remove("12,213.02a",",")
data %>% select(Profit) %>% typeof
?typeof

data %>% select(Profit) %>% unlist %>%  str_remove(",") %>% 
  str_remove("\\$") %>% as.numeric

data$Profit = data %>% select(Profit) %>% unlist %>%  str_remove(",") %>% 
  str_remove("\\$") %>% as.numeric

str(data)

# if we want to remove both , and $ simultaneously:
data %>% mutate(Profit = as.numeric(str_remove_all(.$Profit,"[,\\$]")))
data = data %>% mutate(Profit = as.numeric(str_remove_all(.$Profit,"[,\\$]")))

# look at data 
head(data)

# Add Time column to the data
dim(data) # answer is a vector: in 1st position = number of rows
# 2nd position  = # columns
dim(data)[1]
1:(dim(data)[1])

data$Time = 1:(dim(data)[1])

head(data)

# Visualize Profit:

data %>% ggplot(aes(x=Time,y=Profit)) + geom_line(size=1.25) +  
  geom_point(aes(color=factor(Quarter)),size=2) + theme_bw()

# What patterns data has?
# Upward trend, seasonal component that increases with an 
# increse in level/trend (MULTIPLICATIVE SEASONALITY)
# Possible solutions 1. take logarithmic transformation 
# 2. Interaction

# Regression model is an additive model
# y = b0 + b1*x1 + b2*x2 +... 
# log (Trend * Seasonal) = log(Trend) + log(seasonal)

data %>%  head

# Add a Ramp 000 until 2002, 1,2,3,... starting 2002:

sum(data$Year<2002)
rep(0,sum(data$Year<2002))
sum(data$Year>=2002)
1:sum(data$Year>=2002)
c(rep(0,sum(data$Year<2002)),1:sum(data$Year>=2002))
data$Ramp = c(rep(0,sum(data$Year<2002)),1:sum(data$Year>=2002))
# or using mutate:
# data %>% mutate(Ramp = c(rep(0,sum(Year<2002)),1:sum(Year>=2002)))

# Change character variable, Quarter, to a corresponding factor variable:

str(data)
data$Quarter = factor(data$Quarter)
str(data)

# we do not need to manually create dummies, lm functions converts
# factors to dummies automatically

# If you want to manually create dummies and store them in data, 
# you can use model.matrix function:

data %>% head %>% model.matrix(~.-1,data =.)
# y = b0 + b1*x1   -> lm(y~x1)

#Modeling:

# Build a model to capture trend:

M1 = lm(Profit ~ Time, data = data)
M1 %>% summary

# Add fitted values and residuals to the model:
data$M1 = NA # create a column that contains NAs
data$M1residuals = NA

head(data)
data %>% tail

is.na(data$Profit)
!is.na(data$Profit)
data$M1[!is.na(data$Profit)] = M1$fitted.values
data$M1residuals[!is.na(data$Profit)] = M1$residuals

head(data)
data %>% tail

data %>%  ggplot(aes(x=Time,y=Profit)) + geom_line() +
  geom_line(aes(x=Time,y=M1),color="red")

data %>%  ggplot(aes(x=Time,y=M1residuals)) + geom_line()

# What are columns in data?
head(data)

# Build a model and evaluate whether changes introduced in 2002 
# were successful
# Let's try to automate code
i = 2 # represents which model: M2

M = lm(Profit ~ Time + Ramp, data = data)
?assign
paste("M",i)
# paste("M",i,sep="") = M DOES NOT WORK INSTEAD USE assign or :=
assign(paste("M",i,sep=""),M)

# Is Ramp significant?
M %>% summary
# Answer: Ramp is highly significant, i.e. changes in 2002
# increased profit statistically significantly

# store predicted/fitted values and residuals in data in 
# columns M2 and M2residuals
paste("M",i,sep="")
# !! unqoute column name
data = data %>% mutate(!!paste("M",i,sep=""):=NA,
                !!paste("M",i,"residuals",sep=""):=NA)

# Store model predicted values and residuals in 
# columns Mi and Miresiduals 
data[!is.na(data$Profit),paste("M",i,sep="")] = M$fitted.values
data[!is.na(data$Profit),paste("M",i,"residuals",sep="")] = 
  M$residuals

head(data)
tail(data)

# Plot data and overlay model

data %>%  ggplot(aes(x=Time,y=Profit)) + geom_line() +
  geom_line(aes(x=Time,y=M2),color="red") + theme_bw()

data %>%  ggplot(aes(x=Time,y=M1residuals)) + geom_line()
 

# What model for trend should we use?
data %>%  ggplot(aes(x=Time,y=Profit)) + geom_line()

#2nd order polynomial 
i = 3 # represents which model: M2

M = lm(Profit ~ Time + I(Time^2), data = data)
data = data %>% mutate(!!paste("M",i,sep=""):=NA,
                       !!paste("M",i,"residuals",sep=""):=NA)
data[!is.na(data$Profit),paste("M",i,sep="")] = M$fitted.values
data[!is.na(data$Profit),paste("M",i,"residuals",sep="")] = 
  M$residuals
# Plot data and overlay model
data %>%  ggplot(aes(x=Time,y=Profit)) + geom_line() +
  geom_line(aes(x=Time,y=M3),color="red") + theme_bw()+
  geom_line(aes(x=Time,y=M2),color="blue") +
  geom_line(aes(x=Time,y=M1),color="magenta") 

# what about cubic polynomial?

#3rd order polynomial 
i = 4 # represents which model: 

# M = lm(Profit ~ Time + I(Time^2) +I(Time^3), data = data)
# or use poly()
M = lm(Profit ~ poly(Time,3), data = data)
data = data %>% mutate(!!paste("M",i,sep=""):=NA,
                       !!paste("M",i,"residuals",sep=""):=NA)
data[!is.na(data$Profit),paste("M",i,sep="")] = M$fitted.values
data[!is.na(data$Profit),paste("M",i,"residuals",sep="")] = 
  M$residuals
# Plot data and overlay model
data %>%  ggplot(aes(x=Time,y=Profit)) + geom_line() +
  geom_line(aes(x=Time,y=M3),color="red") + theme_bw()+
  geom_line(aes(x=Time,y=M2),color="blue") +
  geom_line(aes(x=Time,y=M1),color="magenta") +
  geom_line(aes(x=Time,y=M4),color="brown",size=1.25)

# Let's use Time +Ramp and now model seasonal fluctuations:

i = 5 # represents which model: 
head(data)
str(data)
M = lm(Profit ~ Time + Ramp + Quarter, data = data)

data = data %>% mutate(!!paste("M",i,sep=""):=NA,
                       !!paste("M",i,"residuals",sep=""):=NA)
data[!is.na(data$Profit),paste("M",i,sep="")] = M$fitted.values
data[!is.na(data$Profit),paste("M",i,"residuals",sep="")] = 
  M$residuals
# Plot data and overlay model
data %>%  ggplot(aes(x=Time,y=Profit)) + geom_line() +
  geom_line(aes(x=Time,y=M5),color="red") + theme_bw()

# Conclusion: additive model does NOT capture multiplicative
# seasonality, i.e. seasonal oscillations increase with an 
# increase in trend
# To capture multiplicative seasonality apply log or
# create interaction terms. 


# Include time and quarter interaction:

i = 6  # represents which model: 
head(data)
str(data)
M = lm(Profit ~  Time + Ramp + Quarter + Time:Quarter, data = data)
# just interaction Time:Quarter
# Time*Quarter = Time +Quarter + Time:Quarter
M %>% summary

M = lm(Profit ~  (Time + Ramp)*Quarter , data = data)
M %>% summary

data = data %>% mutate(!!paste("M",i,sep=""):=NA,
                       !!paste("M",i,"residuals",sep=""):=NA)
data[!is.na(data$Profit),paste("M",i,sep="")] = M$fitted.values
data[!is.na(data$Profit),paste("M",i,"residuals",sep="")] = 
  M$residuals
# Plot data and overlay model
data %>%  ggplot(aes(x=Time,y=Profit)) + geom_line() +
  geom_line(aes(x=Time,y=M6),color="red") + theme_bw()


i = 7  # Predict log Profit
# log(5*10) = log(5) + log(10)
# log(multiplicative pattern) = additive pattern
data %>%  ggplot(aes(x=Time,y=Profit)) + geom_line()
data$LogProfit = log(data$Profit)
head(data)
str(data)
data %>%  ggplot(aes(x=Time,y=LogProfit)) + geom_line() + 
  geom_point(aes(x=Time,y=LogProfit,color=Quarter)) +theme_bw()
M = lm(LogProfit ~  Time + Ramp + Quarter , data = data)
# just interaction Time:Quarter
# Time*Quarter = Time +Quarter + Time:Quarter
M %>% summary

data = data %>% mutate(!!paste("M",i,sep=""):=NA,
                       !!paste("M",i,"residuals",sep=""):=NA)
data[!is.na(data$Profit),paste("M",i,sep="")] = M$fitted.values
data[!is.na(data$Profit),paste("M",i,"residuals",sep="")] = 
  M$residuals
# Plot data and overlay model
data %>%  ggplot(aes(x=Time,y=LogProfit)) + geom_line() +
  geom_line(aes(x=Time,y=M7),color="red") + theme_bw()

# How to automate ggplot, i.e. y=M7:

# make column names strings and use aes_string

data %>%  ggplot(aes(x=Time,y=LogProfit)) + geom_line() +
  geom_line(aes_string(x="Time",y=paste("M",i,sep="")),color="red") + theme_bw()

# How good are these models? 
# Which one should we use to forecast data?

# Goodness of fit Criterion: a model is good is residuals are white noise
# white noise = residuals without any significant dependence
head(data)


i=1
data %>%  ggplot(aes(x=Time,y=Profit)) + geom_line() +
  geom_line(aes_string(x="Time",y=paste("M",i,sep="")),color="red") + theme_bw()
tsdisplay(data$M1residuals)
# M1 is not a good model, does not capture seasonality


i=2
data %>%  ggplot(aes(x=Time,y=Profit)) + geom_line() +
  geom_line(aes_string(x="Time",y=paste("M",i,sep="")),color="red") + theme_bw()
tsdisplay(data$M2residuals)
# M2 is not a good model, does not capture seasonality


i=3
data %>%  ggplot(aes(x=Time,y=Profit)) + geom_line() +
  geom_line(aes_string(x="Time",y=paste("M",i,sep="")),color="red") + theme_bw()
tsdisplay(data$M3residuals)
# M3 is not a good model, does not capture seasonality


i=4
data %>%  ggplot(aes(x=Time,y=Profit)) + geom_line() +
  geom_line(aes_string(x="Time",y=paste("M",i,sep="")),color="red") + theme_bw()
tsdisplay(data$M4residuals)
# M4 is not a good model, does not capture seasonality


i=5
data %>%  ggplot(aes(x=Time,y=Profit)) + geom_line() +
  geom_line(aes_string(x="Time",y=paste("M",i,sep="")),color="red") + theme_bw()
tsdisplay(data$M5residuals)
# M5 is not a good model, does not capture multiplicative seasonality

i=6
data %>%  ggplot(aes(x=Time,y=Profit)) + geom_line() +
  geom_line(aes_string(x="Time",y=paste("M",i,sep="")),color="red") + theme_bw()
tsdisplay(data$M6residuals)
# M6 is not a good model, does not capture cyclical component

i=7
data %>%  ggplot(aes(x=Time,y=LogProfit)) + geom_line() +
  geom_line(aes_string(x="Time",y=paste("M",i,sep="")),color="red") + theme_bw()
tsdisplay(data$M7residuals)
# M6 is not a good model, does not capture cyclical component


# Both Model 6 and 7 seem to be good candidates. Let's start with M6:
# M6 ACF indicates presence of dependence. M6 PACF has one significant lag. 
# We are going to add lag 1 of profit in Model M6:

head(data)

# create lag 1 of column profit and include it in the table:
data$ProfitLag1 = lag(data$Profit,1)
head(data)
tail(data)

#####################################################################################
i = 8  # represents which model: 
#####################################################################################

M = lm(Profit ~  Time + Ramp + Quarter + Time:Quarter +ProfitLag1, data = data)
M %>% summary

data = data %>% mutate(!!paste("M",i,sep=""):=NA,
                       !!paste("M",i,"residuals",sep=""):=NA)
# & means and, | means or
data[!(is.na(data$Profit) | is.na(data$ProfitLag1)),paste("M",i,sep="")] = M$fitted.values

data[!(is.na(data$Profit) | is.na(data$ProfitLag1)),paste("M",i,"residuals",sep="")] = M$residuals

# Plot data and overlay model
data %>%  ggplot(aes(x=Time,y=Profit)) + geom_line() +
  geom_line(aes_string(x="Time",y=paste("M",i,sep="")),color="red") + theme_bw()+
  geom_line(aes_string(x="Time",y="M6"),color="blue") 

tsdisplay(data$M8residuals)
  
# ACF shows no significant lags, i.e. data follows white noise process, i.e. there is no 
# autocorrelation/dependence in residuals. This means that the features that we included in the model
# captured and modeled all systematic(predictable patterns) and whatever is left(ressiduals) cannot be 
# predicted. 
  

# WE can use this model to predict last 3 quarters of 2012
# use predict function:
data %>%  tail

# Predict profit for 2012 Q2:
data[70,"Profit"]
data[70,"Profit"] = predict(M,newdata = data[70,])
data[70,"Profit"]

# Create ProfitLag1 for 2012 Q3: 
# ProfitLag1 for 2012 Q3 = Profit(if actual is not available use predicted) for 2012 Q2

data[71,"ProfitLag1"] = data[70,"Profit"]
data %>%  tail

# Predict profit for 2012 Q3:
data[71,"Profit"]
data[71,"Profit"] = predict(M,newdata = data[71,])
data[71,"Profit"]

# Plot data and overlay model
data %>%  ggplot(aes(x=Time,y=Profit)) + geom_line() +
  geom_line(aes_string(x="Time",y=paste("M",i,sep="")),color="red") + theme_bw()

# Repeat the same steps for Q4: 
data %>%  tail
data[72,"ProfitLag1"] = data[71,"Profit"]
data %>%  tail
data[72,"Profit"]
data[72,"Profit"] = predict(M,newdata = data[72,])
data[72,"Profit"]

# Plot data and overlay fitted values and future forecast. 
data %>%  ggplot(aes(x=Time,y=Profit)) + geom_line() + geom_point()+
  geom_line(aes_string(x="Time",y=paste("M",i,sep="")),color="red") + theme_bw()+
  geom_line(data = data[70:72,],aes(x=Time,y=Profit),color="blue",size=2)+
  geom_point(data = data[70:72,],aes(x=Time,y=Profit),color="blue",size=4)


# Which model is last?
i
# M8 is the last model

# look at M8 residuals
 head(data)

data %>% ggplot(aes(x=Time,y=M8residuals)) + geom_line() +
  geom_hline(yintercept=0) + theme_bw() +geom_point()


# Evaluate models predictive accurace by calculating teh following accuracy metrics:
# MSE = mean squared error = mean(residuals^2)=mean((actual -predicted)^2)
# RMSE = room mean squared error = sqrt(MSE)
# MAPE = mean absolute % error = mean(|(actual-predicted)/actual|)

# acutal = column Profit
# predicted  = columns M1, M2, ... M8

# Goodness of fit of a model: the smaller the error the better(more accurate)
# the model:


# M1
# MSE
mean((data$Profit - data$M1)^2,na.rm = TRUE) 
# na.rm = TRUE -> removes NAs from mean calculation

#RMSE
sqrt(mean((data$Profit - data$M1)^2,na.rm = TRUE))
# RMSE is used to construct the prediction interval (PI)
# PI = predicted value +/- (give or take) RMSE

# MAPE
mean(abs((data$Profit - data$M1)/data$Profit),na.rm=TRUE)*100

# Create a table that shows MSE, RMSE and MAPE for M1, M2,...M8

# M7 predicts log(profit). 
# before error calculation we need to undo log!

data$M7 = exp(data$M7)

MSE = c(mean((data$Profit - data$M1)^2,na.rm = TRUE) ,
      mean((data$Profit - data$M2)^2,na.rm = TRUE) ,
      mean((data$Profit - data$M3)^2,na.rm = TRUE) ,
      mean((data$Profit - data$M4)^2,na.rm = TRUE) ,
      mean((data$Profit - data$M5)^2,na.rm = TRUE) ,
      mean((data$Profit - data$M6)^2,na.rm = TRUE) ,
      mean((data$Profit - data$M7)^2,na.rm = TRUE) ,
      mean((data$Profit - data$M8)^2,na.rm = TRUE) )

MAPE = c(mean(abs((data$Profit - data$M1)/data$Profit),na.rm=TRUE)*100,
  mean(abs((data$Profit - data$M2)/data$Profit),na.rm=TRUE)*100,
  mean(abs((data$Profit - data$M3)/data$Profit),na.rm=TRUE)*100,
  mean(abs((data$Profit - data$M4)/data$Profit),na.rm=TRUE)*100,
  mean(abs((data$Profit - data$M5)/data$Profit),na.rm=TRUE)*100,
  mean(abs((data$Profit - data$M6)/data$Profit),na.rm=TRUE)*100,
  mean(abs((data$Profit - data$M7)/data$Profit),na.rm=TRUE)*100,
  mean(abs((data$Profit - data$M8)/data$Profit),na.rm=TRUE)*100 )

ModelID = paste("M",1:8,sep="")

accuracy.table = data.frame(ModelID = ModelID,MSE = MSE, MAPE = MAPE)

# sort model from most accurate to least accurate based on MAPE:

accuracy.table %>%  arrange(MAPE)


# Alternative approach: apply transformation that absorbs as patterns as possible
# Result: simpler data (fewer patterns) -> you will need a simpler model. 

plot.ts(diff(data$Profit))


