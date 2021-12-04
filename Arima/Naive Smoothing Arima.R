library(fpp2)
library(tidyverse)
library(forecast)
# Smoothing models in R

# Task Built a smoothing model on training set and 
# predict on teh testing set
# compare with naive model
y = ts(a10,start=c(1991,7),frequency = 12)
# if data is quarterly use frequency 4
# if data is hourly and seasonality is daily use frequency = 24
y
# Create training and testing sets: use window function
y.train=window(y,start=c(1991,7),end=c(2007,6))
# or y.train=window(y,end=c(2007,7))

y.test=window(y,start=c(2007,7),end=c(2008,6))
# or y.test=window(y,start=c(2007,7))

?ets

M1 = ets(y.train) # build smoothing model
M1

# forecast on the testing set:

M1F = forecast(M1,h=length(y.test),level=F)

# Exctract forecast on th testing set:
M1F$mean

# plot
autoplot(M1F) + autolayer(M1F$fitted) + autolayer(y.test)

# accuracy 
accuracy(M1F,y.test)

# Does accuracy function compare y.test with M1F$ mean and 
# calculate testing accuracy metrics?

mean(abs((y.test - M1F$mean)/y.test))*100

######################################################################

# Build arima ( manually and automatically), smoothing and naive models
# on training set, evaluate their performance on the testing set 
# Select a champion - a model that has lowest MAPE on testing set
# Predict next 12 data points using teh champion model


y.train


# M1 = snaive model M1=M1F = model M1 forecast
# h = forecast horizon = how many future values to forecast
length(y.test)
M1 = snaive(y.train,h=length(y.test),level=FALSE)

# Accuracy metrics: RMSE and MAPE
round(accuracy(M1,y.test)[,c("RMSE", "MAPE")],2)

# Is model good?
# look at residuals: if ACF has no significant spikes
# that indicates that your model captured systematically changing 
# patterns that can be predicted and what is left, residuals,
# are completely random and thus cannot be predicted
tsdisplay(M1$residuals)
# M1 is not good, seasonal naive model is not adequate! 
# There is significant dependence in residuals

# Plot M1 
autoplot(M1,size=1.25) + autolayer(M1$fitted,size=1.25) + 
  autolayer(y.test,size=1.25) + theme_bw()

# Plot only testing set and  overlay future forecast
autoplot(y.test) + autolayer(M1$mean,series="forecast") + theme_bw()


######################################################################

# M2 = smoothing model

M2 = ets(y.train) # build a model on the training set
M2

# Generate forecast on testing set:
M2F = forecast(M2,h=length(y.test),level=FALSE)

# Accuracy metrics: RMSE and MAPE
# Accuracy metrics: RMSE and MAPE
round(accuracy(M1,y.test)[,c("RMSE", "MAPE")],2) # Naive
round(accuracy(M2F,y.test)[,c("RMSE", "MAPE")],2) # Smoothing


# Is model good?
# look at residuals: if ACF has no significant spikes
# that indicates that your model captured systematically changing 
# patterns that can be predicted and what is left, residuals,
# are completely random and thus cannot be predicted
tsdisplay(M2$residuals)
# M2 is almost good, there is one significant seasonal lag  
# Hopefully arima will fix this issue

# Plot M
autoplot(M2F,size=1.25) + autolayer(M2$fitted,size=1.25) + 
  autolayer(y.test,size=1.25) + theme_bw()

# Plot only testing set and  overlay future forecast
autoplot(y.test) + autolayer(M2F$mean,series="forecast") + theme_bw()


# Plot y.train and overlay M1 and M2 fitted values

autoplot(y.train) + autolayer(M1$fitted,size=1.25) + 
  autolayer(M2F$fitted,size=1.25) + theme_bw()


# Plot y.test and overlay M1 and M2 forecasts on the testing

autoplot(y.test) + autolayer(M1$mean,size=1.25) + 
  autolayer(M2F$mean,size=1.25) + theme_bw()

######################################################################

# M3 = auto arima model

?auto.arima
# ar = auto regressive (auto regression) we use lags of y to predict y
# is data does not have trend i = 0, if data has trend i = 1
# ma = moving average = we use lags shocks/innovations

M3 = auto.arima(y.train,lambda="auto")
# if data needs to be tranformed, for example data 
# with multiplicative seasonality ( we take log before build model),
# include lambda ="auto"
# lambda =0 applies log transformation
# "auto" - selects the best Box-Cox tranformation 

# How auto.arima selects teh best model?
# The best model = the model that has smalles penalized error 
# on training set. Penalty term is larger for more complex models
# Penalized error on training set = RMSE on training set + Penalty
# term(complexicity of the model)
# ic =  information criterion = penalized error
# aicc is similar to cross validation approcah for time series

M3
#ARIMA(p=3,d=0,q=0)(P=0,D=1,Q=2)[12] with drift 
# Nonseasonal model strucure: (p=3,d=0,q=0)
# p = how many non seasonal lags of y to include in the model
# d = 0 if thre is no trend, 1 if there is trend
# q = how many of past innovations/shocks/residuals 
# to include in the model
# Seasonal model strucure:(P=0,D=1,Q2)
# How many points are in one seasonal cycle
#Model includes teh following explanatory variables:
#p=3: yLag1, yLag2, yLag3
#D=1: seasonal trend
#Q=2: residualLag12, residualLag24

# Box-Cof transformation:
# transformed y = (y^lambda - 1) / lambda
# when lambfa ->0 , tranformed y -> log(y)

# Generate forecast on the testing set

M3F = forecast(M3,h=length(y.test),level=FALSE)
M3F


# Accuracy metrics: RMSE and MAPE
# Accuracy metrics: RMSE and MAPE
round(accuracy(M1,y.test)[,c("RMSE", "MAPE")],2) # Naive
round(accuracy(M2F,y.test)[,c("RMSE", "MAPE")],2) # Smoothing
round(accuracy(M3F,y.test)[,c("RMSE", "MAPE")],2) # Smoothing


# Is model good?
# look at residuals: if ACF has no significant spikes
# that indicates that your model captured systematically changing 
# patterns that can be predicted and what is left, residuals,
# are completely random and thus cannot be predicted
tsdisplay(M3$residuals)
# M2 is almost good, there is one significant seasonal lag  
# Hopefully arima will fix this issue

# Plot M
autoplot(M3F,size=1.25) + autolayer(M3$fitted,size=1.25) + 
  autolayer(y.test,size=1.25) + theme_bw()

# Plot only testing set and  overlay future forecast
autoplot(y.test) + autolayer(M3F$mean,series="forecast") + theme_bw()


# Plot y.train and overlay M1 and M2 and M3 fitted values

autoplot(y.train) + autolayer(M1$fitted,size=1.25) + 
  autolayer(M2F$fitted,size=1.25) + theme_bw()+ 
  autolayer(M3F$fitted,size=1.25)


# Plot y.test and overlay M1 and M2 forecasts on the testing

autoplot(y.test) + autolayer(M1$mean,size=1.25) + 
  autolayer(M2F$mean,size=1.25) + theme_bw() + 
  autolayer(M3F$mean,size=1.25) 


# Create a table that contains RMSEs and MAPEs of all models on testing set

accuracy_all_models = rbind(round(accuracy(M1,y.test)[2,c("RMSE", "MAPE")],2), 
round(accuracy(M2F,y.test)[2,c("RMSE", "MAPE")],2), 
round(accuracy(M3F,y.test)[2,c("RMSE", "MAPE")],2))

data.frame(Model= c("Naive","Smoothing","ARIMA"),accuracy_all_models)


#Based one champion which is smoothing predict future 24 (2years) points:

Mchampion = ets(y) # build a model using entire data set (train +test)
Mchampion

# Generate forecast on testing set:
MchampionF = forecast(Mchampion,h=24,level=95)

autoplot(MchampionF) + autolayer(MchampionF$fitted) + theme_bw()

MchampionF
