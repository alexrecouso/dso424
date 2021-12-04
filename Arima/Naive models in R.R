library(fpp2)
library(tidyverse)
library(forecast)
# Naive models (forecast library)

# 2 types of naive models:
# non seasonal naive model
# and seasonal naive model

# Naive model = base line model = the simplest model

# Task: Build a naive model for a10 from fpp2
# Modeling setting
# Testing set = last 12 data points
# Predict 12 future values

a10

# Create a time series object from a10
y = ts(a10,start=c(1991,7),frequency = 12)
y

# Create training and testing sets: use window function
y.train=window(y,start=c(1991,7),end=c(2007,6))
# or y.train=window(y,end=c(2007,7))

y.test=window(y,start=c(2007,7),end=c(2008,6))
# or y.test=window(y,start=c(2007,7))

# Build a non-seasonal naive model on training set and predict on testing 
# Naive model forecast for period t +1 = actaul value at time t
M1 = naive(y.train,h=length(y.test),level=95)
M1

# Extract forecast on the testing set(Point forecast):
M1$mean

# Exctract fitted values on teh training set
M1$fitted

# Plot data and overlay predicted/fitted values 
# and testing set forecast

autoplot(y)
autoplot(y) + autolayer(M1$fitted,series="Fitted values") +
  autolayer(M1$mean,series="Forecast on \n testing set") +
  theme_bw()
# \n mean new line

# Accuracy metrics:

accuracy(M1,y.test)


# Predict real future: 12 points
# REFIT A MODEL USING THE ENTIRE DATA SET

M2 = naive(y,h=12,level=95)

# Forecast
M2$mean

# Plot data and overlay fitted and forecats:
autoplot(M2) + autolayer(M2$fitted ,series = "Fitted\nvalues")+
  theme_bw()


# Do the same thing but instead of naive use snaive:

M3 = snaive(y.train,h=length(y.test),level=95)
M3

# Plot data and overlay predicted/fitted values 
# and testing set forecast

autoplot(y)
autoplot(y) + autolayer(M3$fitted,series="Fitted values") +
  autolayer(M3$mean,series="Forecast on \n testing set") +
  theme_bw()
# \n mean new line

# Accuracy metrics:

accuracy(M1,y.test)


# Predict real future: 12 points
# REFIT A MODEL USING THE ENTIRE DATA SET

M4 = snaive(y,h=12,level=95)

# Plot data and overlay fitted and forecats:
autoplot(M4) + autolayer(M4$fitted ,series = "Fitted\nvalues")+
  theme_bw()

# plot data and overlay both predictions naive and snaive:
M4$mean

autoplot(y) + theme_bw() + autolayer(M2$fitted, series="naive fitted") +
  autolayer(M2$mean, series="naive forecast")+
  autolayer(M4$fitted, series="snaive fitted") +
  autolayer(M4$mean, series="snaive forecast")
