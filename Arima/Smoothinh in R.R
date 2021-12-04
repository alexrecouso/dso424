library(fpp2)
library(tidyverse)
library(forecast)
# Smoothing models in R

# Task Built a smoothing model on training set and 
# predict on teh testing set
# compare with naive model
y = ts(a10,start=c(1991,7),frequency = 12)
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
