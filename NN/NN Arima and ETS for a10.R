# Feed forward NN based on lags of time series can be
# built using nnetar = neural network autoregression
# in forecast library

library(forecast)
library(fpp2) # has a10 data
library(ggplot2)

# Use a10 data set
# testing set 2007-2008

y.train = window(a10,end = c(2006,12))
y.test = window(a10,start = c(2007,1))


# plot data
autoplot(y.train) + autolayer(y.test)

# Build a NN model
?nnetar

M = nnetar(y.train)
M = auto.arima(y.train,lambda="auto")
M = ets(y.train)
M

# generate forecast

MF=forecast(M,h=length(y.test))

# plot
autoplot(MF)

accuracy(MF,y.test)

# Is it better than arima or ets?



# plot 
library(NeuralNetTools)
plotnet(rep(0,11),struct = c(3,2,1))
