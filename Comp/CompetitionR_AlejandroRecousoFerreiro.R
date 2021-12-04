# Alejandro Recouso Ferreiro - recousof@usc.edu

library(lubridate)
library(tidyverse)
library(dygraphs)

# Import competition data in R:
# dont forget to change working directory
data = read.csv("CompetitionData.csv", sep = ';')

# Look at data: first and last few rows

head(data)
tail(data)

# Let's look at the structure of the table and variable types:
str(data)

# Install the lubridate library which contains functions 
# to work with date/time variables

dmy(data$Date[1])

# change the format of column Date into a date format:
data$Date = lubridate::dmy(data$Date)
str(data)

# Add a column in data that shows days of week
data$Date[1:10]
day(data$Date[1:10])
mday(data$Date[1:10])
wday(data$Date[1:10],label=T,abbr=F)
head(data)
data$DaysOfWeek = wday(data$Date,label=T,abbr=F)
head(data)
tail(data)

# Visualize load:
# In order to visuzalize data by hour we need to create a Time 
# variable = 1,2,3,...
dim(data) # how many rows and columns data has
dim(data)[1] # number of rows in the data table
data$Time = 1:(dim(data)[1])
data %>% ggplot(aes(x=Time,y=Load)) + geom_line()+
  theme_bw()

# you may create a dygraph with a range selector
# data must be a time series data
# we can first create a ts object
y.ts = ts(na.omit(data$Load[1:1000]))
# https://rstudio.github.io/dygraphs/
dygraph(y.ts) %>% dyRangeSelector()

# if dygraph don't wor, use filter in ggoplot:

# one day data
data %>% slice(1:24) %>% 
  ggplot(aes(x=Time,y=Load)) + 
  geom_line()+ geom_point() + theme_bw()

# one week data
data %>% slice(1:(24*7)) %>% 
  ggplot(aes(x=Time,y=Load)) + 
  geom_line()+ geom_point() + theme_bw()

# one month data
data %>% slice(1:(24*31)) %>% 
  ggplot(aes(x=Time,y=Load)) + 
  geom_line()+ geom_point() + theme_bw()

# one year data
data %>% slice(1:(24*365)) %>% 
  ggplot(aes(x=Time,y=Load)) + 
  geom_line()+ geom_point() + theme_bw()


##########################################################
#                          M0
##########################################################

# A baseline model without explanatory variables, 
# built using the simplest regression model
# y = b0 + epsilon
# & using Load as the KPI

M0 = lm(Load ~ 1, data = data)
summary(M0)

# Save predictions in a column M1 in data
data$M0 = predict(M0, newdata=data)

# Save residuals in another column
data$M0residuals = data$Load - data$M0

# plot data and overplay predictions
data %>% slice(1:(48*7*2)) %>%  ggplot(aes(x=Time,y=Load)) + 
  geom_line() + geom_point() + geom_line(aes(x = Time, y = M0), col = "blue") +
  geom_point(aes(x = Time, y = M0), col = "blue") + theme_bw()

data %>% ggplot(aes(x = Time, y = M0residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Time, y = M0residuals), col = "blue") +
  geom_hline(yintercept = 0)

#tsdisplay(data$M0residuals,lag.max=60)

#EVALUATION: M0 is just a baseline model without explanatory variables,
# so in the graph if residuals we can see no trend, seasons, 
# nor cycles are explained by the model


##########################################################
#                          M1 
##########################################################

M1 = lm(Load ~ Time + DaysOfWeek, data=data)
summary(M1)

# Save predictions in a column M1 in data
data$M1 = predict(M1, newdata=data)

# plot data and overplay predictions
data %>% slice(1:(48*7*2)) %>%  ggplot(aes(x=Time,y=Load)) + 
  geom_line() + geom_point() + geom_line(aes(x = Time, y = M1), col = "blue") +
  geom_point(aes(x = Time, y = M1), col = "blue") + theme_bw()

#EVALUATION: M1 the results are very basic but it is a good model 
# to start getting a sense of what’s possible using linear regression modelling
# we can make it better trying to capture daily seasonality too


##########################################################
#                          M2
##########################################################

M2 = lm(Load ~ Time + DaysOfWeek + Hour, data = data)
summary(M2)

# Save predictions in a column M2 in data
data$M2 = predict(M2, newdata = data)

# Save residuals in another column
data$M2residuals = data$Load - data$M2

# plot data and overplay predictions
data %>% slice(1:(48*7*2)) %>%  ggplot(aes(x = Time, y = Load)) + geom_line() +
  geom_point() + geom_line(aes(x= Time, y = M1), col="blue") +
  geom_point(aes(x = Time, y = M1), col = "blue") + 
  geom_line(aes(x = Time, y = M2), col = "red") +
  geom_point(aes(x = Time, y = M2), col = "red") + theme_bw()

data %>% slice(1:(48*7*2)) %>% ggplot(aes(x = Time, y = M2residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Time, y = M2residuals), col = "blue") +
  geom_hline(yintercept = 0)

#tsdisplay(data$M2residuals,lag.max=183)

# EVALUATION: this model does pretty well, despite it misses to capture
# what seems like a cyclical component in the data. We'll need further
# exploration to explain this.


##########################################################
#                          M3
##########################################################

str(data)
data$Temperature = data %>% select(Temperature) %>% unlist %>%  
  str_replace_all(",", ".") %>% as.numeric

M3 = lm(Load ~ Time + DaysOfWeek + Hour + Temperature, data = data)
summary(M3)

# Save predictions in a column M2 in data
data$M3 = predict(M3, newdata = data)

# Save residuals in another column
data$M3residuals = data$Load - data$M3

# plot data and overplay predictions
data %>% slice(1:(48*7*2)) %>%  ggplot(aes(x = Time, y = Load)) + geom_line() +
  geom_point() +
  geom_line(aes(x = Time, y = M2), col = "red") +
  geom_point(aes(x = Time, y = M2), col = "red") +
  geom_line(aes(x = Time, y = M3), col = "blue") +
  geom_point(aes(x = Time, y = M3), col = "blue") + theme_bw()

data %>% slice(1:(48*7*2)) %>% ggplot(aes(x = Time, y = M3residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Time, y = M3residuals), col = "blue") +
  geom_hline(yintercept = 0)

# EVALUATION: from the visualizations it seems hard to tell if adding the Temperature
# feature improved the model, but we see a small improvement in MAPE using Excel calculator.

##########################################################
#                          M4
##########################################################

#Let's try to model the months, since some traditional periods of holiday
# like December, may influence the consumption of electricity

# Add a column in data that shows months of the year
data$Month = month(data$Date,label=T,abbr=F)
head(data)
tail(data)

M4 = lm(Load ~ Time + DaysOfWeek + Hour + Temperature + Month, data = data)
summary(M2)

# Save predictions in a column M2 in data
data$M4 = predict(M4, newdata = data)

# Save residuals in another column
data$M4residuals = data$Load - data$M4

# plot data and overplay predictions
data %>% slice(1:(48*7*2)) %>%  ggplot(aes(x = Time, y = Load)) + geom_line() +
  geom_point() + geom_line(aes(x= Time, y = M3), col="blue") +
  geom_point(aes(x = Time, y = M3), col = "blue") + 
  geom_line(aes(x = Time, y = M4), col = "red") +
  geom_point(aes(x = Time, y = M4), col = "red") + theme_bw()

data %>% slice(1:(24*7*2)) %>% ggplot(aes(x = Time, y = M4residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Time, y = M4residuals), col = "blue") +
  geom_hline(yintercept = 0)

#tsdisplay(data$M4residuals,lag.max=183)

# EVALUATION: there's still some potentially cyclical component we are missing.
# However, it seems to be more than monthly, or every couple weeks, so it would
# be somewhat impractical to try to model it under linear regression using 
# the data provided. Also, a MAPE of around 10% obtained with the calculator
# on testing set is on the edge of being an acceptable model.

data %>% head
write.csv(data,"/Users/alexrecouso/Documents/GitHub/dso424/HW4\\comp_pred.csv", row.names = FALSE)