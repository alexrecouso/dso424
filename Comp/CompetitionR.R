library(lubridate)
library(tidyverse)
library(dygraphs)

# Import competition data in R:
# dont forget to change working directory
data = read.csv("CompetitionData.csv")

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
