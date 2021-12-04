library(readxl)
library(dplyr)
library(ggplot2)

# Instal readxl library/package
# install.packages("readxl")

# Load readxl package: see above


# import data: 
?read_xlsx
setwd("C:/Users/gabrys/Desktop")
data = read_xlsx("Case Data.xlsx")
getwd()

# display first few rows and last few rows of data:
head(data)
tail(data)

# or useing the pipe %>% (hold down ctrl and shift and press m)operator:
# %>% require dplyr package
data %>% head

data = data %>% mutate(KPI=ACC/FUEL)
# data = data %>% mutate(.,KPI=ACC/FUEL)

data %>% head

# if you do not use pipe: data = mutate(data,KPI=ACC/FUEL)

# instead of mutate you can se$ sign:
# data$NewColumnName = data$ACC / data$FUEL


# Create line graphs for ACC, FUEL and KPI3:

data %>% ggplot(mapping=aes(x=QTR,y=ACC)) + geom_point() + 
  geom_line()


data %>% ggplot(mapping=aes(x=QTR,y=KPI)) + geom_point() + 
  geom_line() + geom_vline(xintercept=30,color="red")

data %>% ggplot(mapping=aes(x=QTR,y=FUEL)) + geom_point() + 
  geom_line()

# carry out 2 sample t test to assess whether the BM program 
# was effective

?t.test

t.test(x= data$KPI[1:29],y=data$KPI[30:52],alternative="greater")

# Ha:BM is effective, i.e. mu_before > mu_after 
# subtract mu_after from both sides: mu_before - mu_after >0

test_statistic=(mean(data$KPI[1:29]) - mean(data$KPI[30:52]))/
  sqrt(var(data$KPI[1:29])/29 + var(data$KPI[30:52])/23)
test_statistic

# Conclusion: since p-value = 0.131 > 0.05, we conclude that the data
# provides statistically insignificant evidence that the BM program
# is effective.

t.test(x=data$KPI[30:52],y= data$KPI[1:29],alternative="less")

#  Visualize the results:

data %>% ggplot(mapping=aes(x=QTR,y=KPI)) + geom_point() + 
  geom_line() + geom_vline(xintercept=30,color="red") +
  geom_hline(yintercept=mean(data$KPI[1:29]),color="blue")+
  geom_hline(yintercept=mean(data$KPI[30:52]),color="magenta")+
  theme_bw()
  
# Implement the abrupt change using regression model:
# create a dummy variable 0/1 and call it BM 
data %>%  head
data$BM = 0
data %>%  head
data %>%  tail
data$BM
data$BM[30:52]=1
data$BM


# or use mutate instead of $

# rep(0,29) repeat 0 29 times
c(rep(0,29),rep(1,23))
data = data %>% mutate(BM=c(rep(0,29),rep(1,23)))

# Using teh regression model assess the abrupt change in
# KPI:

data$BM

data %>% ggplot(mapping=aes(x=QTR,y=BM)) + geom_point() + 
  geom_line()

# y = b0 + b1 *x = regression equation
# in R: lm(y~x,data = namee of a data table)
M1 = lm(KPI ~ BM,data=data)
summary(M1)

class(M1)
typeof(M1)
names(M1)

# in lm output p-values are two sided
# p-value = 0.249/2 = 0.123

# Create a graph to show how regression fits the data
data = data %>% mutate(M1=M1$fitted.values)
# or instead using mutate use $ operator to add a column to a table data:
data$M1 = M1$fitted.values

data %>% ggplot(mapping=aes(x=QTR,y=KPI)) + geom_point() + 
  geom_line() + geom_vline(xintercept=30,color="red") +
  geom_point(mapping=aes(x=QTR,y=M1,col=as.factor(BM)))+
  theme_bw()


# How to capture gradual change?
# We will capture using trend
1:10
plot(1:10,type="b")  


c(rep(0,29),rep(1,23))

c(rep(0,29),1:23)

plot(c(rep(0,29),1:23),type="b") 

# add this feature to the data table and using regresssion 
# evaluate significance of it:
head(data)
data$Ramp = c(rep(0,29),1:23)
head(data)
tail(data)

# capture Ramp using regersssion:
M2=lm(KPI ~ Ramp,data=data)
summary(M2)
# Regression equation: 
# y = 6.18 -0.03 * x

# is gradual effect significant?
# Ho:BM is NOT effective (slope of Ramp is NOT negative)
# Ha:BM is effective (slope of Ramp is negative)
# calculate p-value:
0.0237/2 # = 0.01185

# save M2 fitted values in a coumn in data :

data$M2 = M2$fitted.values

data %>% ggplot(mapping=aes(x=QTR,y=KPI)) + geom_point() + 
  geom_line() + geom_vline(xintercept=30,color="red") +
  geom_point(mapping=aes(x=QTR,y=M1,col=as.factor(BM)))+
  theme_bw() +
  # geom_point(mapping=aes(x=QTR,y=M2,col=as.factor(BM)))
  geom_point(mapping=aes(x=QTR,y=M2),col="magenta")


# Build a model and include both BM and Ramp

M3 = lm(KPI ~ BM + Ramp,data=data)
# y = b0 + b1*x1 + b2*x2
# in R: lm(y ~ x1 + x2)
summary(M3)

# save M3 predicted/fitted values in data table in column M3:

data$M3 = M3$fitted.values

data %>% ggplot(mapping=aes(x=QTR,y=KPI)) + geom_point() + 
  geom_line() + geom_vline(xintercept=30,color="red") +
  geom_point(mapping=aes(x=QTR,y=M1),col="brown")+
  theme_bw() +geom_point(mapping=aes(x=QTR,y=M2),col="magenta")+
  geom_point(mapping=aes(x=QTR,y=M3),col="blue")
