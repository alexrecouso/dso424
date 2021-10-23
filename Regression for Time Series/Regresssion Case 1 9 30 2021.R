library(readxl)
library(dplyr)
library(ggplot2)
library(forecast)

setwd("C:/Users/gabrys/Desktop")
# data = read_xlsx("Case1Data.xlsx")
data = read.csv("Case1Data.csv")


head(data)
# when column names have spaces use `` (do not use " or ')

# rename last 3 columns, i.e. 4,5,6:

colnames(data)
colnames(data)[4:6] = c("cars","trucks","total")

colnames(data)

# visualize car and truck sales:
data %>% ggplot(aes(x=Time,y=cars))+
  geom_line(color='magenta')+
  geom_line(aes(x=Time,y=trucks),color='blue')+
  theme_bw() + 
  geom_vline(xintercept = 2007.9,col="red")

data %>% filter(Time > 2007.3)

# LEt's focus on car sales only

data %>% ggplot(aes(x=Time,y=cars))+
  geom_line()+ theme_bw() + 
  geom_vline(xintercept = 2007.9,col="red")+
  geom_point()

# Divide data into training and testing
1:72
# data = data %>% arrange(Time)
train = data[1:72,]
test = data[73:88,]

data$Time

dim(train)
dim(test)

head(train)
tail(train)

head(test)
tail(test)


train %>% ggplot(aes(x=Time,y=cars))+
  geom_line()+ theme_bw() + geom_point()

# Let's first create a feature to capture 
# trend in data: 

dim(train)

train = train %>% mutate(Trend = 1:72)
# or use $ operator to add a new column to the table
# train$Trend = 1:72

# Build a regression model to model trend:
train %>%  head
M1 = lm(cars ~ Trend,data = train)
# How good is the model?
summary(M1)

# Store Model M1 fitted/predicted values in column M1 in 
# table train 




train$M1 = M1$fitted.values
head(train)

# ALso add baseline model fitted/predicted values:

train$Baseline = mean(train$cars)

# Plot car sales in train set and overlay M1 and Baseline 
# predicted fitted values

train %>% ggplot(aes(x=Time,y=cars))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x=Time,y=M1),col="red")+
  geom_line(aes(x=Time,y=Baseline),col="blue")


# Alternatively: build a baseline model, M0,
# using regression
# Simplest regression model: y = b0 + epsilon
# a model without explanatory variables

M0 = lm(cars ~ 1,data = train)
# How good is the model?
summary(M0)

# Add predicted values to train table and call M0
train$M0 = M0$fitted.values

train %>% ggplot(aes(x=Time,y=cars))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x=Time,y=M1),col="red")+
  geom_line(aes(x=Time,y=Baseline),col="blue")+
  geom_point(aes(x=Time,y=M0),col="magenta",size=0.25)


# How can we capture and model seasonality
# Data = quartery data, annual seasonality (4 points
# represent one seasonal cycle)
# Seasonality = Q1, Q2, Q3, and Q4 =categorical variable
# How do we include categorical variable in regression?
# Use dummy variable
# Create columns Q1, Q2 and Q3:
head(train)
train$Q1 = 0
train$Q1[train$Quarter=="Q1"]=1

train$Q2 = 0
train$Q2[train$Quarter=="Q2"]=1

train$Q3 = 0
train$Q3[train$Quarter=="Q3"]=1

train$Q4 = 0
train$Q4[train$Quarter=="Q4"]=1

head(train)

# Build a modelto capture onlu seasonal component
# include intercept ( one Q's should not be included)
train %>% group_by(Quarter) %>% summarise(mean(cars))
M2=lm(cars ~ Q1 + Q2 + Q3,data=train)
summary(M2)

# save fitted/predicted values in teh data table:

train$M2 = M2$fitted.values
head(train)
train %>% ggplot(aes(x=Trend,y=cars)) + geom_line()+
  geom_line(aes(x=Trend,y=M2),color="red")+
  geom_line(aes(x=Trend,y=M1),color="blue")

# if we include all 4 Q's don't include intercept:
M2.NoIntercept = lm(cars ~ 0 + Q1 + Q2 + Q3 +Q4, data =train)
summary(M2.NoIntercept)

# Build a model to capture both trend and seasonal components
head(train)
M3 = lm(cars ~ Trend + Q1 + Q2 + Q3,data = train)
summary(M3)

# plot data and overlay fitted values

train$M3 = M3$fitted.values

train %>% ggplot(aes(x=Trend,y=cars)) + geom_line()+
  geom_line(aes(x=Trend,y=M2),color="red")+
  geom_line(aes(x=Trend,y=M1),color="blue")+
  geom_line(aes(x=Trend,y=M3),color="orange",size=1)+ theme_bw()

train %>% ggplot(aes(x=Trend,y=cars)) + geom_line()+
  geom_line(aes(x=Trend,y=M1),color="blue")+
  geom_line(aes(x=Trend,y=M3),color="orange",size=1)+ theme_bw()


# What are R^2's of each model

summary(M0)
summary(M1)$r.squared
summary(M1)$adj.r.squared

summary(M2)$r.squared
summary(M2)$adj.r.squared

summary(M3)$r.squared
summary(M3)$adj.r.squared

# Is our regression model good?
# Formula of data:
# Data = Systematic patterns + Random noise/error
# Systematic patterns can be predicted 
# Systematic patters = fitted/predicted values
# Noise cannot be predicted
# Noise is estimated by residuals:
# Estimate of noise = residuals = Data - Systematic patterns(Fitted values)

# Examples of systematic patters: trend, seasonal, 

# Example of noise: generate a sequence of let's say 100
# independent normal random variables and plot

x = rnorm(n=100)
x
plot(x,type="l") # l stands for line
abline(h=0) # add horizontal line to existing graph

# Model is good if residuals look like noise, i.e. contains no pattern:
train$M3Residuals = M3$residuals

train %>% ggplot(aes(x=Trend,y=M3Residuals)) + geom_line() +
  geom_hline(yintercept = 0) + theme_bw()

# Residuals are not random! 
# How can we understand the dependence structure in residuals?
# We can use correlation to measure dependence
# Acf = autocorrelation function that shows correlations of data 
# with its own lags
# Pacf = Partial autocorrelation  that shpows correlation of data
# with its own past after we remove effects of intermediate lags

Acf(train$M3Residuals)
# Blue dashed lines are confidence bounds/interval around 
# correlation of ZERO (no linear dependence, no memory)

# confidence interval is constructed in the following way:
# CI = 0  plus /minus Factor * Standard error of sample correlation
# Factor = 2( approx 1.96) for 95% confidence interval
# If there is no correlation, then sample correlation follows
# approximately N(0,St. dev=1/sqrt(n)), where n is sample size

# CI = 0 +/- 2* 1/sqrt(n)

# Conclusion: presence of significant lags indicate presence 
# of dependence in residuals, i.e. residuals are NOT white noise
# Presence of dependence = presence of systematic patterns


# How many lags shouyld we include in the model?
# We canNOT rely on ACF!!!!!!
# Instead we will look at Pacf:

Pacf(train$M3Residuals)
# Let's include first 2 lags in teh model.

# Create 2 columns for first two lags in teh train data set:
# Example begins
x=1:5
x
lag(x,n=1)
data.frame(x,xlag1 = lag(x,n=1),xlag2 = lag(x,n=2))
# Example ends

train$carsLag1 = lag(train$cars,1)
head(train)
train$carsLag2 = lag(train$cars,2)
head(train)


# Build a model with trend, seasonal dummies and 2 lags:
# Build a model to capture both trend and seasonal components
head(train)
M4 = lm(cars ~ Trend + Q1 + Q2 + Q3 + carsLag1 +carsLag2,data = train)
summary(M4)

# plot data and overlay fitted values

train$M4 = c(NA,NA,M4$fitted.values)

train %>% ggplot(aes(x=Trend,y=cars)) + geom_line()+
  geom_line(aes(x=Trend,y=M3),color="orange",size=1)+ theme_bw()

train %>% ggplot(aes(x=Trend,y=cars)) + geom_line()+
  geom_line(aes(x=Trend,y=M4),color="red",size=1)+ theme_bw()

# Is M4 good? = are residuals white noise? (=no significant corralations
# in ACF)

train$M4residuals = c(NA,NA,M4$residuals)

Acf(train$M4residuals)
Pacf(train$M4residuals)

plot(train$M4residuals,type="l") # l = line
abline(h=0)


############################
# Example

for(i in 1:10){
  print(i)
}
###########################

?predict

M4 = lm(cars ~ Trend + Q1 + Q2 + Q3 + carsLag1 +carsLag2,data = train)
head(train)

dim(test)
tail(train)

newxs = data.frame(Trend=73:(73+16-1),Q1=rep(c(1,0,0,0),4),
                   Q2=rep(c(0,1,0,0),4),Q3=rep(c(0,0,1,0),4),
                   carsLag1=NA,carsLag2=NA,prediction=NA)
newxs

newxs[1,"carsLag1"]=train[72,"cars"]
newxs[1,"carsLag2"]=train[71,"cars"]

newxs[1,"prediction"] = predict(M4,newxs[1,])
newxs %>% head

newxs[2,"carsLag1"]=newxs[1,"prediction"]
newxs %>% head
newxs[2,"carsLag2"]=newxs[1,"carsLag1"]
newxs %>% head

newxs[2,"prediction"] = predict(M4,newxs[2,])
newxs %>% head

for(i in 3:16){
  newxs[i,"carsLag1"]=newxs[i-1,"prediction"]
  newxs[i,"carsLag2"]=newxs[i-1,"carsLag1"]
  newxs[i,"prediction"] = predict(M4,newxs[i,])
}

newxs

finaldataset=data.frame(ActualSales=data$cars,
                        PredictedSales=c(NA,NA,M4$fitted.values,
                                         newxs$prediction),
                        Quarter=1:88)

finaldataset %>% ggplot(aes(x=Quarter,y=ActualSales))+
  geom_line()+ geom_line(aes(x=Quarter,y=PredictedSales),color="red")+
  theme_bw()

# Impact of recession:
tail(train)

sum(finaldataset$ActualSales[73:88]-finaldataset$PredictedSales[73:88])*1000
