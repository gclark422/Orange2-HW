# September 08, 2019
# HW 2 Time Series 
# Daily Mean PM2.5 Concentration - Particulate Matter smaller than 2.5 micrometers Forecast

library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(imputeTS)
library(xts)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tseries)

# Set the working directory
setwd("/Users/CathyTran/Documents/Fall I 2019/Time Series")

# Read CSV into R
data <- read.csv(file="HW2_PM_2_5_Raleigh2.csv", header=TRUE, sep=",")

# set strings as factors to false
options(stringsAsFactors = FALSE)

# Convert character string to date format
data$Date <- as.Date(data$Date, format="%m/%d/%Y")

# Get the range of dates covered
DateRange <- seq(min(data$Date), max(data$Date), by = 1)

# Calculate missing values - 353
length(DateRange[!DateRange %in% data$Date])

# Create a z object to check for missing value after aggregated month
z <- zoo(data$Daily.Mean.PM2.5.Concentration, data$Date)
monthavg <- aggregate(z, as.yearmon, mean)

# 0 missing value after aggregated by month
sum(is.na(monthavg))

# Create Training (1300 obs 18 vars) & Validation (173 obs 18 vars) Data Set
data.train <- data[data$Date >= "2014-01-01" & data$Date <= "2018-06-30",]
data.valid <- data[data$Date >= "2018-07-01" & data$Date <= "2018-12-31",]

# Aggregate by month using Daily Mean PM2.5 Concentration
months.train <- data.train %>%
  group_by(year=year(Date), month=month(Date)) %>%
  summarise(mean=mean(Daily.Mean.PM2.5.Concentration))

months.valid <- data.valid %>%
  group_by(year=year(Date), month=month(Date)) %>%
  summarise(mean=mean(Daily.Mean.PM2.5.Concentration))

# Time Series Object#
ts.months.train <- ts(months.train$mean, start = 2014, frequency =12)

ts.months.valid <- ts(months.valid$mean, start = 2018, frequency = 12)

# Min, Max, Average from 2014 - 2018
whole <- data[data$Date >= "2014-01-01" & data$Date <= "2018-12-31",]

months.whole <- whole %>%
  group_by(year=year(Date), month=month(Date)) %>%
  summarise(mean=mean(Daily.Mean.PM2.5.Concentration))

ts.whole<- ts(months.whole$mean, start = 2014, frequency =12)

min(ts.whole) # 5.5
max(ts.whole) # 16.08
mean(ts.whole) # 9.85

min(ts.months.train) #5.5
max(ts.months.train) #16.08
mean(ts.months.train) #9.82

# Avg prior to 2017: 10.64
data.train.test <- data[data$Date >= "2014-01-01" & data$Date <= "2016-12-31",]

months.train.test <- data.train.test %>%
  group_by(year=year(Date), month=month(Date)) %>%
  summarise(mean=mean(Daily.Mean.PM2.5.Concentration))

avg.prior.2017 <- ts(months.train.test$mean, start = 2014, frequency =12)

mean(avg.prior.2017)

# Avg after 2017: 8.66
after.2017 <- data[data$Date >= "2017-01-01" & data$Date <= "2018-12-31",]
after <- after.2017 %>%
  group_by(year=year(Date), month=month(Date)) %>%
  summarise(mean=mean(Daily.Mean.PM2.5.Concentration))

avg.after.2017 <- ts(after$mean, start = 2017, frequency =12)

mean(avg.after.2017)
# Time Series Decomposition ...STL#
decomp_stl <- stl(ts.months.train, s.window = 7)
plot(decomp_stl, main = "STL Decomposition", xlab = "Year")

# Actual PM2.5 values overlaid with the trend/cycle component for the training set.
plot(ts.months.train, col = "grey", main = "Monthly PM2.5 Values - Trend/Cycle", xlab = "Year", ylab = "PM2.5 (ug/m3 LC)", lwd = 2)
lines(decomp_stl$time.series[,2], col = "red", lwd = 2)

# Actual PM2.5 values overlaid with the seasonally adjusted PM2.5 values for the training set.
seas_pm=ts.months.train-decomp_stl$time.series[,1]
plot(ts.months.train, col = "grey", main = "Monthly PM2.5 Values Seasonally Adjusted", xlab = "Year", ylab = "PM2.5 (ug/m3 LC)", lwd = 2)
lines(seas_pm, col = "red", lwd = 2)

# Sub-series
monthplot(decomp_stl$time.series[,"seasonal"], main = "PM 2.5 - Monthly Effects", ylab = "PM 2.5 Sub-Series", xlab = "PM 2.5 (Months)", lwd = 2)

# For the trend/cycle and seasonal breakdown, indicate whether classical or STL
# decomposition was used and why this technique was chosen.

# Classical
# trend - uses moving average smoothing
# seasonal- average de-trended values across seasons

# STL - better technique
# uses LOcal regrESSion techniques to estimate trend and seasonality
# allows for changing effects for trend and season
# adapted to handle outliers

# Time Plot of the predicted versus actual for the test data.

############ Model Building  ###############
# 1. Single Exponential Smoothing Model
# h= how many observation in the future you want to predict (6 months)
# Results on training data set
# AIC       AICc      BIC 
# 297.4907  297.9707  303.4576 
# MAE       MAPE
# 1.45      16.70127

SES.pm <- ses(ts.months.train, initial = "optimal", h = 6)
summary(SES.pm)

plot(SES.pm, main = "PM2.5 with Simple ESM Forecast", xlab = "Date", ylab = "PM Concentration (ug/m3 LC)")
abline(v = 2014, col = "red", lty = "dashed")
round(accuracy(SES.pm),2)

autoplot(SES.pm)+
  autolayer(fitted(SES.pm),series="Fitted")+ylab("PM Concentration (ug/m3 LC)")

# On Validation Data Set
# MAE         MAPE
# 1.888392    0.1810493
test.results=forecast(SES.pm,h=6)

x = unclass(ts.months.valid)
y = unclass(test.results$mean)

# compare how the model that I built which contains the predicted values against the validation data set
error=x-y
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(x))
print(MAE)
print(MAPE)

# 2. Linear Exponential Smoothing Model
# Results
# AIC         AICc        BIC 
# 300.9946    302.2446    310.9395 
# MAE         MAPE
# 1.456021    16.3127

LES.pm <- holt(ts.months.train, initial = "optimal", h = 6)
summary(LES.pm)

plot(LES.pm, main = "PM2.5 with Linear ESM Forecast", xlab = "Year", ylab = "PM Concentration (ug/m3 LC)")
abline(v = 2018.5, col = "red", lty = "dashed")

autoplot(LES.pm)+
  autolayer(fitted(LES.pm),series="Fitted")+ylab("PM Concentration (ug/m3 LC)")

# On Validation Data Set
# MAE         MAPE
# 2.200177    0.2074771
test.results=forecast(LES.pm,h=6)

x = unclass(ts.months.valid)
y = unclass(test.results$mean)

# compare how the model that I built which contains the predicted values against the validation data set
error=x-y
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(x))
print(MAE)
print(MAPE)


# 3. Linear Damping Exponential Smoothing Model
# Results
# AIC         AICc         BIC 
# 297.2389    299.0262    309.1728 
# MAE         MAPE
# 1.412463    15.47816

LDES.damped <- holt(ts.months.train, initial = "optimal", h = 6, damped = TRUE)
summary(LDES.damped)

plot(LDES.damped, main = "PM2.5 with Linear Damped ESM Forecast", xlab = "Year", ylab = "PM Concentration (ug/m3 LC)")
abline(v = 2018.5, col = "red", lty = "dashed")

autoplot(LDES.damped)+
  autolayer(fitted(LDES.damped),series="Fitted")+ylab("PM Concentration (ug/m3 LC)")

# On Validation Data Set
# MAE         MAPE
# 2.008073    0.1906637
test.results=forecast(LDES.damped,h=6)

x = unclass(ts.months.valid)
y = unclass(test.results$mean)

# compare how the model that I built which contains the predicted values against the validation data set
error=x-y
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(x))
print(MAE)
print(MAPE)

# 4. Holt-Winters ESM - Additive Model PM 2.5 Data #
# Additive Results on Training Data Set
# AIC         AICc        BIC 
# 299.0722    316.0722    332.8850 
# MAE         MAPE
# 1.32724     14.35338

HWES.pma <- hw(ts.months.train, seasonal = "additive")
summary(HWES.pma)

plot(HWES.pma, main = "PM 2.5 with Holt-Winters ESM Forecast", xlab = "Date", ylab = "PM Concentration (ug/m3 LC)")
abline(v = 2018.5, col = "red", lty = "dashed")


autoplot(HWES.pma)+
  autolayer(fitted(HWES.pma),series="Fitted")+ylab("PM Concentration")

# On Validation Data Set
# MAE         MAPE
# 2.2477      0.2285497

# 6 months forecast of the built model
test.results=forecast(HWES.pma,h=6)

x = unclass(ts.months.valid)
y = unclass(test.results$mean)

# compare how the model that I built which contains the predicted values against the validation data set
error=x-y
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(x))
print(MAE)
print(MAPE)

# 5. Holt-Winters ESM Multiplicative
HWES.pmm <- hw(ts.months.train, seasonal = "multiplicative")
summary(HWES.pmm)

plot(HWES.pmm, main = "PM 2.5 with Holt-Winters ESM Forecast", xlab = "Date", ylab = "PM Concentration (ug/m3 LC)")


autoplot(HWES.pmm)+
  autolayer(fitted(HWES.pmm),series="Fitted")+ylab("PM Concentration")

#Multiplicative Results
# AIC       AICc      BIC 
# 297.5899  314.5899  331.4026 
# 
# Error measures:
# MAE   1.233648     MAPE  13.54338

# MAE on validation data set 2.029253
# MAPE on validation data set 0.2090463

# Goodness of Fit Tests
# Change the model to calculate the MAE and MAPE on validation data
# 6 months forecast of the built model
test.results=forecast(HWES.pmm,h=6)

x = unclass(ts.months.valid)
y = unclass(test.results$mean)

# compare how the model that I built which contains the predicted values against the validation data set
error=x-y
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(x))
print(MAE)
print(MAPE)
