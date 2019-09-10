# HW 2

# Needed Libraries for Analysis #
install.packages('lubridate')
install.packages('dplyr')
library(lubridate)
library(dplyr)
library(haven)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)
library(ggplot2)

# Saving File Locations and Uploading CSV File #
file.dir <- "C:/Users/Price Burnett/Documents/time series/"
input.file1 <- "PM_2_5_Raleigh2"
raleigh <- read.csv(file="C:/Users/Price Burnett/Documents/time series/PM_2_5_Raleigh2.csv", header=TRUE, sep=",")


# Extract Date
dates <- as.Date(raleigh$Date,"%m/%d/%Y")

# Extract month and yr
raleigh$Month_Yr <- format(as.Date(dates), "%Y-%m")

# Split to training/Validation
train.raleigh <- raleigh[raleigh$Month_Yr >= "2014-01" & raleigh$Month_Yr < "2018-07",]
valid.raleigh <- raleigh[raleigh$Month_Yr >= "2018-07" & raleigh$Month_Yr <= "2018-12",]


#aggregate by months with dplyr
months.train <- aggregate(train.raleigh$Daily.Mean.PM2.5.Concentration, list(time=train.raleigh$Month_Yr), mean)
months.valid <- aggregate(valid.raleigh$Daily.Mean.PM2.5.Concentration, list(time=valid.raleigh$Month_Yr), mean)
months.train.ts = ts(months.train$x,frequency = 12)

#Decompose- Actual vs. Trend. vs. Seasonal
decomp_train <- ts(data=months.train$x, start = c(2014,1),end=c(2018,6),
                frequency = 12)
model <- stl(decomp_train, s.window=7)
plot(model)

# Linear Exponential Smoothing
LES.Raleigh <- holt(months.train.ts, initial = "optimal", h=6)
summary(LES.Raleigh)
plot(LES.Raleigh)

# Damped Version
LESdamped.Raleigh <- holt(months.train.ts, initial = "optimal", damped = TRUE, h=6)
summary(LESdamped.Raleigh)
plot(LESdamped.Raleigh)


# Holt Winters
HWadditive.Raleigh <- hw(months.train.ts,seasonal="additive")
summary(HWadditive.Raleigh)
plot(HWadditive.Raleigh)

HWmult.Raleigh <- hw(months.train.ts,seasonal="multiplicative")
summary(HWmult.Raleigh)
plot(HWmult.Raleigh)

# NEED to add Labels to plots

# Testing Output
test.results=forecast(HWadditive.Raleigh,h=6)
error=months.valid$x-test.results$mean
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(months.valid$x))



