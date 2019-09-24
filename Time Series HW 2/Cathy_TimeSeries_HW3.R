# September 08, 2019
# HW 2 Time Series 
# Daily Mean PM2.5 Concentration - Particulate Matter smaller than 2.5 micrometers Forecast

library(forecast)
library(tsa)
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


# Check the stationarity ADF Test 3 lags
adf.test(ts.months.train, alternative = "stationary", k = 0) # p-value = 0.01 We have stationarity!
adf.test(ts.months.train, alternative = "stationary", k = 1) # p-value = 0.01
adf.test(ts.months.train, alternative = "stationary", k = 2) # p-value = 0.0171

# Pulling out the p-values
ADF.Pvalues <- rep(NA, 3)
for(i in 0:2){
  ADF.Pvalues[i+1] <- adf.test(ts.months.train, alternative = "stationary", k = i)$p.value
}

# Check for trending data using STL
# What is the x term
# • Order=c(0,0,0)
# ○ 1st 0 Autoregressive
# ○ 2nd 0 Integrative differences
# 3rd 0 Moving averages

# Define x with length 54 as far out the data goes
length(ts.months.train)

x=seq(1,54)

# Fitting Linear Regression
arima.trend=Arima(ts.months.train, xreg=x,order=c(0,0,0))

# Plot the residuals plot
plot(arima.trend$residuals[1:54], xlab='Number of Observations',ylab='Residuals',main='Residuals Plot',type='l')

# Stationary about the Trend because we know there is NO random walk

# Ljung-Box Test No MA or AR term#
Acf(arima.trend$residuals, lag=10,main = "")$acf
Pacf(arima.trend$residuals, lag=10, main = "")$acf

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(arima.trend$residuals, lag = i, type = "Ljung", fitdf = 0)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

# H0: White Noise, No Autocorrelation
# HA: One or more autocorrelation up to lag m are not 0

# no white noise - there is correlation that needs to be modeled, ARIMA is a good way to go

################ AR(1)  ###############
# AR(1) Model - PACF
# 3 characteristics 
# a) ACF decreases exponentially
# b) PACF  has a significant spike at the 1st lag, followed by nothing after
# c) IACF has a significant spike at the 1st lag, followed by nothing after

arima.trend1=Arima(ts.months.train, xreg=x, order=c(1,0,0))

# Forecast n.ahead = 6 forecast 6 periods
xnew=seq(55,60)
arima.forecast <- predict(arima.trend1, n.ahead=6, newxreg=xnew)

# Compare these the predicted values to the validation data set
months.valid$mean - arima.forecast$pred

# For later Calculate MAE and MAPE

# Check residuals for white noise AR(1)
# Ljung-Box Test We have white noise now#
Acf(arima.trend1$residuals, lag=10,main = "")$acf
Pacf(arima.trend1$residuals, lag=10, main = "")$acf

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(arima.trend1$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

################ MA(1)  ###############
# MA(1) Model focus on ACF
# a) Look for spikes in ACF
# b) PACF decreases exponentially as the number of lags increases
# c) IACF decreases exponentially as the number of lags increases

ma.arima1=Arima(ts.months.train, xreg=x,order=c(0,0,1))

# Check residuals for white noise MA(1)
# Ljung-Box Test We have white noise now#
Acf(ma.arima1$residuals, lag=10,main = "")$acf
Pacf(ma.arima1$residuals, lag=10, main = "")$acf

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(arima.trend1$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")
