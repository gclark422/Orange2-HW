# HW 2 #

library(haven)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)
library(ggplot2)
library(seasonal)
library(openxlsx)

################################################################################################
####################################### Data Preparation ######################################
################################################################################################
getwd()
setwd("C:/Users/97420/OneDrive/Documents/MSA Fall/Time Series/Homework/Time Series Homework 2")
PM2_5 <- read.xlsx("PM_2_5_Raleigh2.xlsx", sheet = 1)


# convert variable to date format
PM2_5$Date <- as.Date(PM2_5$Date, origin = '1899-12-30')
PM2_5$month <- format(PM2_5$Date, format = '%m')
PM2_5$year <- format(PM2_5$Date, format = '%y')


# check no missing values for monthly data
sum(is.na(monthavg$Daily.Mean.PM2.5.Concentration))

# split the data into training and validation dataset
PM2_5.train <- PM2_5[PM2_5$Date >= "2014-01-01" & PM2_5$Date <= "2018-06-30",]
PM2_5.valid <- PM2_5[PM2_5$Date >= "2018-07-01" & PM2_5$Date <= "2018-12-31",]


# aggregate to monthly data by monthly average
train <- aggregate( Daily.Mean.PM2.5.Concentration ~ month + year, PM2_5.train, mean)
valid <- aggregate( Daily.Mean.PM2.5.Concentration ~ month + year, PM2_5.valid, mean)

################################################################################################
####################################### Decomposition ######################################
################################################################################################
# chose STL because it uses local regressions to estimate trend and seasonality that allows changes
# within the decomposition componnents. In this way, we would get a more accurate description from 
# the decomposition to know what's really happenning. Contrastingly, Classical decomposition forces
# the season piece to look exactly the same.
 

# STL decomposition plot
Monthly <- ts(train$Daily.Mean.PM2.5.Concentration, start = 14, frequency =12)
decomp_stl <- stl(Monthly, s.window = 7)
plot(decomp_stl)
# Interpretation:

# Actual montly PM2.5 values overlaid with the trend/cycle component for the training set. 
plot(Monthly, col = "grey", main = "Monthly PM2.5 Values - Trend/Cycle", xlab = "", 
     ylab = "Monthly PM2.5 Values", lwd = 2)
lines(decomp_stl$time.series[,2], col = "red", lwd = 2)
# in decomposition, could pull out individual themselves 
# (time.series[,2] -- the trend itself = Tt), the trend is smoother

# Actual PM2.5 values overlaid with the seasonally adjusted PM2.5 values for the training set. 
seas_pass=Monthly-decomp_stl$time.series[,1]
plot(Monthly, col = "grey", main = "Monthly PM2.5 Values - Seasonally Adjusted", 
     xlab = "", ylab = "Monthly PM2.5 Values", lwd = 2)
lines(seas_pass, col = "red", lwd = 2)
# (time.series[,1] -- seasonally adjusted: Yt - St = Tt + Et), rougher



################################################################################################
####################################### Building Models ######################################
################################################################################################

# 1. Building a Single Exponential Smoothing Model#
SES.Monthly <- ses(Monthly, initial = "optimal", h = 12)
summary(SES.Monthly)

plot(SES.Monthly, main = "Monthly Average PM2.5 with Simple ESM Forecast", xlab = "Date", ylab = "Monthly Average PM2.5")
abline(v = 2014, col = "red", lty = "dashed")
round(accuracy(SES.Monthly),2)

autoplot(SES.Monthly)+
  autolayer(fitted(SES.Monthly),series="Fitted")+ylab("Monthly Average PM2.5 with Simple ESM Forecast")
# Goofness fit of model:
# AIC     AICc      BIC 
# 297.4907 297.9707 303.4576 
# Error measures (training):
#  ME          RMSE      MAE       MPE     MAPE      MASE      ACF1
#-0.3071732 2.022826 1.452538 -7.480275 16.70127 0.7016088 0.3215544

# Accuracy 
test.results=forecast(SES.Monthly,h=6)

error=valid$Daily.Mean.PM2.5.Concentration-test.results$mean
MAE <- mean(abs(error))
MAPE <- mean(abs(error)/abs(valid$Daily.Mean.PM2.5.Concentration))
# MAE:  1.888392
# MAPE: 0.1810493





####################################################################
########## 2. Building a Linear Exponential Smoothing Model ########
LES.Monthly <- holt(Monthly, initial = "optimal", h = 12)
summary(LES.Monthly)

plot(LES.Monthly, main = "Monthly Average PM2.5 with Linear ESM Forecast", xlab = "Date", ylab = "Monthly Average PM2.5")
abline(v = 2014, col = "red", lty = "dashed")

autoplot(LES.Monthly)+
  autolayer(fitted(LES.Monthly),series="Fitted")+ylab("Monthly Average PM2.5 with Holt ESM Forecast")

# Goodness fit of Model Output:
# AIC         AICc      BIC 
# 300.9946 302.2446 310.9395 
# Error measures (training):
#  ME          RMSE      MAE       MPE     MAPE      MASE      ACF1
#-0.09910523 2.013556 1.456021 -4.913415 16.3127 0.7032912 0.3139798

# Accuracy 
test.results=forecast(LES.Monthly,h=6)

error=valid$Daily.Mean.PM2.5.Concentration-test.results$mean
MAE <- mean(abs(error))
MAPE <- mean(abs(error)/abs(valid$Daily.Mean.PM2.5.Concentration))
# MAE:  2.200177
# MAPE: 0.2074771


#######################################################################
####### 3. Building a Linear Damping Exponential Smoothing Model ######
LDES.Monthly <- holt(Monthly, initial = "optimal", h = 12, damped = TRUE)
summary(LDES.Monthly)

plot(LDES.Monthly, main = "Monthly Average PM2.5 with Linear Damped ESM Forecast", xlab = "Date", ylab = "Monthly Average PM2.5")
abline(v = 2014, col = "red", lty = "dashed")

autoplot(LDES.Monthly)+
  autolayer(fitted(LDES.Monthly),series="Fitted")+ylab("Monthly Average PM2.5")

# Goodness fit of model:
#    AIC     AICc      BIC 
# 297.238 299.0262 309.1728  
# Error measures (training):
#  ME          RMSE      MAE       MPE     MAPE      MASE      ACF1
#-0.04533004 1.909056 1.412463 -4.373315 15.47816 0.6822517 0.3613776

# Accuracy 
test.results=forecast(LDES.Monthly,h=6)

error=valid$Daily.Mean.PM2.5.Concentration-test.results$mean
MAE <- mean(abs(error))
MAPE <- mean(abs(error)/abs(valid$Daily.Mean.PM2.5.Concentration))
# MAE:  2.008073
# MAPE: 0.1906637


#################################################################
########### 4. Building a Holt-Winters ESM - Additive ###########
HWES.Monthlyadd <- hw(Monthly, seasonal = "additive")
summary(HWES.Monthlyadd)

plot(HWES.Monthlyadd, main = "Monthly Average PM2.5 with Holt-Winters ESM Forecast", xlab = "Date", ylab = "Monthly Average PM2.5")
abline(v = 2014, col = "red", lty = "dashed")


autoplot(HWES.Monthlyadd)+
  autolayer(fitted(HWES.Monthlyadd),series="Fitted")+ylab("Monthly Average PM2.5")

# Goodness fit of Model:
#    AIC     AICc      BIC 
# 299.0722 316.0722 332.8850 
# Error measures (training):
#  ME          RMSE      MAE       MPE     MAPE      MASE      ACF1
#-0.06204349 1.583885  1.32724  -2.76048 14.35338  0.6410869  0.3854243 

# Accuracy 
test.results=forecast(HWES.Monthlyadd,h=6)

error=valid$Daily.Mean.PM2.5.Concentration-test.results$mean
MAE <- mean(abs(error))
MAPE <- mean(abs(error)/abs(valid$Daily.Mean.PM2.5.Concentration))
# MAE:  2.2477
# MAPE: 0.2285497

#########################################################################
########## 5. Building a Holt-Winters ESM - Multiplicative ##############
HWES.Monthlymul <- hw(Monthly, seasonal = "multiplicative")
summary(HWES.Monthlymul)

plot(HWES.Monthlymul, main = "#Monthly Average PM2.5 with Holt-Winters ESM Forecast", xlab = "Date", ylab = "Monthly Average PM2.5")
abline(v = 2014, col = "red", lty = "dashed")


autoplot(HWES.Monthlymul)+
  autolayer(fitted(HWES.Monthlymul),series="Fitted")+ylab("Monthly Average PM2.5")

# Model Output:
#    AIC     AICc      BIC 
# 297.5899 314.5899 331.4026 
# Error measures (training):
#  ME          RMSE      MAE       MPE     MAPE      MASE      ACF1
#-0.01635506 1.551447 1.233648 -2.501981 13.54338 0.5958798 0.375355 

# Accuracy 
test.results1=forecast(HWES.Monthlymul,h=6)

error=valid$Daily.Mean.PM2.5.Concentration-test.results1$mean
MAE <- mean(abs(error))
MAPE <- mean(abs(error)/abs(valid$Daily.Mean.PM2.5.Concentration))
# MAE:  2.029253
# MAPE: 0.2090463

