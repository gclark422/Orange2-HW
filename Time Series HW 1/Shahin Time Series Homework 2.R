#------------------------------------#
#        Exponential Smoothing       #
#               Models               #
#                                    #
#           Dr Susan Simmons         #
#------------------------------------#

# Needed Libraries for Analysis #
install.packages('lubridate')
library(haven)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)
library(ggplot2)
library(lubridate)
library(plyr)

# Saving File Locations and Uploading SAS File #
file.dir <- "C:\\Users\\Asus\\Documents\\Learning\\Advanced Analytics\\Time Series and Forecasting\\Homework\\Homework 2\\PM_2_5_Raleigh2.csv"
hw2 <- read.csv(file.dir)

View(hw2)

#Aggregating the data by month
hw2$Date2 <- as.Date(hw2$Date, "%m/%d/%Y")
mo <- strftime(hw2$Date2, "%m")
yr <- strftime(hw2$Date2, "%Y")
dd <- data.frame(mo, yr, hw2$Daily.Mean.PM2.5.Concentration)

View(dd)

dd.agg <- aggregate(hw2.Daily.Mean.PM2.5.Concentration ~ mo + yr, dd, FUN = mean)
dd.agg$date <- as.POSIXct(paste(dd.agg$yr, dd.agg$mo, "01", sep = "-"))

View(dd.agg)


#Split the data
data.train <- dd.agg[dd.agg$date <= "2018-06-1",]

data.valid <- dd.agg[dd.agg$date > "2018-06-01",]

View(data.valid)

# Creating Time Series Data Objects #
Daily_Mean_Train <- ts(data.train$hw2.Daily.Mean.PM2.5.Concentration, start = 2014, frequency = 12)

Daily_Mean_Valid <- ts(data.valid$hw2.Daily.Mean.PM2.5.Concentration, start = 2018, frequency = 12)

View(Daily_Mean_Valid)

# STL Decomposition
decomp_stl <- stl(Daily_Mean_Train, s.window = 7)
plot(decomp_stl)

# Plot of the data with the trend line, STL
plot(Daily_Mean_Train, col = "grey", main = "PM2.5 - Trend/Cycle", xlab = "", ylab = "PM2.5", lwd = 2)
lines(decomp_stl$time.series[,2], col = "red", lwd = 2)

# Plot of the data with the seasonally adjusted overlay, STL
seas_pass = Daily_Mean_Train - decomp_stl$time.series[,1]
plot(Daily_Mean_Train, col = "grey", main = "PM2.5 - Seasonally Adjusted", xlab = "", ylab = "PM2.5", lwd = 2)
lines(seas_pass, col = "red", lwd = 2)

# Month plot. Cool stuff!
monthplot(decomp_stl$time.series[,"seasonal"], main = "PM2.5 - Monthly Effects", ylab = "Seasonal Sub Series", xlab = "Seasons (Months)", lwd = 2)


# Holt-Winters Additive ESM
HWES_PA <- hw(Daily_Mean_Train, seasonal = "additive")
train_results <- forecast(HWES_PA, h=6)
summary(HWES_PA)

View(train_results)
View(Daily_Mean_Valid)

plot(HWES.PA, main = "PM2.5 with Holt-Winters ESM Forecast", xlab = "Date", ylab = "PM2.5")

autoplot(HWES.PA)+ autolayer(fitted(HWES.PA),series="Fitted")+ylab("PM2.5 with Holt-Winters ESM Forecast")

# Running MAPE on the Validation Data Set

DMV <- unclass(Daily_Mean_Valid)
TRM <- unclass(train_results$mean)
error = DMV - TRM
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(DMV))

print(MAE)    #2.25
print(MAPE)   #22.85%




# Holt-Winters Multipliciative ESM

HWES_PM <- hw(Daily_Mean_Train, seasonal = "multiplicative")
train_results <- forecast(HWES_PM, h=6)
summary(HWES_PM)

View(train_results)
View(Daily_Mean_Valid)

plot(HWES.PM, main = "PM2.5 with Holt-Winters ESM Forecast", xlab = "Date", ylab = "PM2.5")

autoplot(HWES.PM)+ autolayer(fitted(HWES.PM),series="Fitted")+ylab("PM2.5 with Holt-Winters ESM Forecast")


# Running MAPE on the Validation Data Set

DMV <- unclass(Daily_Mean_Valid)
TRM <- unclass(train_results$mean)
error = DMV - TRM
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(DMV))

print(MAE)   # 2.029
print(MAPE)  # 20.90%
