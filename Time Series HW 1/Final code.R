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

file <- "C:\\Users\\grant\\Desktop\\IAA\\Orange2-HW\\Data\\Time Series HW 1\\PM_2_5_Raleigh2.csv"
data <- read.csv(file=file, header=TRUE, sep=",")

data$Date <- as.Date(data$Date, format="%m/%d/%Y", origin = "01/01/2014")

# Separate in training and validation
data.train <- data[data$Date >= "2014-01-01" & data$Date <= "2018-06-30",]
data.valid <- data[data$Date >= "2018-07-01" & data$Date <= "2018-12-31",]

# Aggregate by months
months <- data.train %>%
  group_by(year=year(Date), month=month(Date)) %>%
  summarise(mean=mean(Daily.Mean.PM2.5.Concentration))

plot(months)

ts.months.train <- ts(months$mean, start=14, frequency = 12)
plot(ts.months.train)

months.valid <- data.valid %>%
  group_by(year=year(Date), month=month(Date)) %>%
  summarise(mean=mean(Daily.Mean.PM2.5.Concentration))

ts.months.valid <- ts(months.valid$mean, start = 14, frequency = 12)

# Decomposition
decomp_stl <- stl(ts.months.train, s.window=7)
plot(decomp_stl)

# ESM
LES.data <- holt(ts.months.train, inital="optimal", h=6)
summary(LES.data)

LDES.data <- holt(ts.months.train, inital="optimal", h=6, damped = TRUE)
summary(LDES.data)
plot(LDES.data)

# Building a Holt-Winters ESM 
HWES.data <- hw(ts.months.train, seasonal = "additive")
summary(HWES.data)

plot(HWES.data, main = "Holt-Winters ESM Forecast", xlab = "Date")
# abline(v = 2008.25, col = "red", lty = "dashed")


autoplot(HWES.data)+
  autolayer(fitted(HWES.data),series="Fitted")


HWES.mult.data <- hw(ts.months.train, h=0.5*frequency(ts.months.train), seasonal = "multiplicative")
summary(HWES.mult.data)

plot(HWES.mult.data, main = "Holt-Winters ESM Forecast - multiplicative", xlab = "Date")

plot(HWES.mult.data, include = 1, PI=FALSE, main = "Holt-Winters ESM Forecast - multiplicative", xlab = "Date")
lines(ts.months.valid)

plot(ts.months.valid)

df_test <- as.data.frame(HWES.mult.data)
df_test

w <- ts(HWES.mult.data)
plot(w)

ts.plot(w, gpars = list(col = c("black", "red")))

HWES.mult.data$fitted

df_valid <- as.data.frame(ts.months.valid)
df_train <- as.data.frame(ts.months.train)

# The plot for actual vs. holt-winters 
ggplot() + 
  geom_line(aes(y=df_test$`Point Forecast`, x=seq.Date(from = as.Date("2018-07-01"), to = as.Date("2018-12-31"), by = "month")),color = 'red', size = 0.9)+
  geom_line(aes(y=df_valid$x, x=seq.Date(from = as.Date("2018-07-01"), to = as.Date("2018-12-31"), by = "month")),color = 'black', size = 1) +
  labs(title = "Actual vs Holt-Winters Forecast", x="Month", y="Mean PM 2.5 Concentration (mg/m^3)") + 
  theme_minimal() + 
  theme(legend.position = "right") +
  theme(plot.title = element_text(size=22))

# The original data
ggplot() + 
  geom_line(aes(y=df_train$x, x=seq.Date(from = as.Date("2014-01-01"), to = as.Date("2018-06-30"), by = "month")), color = 'black', size = 0.9) + 
  labs(title = "Mean PM 2.5 Concentration over time", x="Year", y="Mean PM 2.5 Concentration (mg/m^3)") + 
  theme_minimal() + 
  theme(plot.title = element_text(size=22))

df_test$`Point Forecast`
data.valid$Date
ts.months.valid



# abline(v = 2008.25, col = "red", lty = "dashed")
autoplot(HWES.data)+
  autolayer(fitted(HWES.mult.data),series="Fitted")

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
plot(ts.months.train, col = "grey", main = "Monthly PM2.5 Values - Trend/Cycle", xlab = "Year", ylab = "Mean PM 2.5 Concentration (mg/m^3)", lwd = 2)
lines(decomp_stl$time.series[,2], col = "red", lwd = 2)

# Actual PM2.5 values overlaid with the seasonally adjusted PM2.5 values for the training set.
seas_pm=ts.months.train-decomp_stl$time.series[,1]
plot(ts.months.train, col = "grey", main = "Monthly PM2.5 Values Seasonally Adjusted", xlab = "Year", ylab = "Mean PM 2.5 Concentration (mg/m^3)", lwd = 2)
lines(seas_pm, col = "red", lwd = 2)

# Sub-series
monthplot(decomp_stl$time.series[,"seasonal"], main = "PM 2.5 - Monthly Effects", ylab = "PM 2.5 Sub-Series", xlab = "PM 2.5 (Months)", lwd = 2)

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
