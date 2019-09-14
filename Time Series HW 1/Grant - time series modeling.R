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

# the plot
ggplot() + 
  geom_line(aes(y=df_train$x, x=seq.Date(from = as.Date("2014-01-01"), to = as.Date("2018-06-30"), by = "month")), color = 'black', size = 1) + 
  geom_line(aes(y=df_test$`Point Forecast`, x=seq.Date(from = as.Date("2018-07-01"), to = as.Date("2018-12-31"), by = "month")),color = 'red', size = 0.9)+
  geom_line(aes(y=df_valid$x, x=seq.Date(from = as.Date("2018-07-01"), to = as.Date("2018-12-31"), by = "month")),color = 'black', size = 1) + 
  geom_vline(xintercept=as.Date("2018-06-30"), linetype="dashed", color='red') +
  labs(title = "Actual vs Holt-Winters Forecast", x="Year", y="Mean PM 2.5 Concentration (Mg/m^3)") + 
  theme_minimal() + 
  theme(legend.position = "right") +
  theme(plot.title = element_text(size=22))

df_test$`Point Forecast`
data.valid$Date
ts.months.valid



# abline(v = 2008.25, col = "red", lty = "dashed")
autoplot(HWES.data)+
  autolayer(fitted(HWES.mult.data),series="Fitted")


#MAE and MAPE
HWES.data.train <- hw(ts.months.train, seasonal = "multiplicative",initial='optimal')
train.results=forecast(HWES.data.train,h=6)

error=ts.months.valid - train.results$mean
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(ts.months.valid))

