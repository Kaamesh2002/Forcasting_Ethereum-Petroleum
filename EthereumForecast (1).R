# Libraries
library(prophet)
library(lubridate)
library(ggplot2)

# Ethereum data
data <- read.csv(file.choose(), header = T)
etherdat<- "D:/SEM 3/Package stuff/Ethereum.csv"
data$Date <- dmy(data$Date)
etherdat$Date <- dmy(etherdat$Date)

# Log transformation
ds <- data$Date
y <- log(data$Close)
data$Close
df <- data.frame(ds, y)
df

#eds <- etherdat$Date
#ey <- log(etherdat$Close)
#edf <- data.frame(eds, ey)

# Forecasting with Facebook's prophet package
m <- prophet(df)
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

#ether<- prophet(df1)
#etfuture <- make_future_dataframe(ether, periods = 365)
#forecast <- predict(ether, etfuture)

# Plot forecast
plot(m, forecast)
dyplot.prophet(m,forecast)
prophet_plot_components(m, forecast)

#plot(ether, forecast)
#dyplot.prophet(ether,forecast)
#prophet_plot_components(ether, forecast)

# Model performance
pred <- forecast$yhat[1:1544]
actual <- m$history$y
plot(actual, pred) #1
abline(lm(pred~actual), col='red')
summary(lm(pred~actual))

x <- cross_validation(m, 365, units = 'days')
performance_metrics(x, rolling_window = 0.1)
plot_cross_validation_metric(x,metric = 'rmse',rolling_window = 0.2) 

#etpred <- forecast$yhat[1:1544]
#etactual <- ether$history$ey
#plot(etactual, etpred)

#etx <- cross_validation(ether, 365, units = 'days')
#performance_metrics(etx, rolling_window = 0.1)
#plot_cross_validation_metric(etx,metric = 'mae',rolling_window = 0.2)
