# Time series data
data('AirPassengers')
AP <- AirPassengers
str(AP)


head(AP)
ts(AP, frequency = 12, start = c(1949,1))

attributes(AP)

plot(AP)

# Log transform
AP <- log(AP)
plot(AP)

# decomposition of additive time series
decomp <- decompose(AP)
decomp$figure
plot(decomp$figure,
     type = 'b',
     xlab = 'Month',
     ylab = 'Sesaonality Index',
     col = 'blue',
     las = 2)

plot(decomp)

# ARIMA
library(forecast)
model <- auto.arima(AP)
attributes(model)

# ACF and PACF plots
acf(model$residuals, main = 'Correlogram')
pacf(model$residuals, main = 'Partial Correlogram')

# Ljung-Box test
Box.test(model$residuals, lag = 20, type = 'Ljung-Box')

# Residual plot
hist(model$residuals,
     col = 'red',
     xlab = 'Error',
     main = 'Histogram of residuals',
     freq = FALSE)
lines(density(model$residuals))

# Forecast
f <- forecast(model, 48)
library(ggplot2)
autoplot(f)
accuracy(f)

