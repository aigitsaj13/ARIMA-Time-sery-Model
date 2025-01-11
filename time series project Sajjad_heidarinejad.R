#load necessary libraries
library(tseries)
library(forecast)

#load data
data1 <-read.csv("C:\\Users\\2sajj\\OneDrive\\Desktop\\Sample-Spreadsheet-100-rows.csv")
data <- ts(data1)


#plot time series
plot(data, main = "Time Series Plot",ylab='Electric_Production')

#show all components of time series
plot(decompose(data))

#Difference data
diff_data<-diff(data)

# Plot differenced data
plot(diff_data, main = "Differenced Time Series plot")


# Decompose differenced data
decomp <- decompose(diff_data)
plot(decomp)


# Deseasonalize data
data_statinary <- decomp$x - decomp$seasonal
plot(data_statinary)


#fit the best ARIMA model
fit <- auto.arima(data_statinary)
summary(fit)

#Extract fitted coefficients
coefs <- coef(fit)
print(coefs)


#Plot time series and fitted values
plot(data_statinary, col = "black", main = "Deseasonalized Differenced Time Series and Fitted ARIMA Model")
lines(fitted(fit), col = "red")


#plot ACF an PACF
acf(data_statinary, main = "ACF Plot")
pacf(data_statinary, main = "PACF Plot")


#forecast future values
forecast(fit, h=10)