
beerData = read.csv("beer.csv")
install.packages("tseries")
install.packages("astsa")
install.packages("fpp2")
install.packages("TSA")
library(tseries)
library(astsa)
library(ggplot2)
library(fpp2)
library(TSA)
str(beerData)
summary(beerData)
tsObj <- ts(beerData, start = 2000, frequency = 4, end=2018)
summary(tsObj)
str(tsObj)
is.ts(tsObj)
##  Time-Series [1:73] from 2000 to 2018: 284 213 227 308 262 ...
ts.plot(tsObj, col="red" ,main="Beer Sales trend", xlab="Years", ylab="Sales", type="b")
frequency(tsObj)
cycle(tsObj)

ggsubseriesplot(tsObj,facets=TRUE)


boxplot(tsObj~cycle(tsObj), main="Sales trend between quarters over the years")
ggseasonplot(tsObj,facets=TRUE,polar = TRUE)
kpss.test(tsObj)
acf(tsObj)
acf(diff(log(tsObj)))

hw_beer <- hw(tsObj,seasonal = "additive")
summary(hw_beer)
autoplot(forecast(hw_beer,n.ahead=8))

Decomp_states=hw_beer$model$states[,1:3]
colnames(Decomp_states)= cbind('level','trend','seasonality')
plot(Decomp_states,col="red",main = "Decompose - forecast")

checkresiduals(hw_beer)
ExpSm <- ets(tsObj)
ExpSm
ExpSm %>% forecast(h=8) %>% autoplot()
pacf(diff(log(tsObj)))
acf(diff(log(tsObj)))
beer_arima1 <- arima(log(tsObj),c(0,1,2),seasonal = list(order = c(0,1,2),period = 4))
beer_arima1
beer_arima2 <- arima(log(tsObj),c(0,1,1),seasonal = list(order = c(0,1,1),period = 4))
beer_arima2
beer_arima3 <- arima(log(tsObj),c(1,1,0),seasonal = list(order = c(1,1,0),period = 4))
beer_arima3
beer_arima4 <- arima(log(tsObj),c(1,1,1),seasonal = list(order = c(1,1,1),period = 4))
beer_arima4
beer_arima5 <- arima(log(tsObj),c(1,1,2),seasonal = list(order = c(1,1,2),period = 4))
beer_arima5

acf(residuals(beer_arima1))
pacf(residuals(beer_arima1))
Box.test(residuals(beer_arima1),lag=4,type="Ljung")
pred_arima <- predict(beer_arima1, n.ahead = 8)
ts.plot(tsObj,2.718^pred_arima$pred, lty = c(1,3),ylabel ="beer",main="Forecasted Series")
fhw <- function(x,h)
{
  forecast(hw(x),h=h)
}

farima <- function(x,h)
{
  forecast(auto.arima(x),h=h)
}
error1 <- tsCV(tsObj,fhw,h=1)
error2 <- tsCV(tsObj,farima,h=1)
mean(error1^2, na.rm=TRUE)
mean(error2^2, na.rm=TRUE)
error1
error2
beer_arima1 = arima((tsObj), c(0,1, 2),seasonal = list(order = c(0, 1, 2), period = 4, include.constant=FALSE))
beer_arima1
beer_arima2 = arima((tsObj), c(0,1, 1),seasonal = list(order = c(0, 1, 1), period = 4, include.constant=FALSE))
beer_arima2
beer_arima3 = arima((tsObj), c(1,1, 0),seasonal = list(order = c(1, 1, 0), period = 4, include.constant=FALSE))
beer_arima3
beer_arima4 = arima((tsObj), c(1,1, 1),seasonal = list(order = c(1, 1, 1), period = 4, include.constant=FALSE))
beer_arima4
beer_arima5 = arima((tsObj), c(1,1, 2),seasonal = list(order = c(1, 1, 2), period = 4, include.constant=FALSE))
beer_arima5
acf(residuals(beer_arima1))
pacf(residuals(beer_arima1))
Box.test(residuals(beer_arima1),lag=4,type="Ljung")
pred_arima1 <- predict(beer_arima1, n.ahead = 8)
ts.plot(tsObj,2.718^pred_arima1$pred, lty = c(1,3),ylabel ="beer",main="Forecasted Series")


pred <- predict(beer_arima1, n.ahead = 8)
ts.plot(tsObj,2.718^pred$pred, lty = c(1,3),ylabel ="beer",main="Forecasted Series")
auto.arima(tsObj,stepwise=FALSE) %>% forecast(h=8) %>% autoplot()
