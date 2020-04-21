library(ggplot2)
library(quantmod)
library(ggplot2)
library(fpp)
library(fpp2)

start_date <- as.Date("2012-01-01")
end_date <- as.Date("2018-01-01")
start_date
end_date

lapply(start_date, class)
lapply(end_date, class)

getSymbols("AAPL", src = "yahoo", from = start_date, to = end_date)
summary(AAPL)
head(AAPL)
names(AAPL)
data <- ts(AAPL,start=c(2012,1),end=c(2018,12), frequency = 12)
data=data[,3]

##############Train and Test data##############
train_data = window(data,start=c(2012,1), end=c(2016,12))
test_data = window(data,start=c(2017,1), end=c(2018,12))
train_data
test_data

autoplot(data) + ggtitle("Apple stock price") + ylab("$ million") + xlab("Year")
ggsubseriesplot(data) + ylab("$ million") + ggtitle("Seasonal subseries plot:Apple stock price")

Acf(data, lwd=5, main="Apple stock price")

##############Mean, Naive, Seasonal Naive##############
fit.mean=meanf(train_data,h=12)
fit.naive=naive(train_data,h=12)
fit.snaive=snaive(train_data,h=12)

autoplot(train_data) +
  autolayer(meanf(train_data, h=12),
            series="Mean", PI=FALSE) +
  autolayer(naive(train_data, h=12),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(train_data, h=12),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts Apple stock price") +
  xlab("Year") + ylab("$ millions") +
  guides(colour=guide_legend(title="Forecast"))

##############Linear Trend##############
linear_reg <- tslm(train_data ~ trend)
fit.tslm1=forecast(linear_reg, h=12)
summary(fit.tslm1)
plot(fit.tslm1, ylab="Apple stock price",
     xlab="t")

##############Season + Trend##############
linear_season <- tslm(train_data ~ trend + season)
fit.tslm2=forecast(linear_season, h=12)
summary(fit.tslm2)
plot(fit.tslm2, ylab="Apple stock price",
     xlab="t")


##############STL decomposition##############
stl_decomp <- stl(train_data, t.window=12, s.window="periodic")
plot(stl_decomp)

fit.stl <- forecast(stl_decomp,h=12)
summary(fit.stl)
plot(fit.stl)

##############ETS##############
library(forecast)
ets_forecast<-ets(train_data)
fit.ets_forecast <- forecast(ets_forecast)

##############SES##############
fit.ses <- ses(train_data, h = 12)
fit.ses <- forecast(fit.ses)
summary(fit.ses)
plot(fit.ses)

fit1 <-ses(train_data, alpha=0.2, initial="simple", h=3)
fit2 <-ses(train_data, alpha=0.6, initial="simple", h=3)
fit3 <-ses(train_data, h=3)
plot(fit1,main="Apple stock price", ylab="$
     (millions)", xlab="Year", fcol="white", type="o")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft",lty=1, col=c(1,"blue","red","green"),
       c("data", expression(lambda == 0.2), expression(lambda == 0.6),
         expression(lambda == 0.89)),pch=1)


##############HW Additive and Multiplicative##############
fit1_add <- hw(train_data,seasonal="additive")
fit1_add <- forecast(fit1_add)
fit2_multi <- hw(train_data,seasonal="multiplicative")
fit2_multi <- forecast(fit2_multi)
autoplot(train_data) +
  autolayer(fit1_add, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2_multi, series="HW multiplicative forecasts", PI=FALSE) +
  xlab("Year") +
  ylab("$ (millions)") +
  ggtitle("Apple stock price") +
  guides(colour=guide_legend(title="Forecast"))


##############Holt's Linear trend##############
fit.hlinear <- holt(train_data, h=3)
fit.hlinear <- forecast(fit.hlinear)
summary(fit.hlinear)
plot(fit.hlinear, main = "Holt's Linear Trend")
lines(train_data)

##############Check stationarity##############
ndiffs(train_data)
y.diff1 = diff(train_data, differences = 1)
adf.test(y.diff1, alternative = "stationary")
plot(y.diff1)

Acf(train_data)
Pacf(train_data)

###############Auto ARIMA##############
y.arima <- auto.arima(train_data)
fit.arima <- forecast(y.arima, h=12)
summary(fit.arima)
plot(fit.arima)


###############Comparison##############
a.mean=accuracy(fit.mean,test_data)
a.naive=accuracy(fit.naive,test_data)
a.snaive=accuracy(fit.snaive,test_data)
a.linear=accuracy(fit.tslm1,test_data)
a.linear_season=accuracy(fit.tslm2,test_data)
a.ets=accuracy(fit.ets_forecast,test_data)
a.ses=accuracy(fit.ses, test_data)
a.stl=accuracy(fit.stl, test_data)
a.holt=accuracy(fit.hlinear, test_data)
a.multi=accuracy(fit1_add, test_data)
a.add=accuracy(fit2_multi, test_data)
a.arima=accuracy(fit.arima, test_data)


a.table<-rbind(a.mean, a.naive, a.snaive, a.linear, a.linear_season, a.ets, a.ses, a.stl, a.holt, a.add, a.multi, a.arima)
a.table
row.names(a.table)<-c('Mean training','Mean test', 'Naive training', 'Naive test', 'S. Naive training', 'S. Naive test' ,'Linear training', 'Linear test','season-trend training', 'season-trend test', 'ets training', 'ets test',"ses training", "ses test",'STL training', 'STL test',"Holt's Linear training", "Holt's Linear test", 'Add training', 'Add test','Multi training', 'Multi test','ARIMA training', 'ARIMA test')

###############Tabular format##############
a.table<-as.data.frame(a.table)
a.table

