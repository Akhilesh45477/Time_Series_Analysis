View(Primary_Energy)

names(Primary_Energy)
class(Primary_Energy)
library(tseries)
library(forecast)
ts_p<-ts(Primary_Energy$`Primary energy cunsumption(exajoules)`,start = 1998,end = 2022, frequency = 1)
class(ts_p)
start(ts_p)
end(ts_p)
summary(ts_p)
plot(ts_p)
#testing the ts 
#testing for stationary
#1) augmented dicky-fuller(ADF)test:Ho data is not stationary
#if p value less than 5% then we reject null hypothesis

#2) KPSS test: Ho- data is stationary
#if p value less tha 5% then we reject null hypothesis

#if eithe of the 2 test suggests there is non stationary,reject

#need to check both test
#use BJ sales
adf.test(ts_p)
kpss.test(ts_p)
# here we do not reject H0.it means data is not stationary
#TO MAKE staionary series
ts_p_1<-diff(ts_p,differences = 1)
ts_p_2<-diff(ts_p,differences = 2)
ts_p_3<-diff(ts_p,differences = 3)
par(mfrow=c(2,2))
plot(ts_p)
plot(ts_p_1)
plot(ts_p_2)
plot(ts_p_3)
par(mfrow=c(1,1))
acf(ts_p)
acf(ts_p_1)
acf(ts_p_2)
acf(ts_p_3)
#crosschecking with tests
adf.test(ts_p_1)
kpss.test(ts_p_1)

adf.test(ts_p_2)
kpss.test((ts_p_2))

adf.test(ts_p_3)
kpss.test(ts_p_3)
par(mfrow=c(1,1))
acf(ts_p_2)
pacf(ts_p_2)
##Auto Arima
fit=auto.arima(ts_p_2,trace=TRUE,
               test="adf",ic="aicc",approximation = FALSE)
#fitting the arima model
arima_1<-arima(ts_p_2,order = c(2,0,1))
summary(arima_1)
# forecast the arima model
forecasted.data=forecast(arima_1,h=10)
plot(forecasted.data,col="red")


Box.test(arima_1$residuals,type="Ljung-Box")
pacf(arima_1$residuals)

