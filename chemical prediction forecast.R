setwd("C:\\Users\\Aakanksha\\Desktop\\time series forecasting case study")
getwd()

sub<-read.csv("DemandSubmission.csv")
test<-read.csv("DemandTest.csv")
train<-read.csv("DemandTrain.csv")

library(lubridate)
##inspecting the data ##
View(sub)
str(sub)

View(test)
str(test)

View(train)
str(train)

##converting it to the date format##
train$date=as.Date(train$date,"%d-%m-%Y")
class(train$date)
library(tseries)
##converting dataset in time series set
ts=ts(train$mean_demand_kiloGallon,start=c(2014,01,1),end=c(2018,01,1),frequency=365.25)

##ADF,PACF##
adf.test(ts)
plot(ts)
par(mfrow=c(1,2))
acf(ts,main="ACF")
pacf(ts,main="PACF")
tsl<-diff(ts,differences = 1)
adf.test(tsl)
plot(tsl)
acf(tsl,main="ACF")
pacf(tsl,main='PACF')
library(timetk)
library(forecast)
arima<-Arima(y=ts,order=c(1,2,1),
             seasonal =c(0,1,0))
arima
futForecast<-forecast(arima, h=113)
str(futForecast)
plot(futForecast)
dev.off()
plot(futForecast)

arima1<-SARIMA(y=tsl,order=c(3,2,1),
              seasonal = c(0,1,0))
arima1

futForecast1 <- forecast(arima1, h=113)
str(futForecast)
plot(futForecast)

arima2<-arima(y=ts,order=c(2,1,3),
              seasonal = c(0,0,1))
arima2

futForecast2 <- forecast(arima2, h=113)
str(futForecast2)
plot(futForecast2)

arima3<-arima(y=tsl,order=c(1,0,3),
              seasonal = c(1,0,1))
arima3

futForecast3 <- forecast(arima3, h=113)
str(futForecast3)
plot(futForecast3)

head(futForecast$mean,30)

test$meanChemDemand<-(futForecast$mean)
sub$mean_demand_kiloGallon <- test$meanChemDemand
submission1 <- sub
write.csv(submission1,"newcasestudysubmission_C.csv",row.names = FALSE)
