rm(list=ls())
library(tseries)
library(forecast)
library(zoo)
library(xts)
library(TSA)

#0. Load Monthly Close Price of Stock Market
#Seperate Data into Training and Testing
Monthly <- read.csv(file="C:/Users/Jeremy/Nextcloud/SYDE631 - Time Series/Project/Financial - Report/Monthly-DJA.csv",
                  header=TRUE, sep=",")
m <- xts(Monthly[,c(1,6)][,-1], order.by=as.Date(Monthly[,c(1,6)][,1], "%m/%d/%Y"))
MTS_W<-ts(m,start=c(2014,11),end=c(2019,10),frequency=12);
mTS <- ts(m,start=c(2014,11),end=c(2018,10),frequency=12);
mTS_test <- ts(m,start=c(2018,11),end=c(2019,10),frequency=12);

#1. Ploting and Decomposition => Statistical Properties
plot(mTS,main='Monthly Changes of DJA',ylab='Open Price');

#==plot components of time sereis
comps=decompose(mTS, filter = NULL)
plot(comps)
#==ACF and PACF
acf(m);
pacf(m);
#==Test Stationary
adf.test(m);
mTSclean <- tsclean(mTS)
plot(mTS)
plot(mTSclean)

#==differencing is needed.
mTSD<-diff(mTS, lag = 1);
mD<-diff(m,lag=1);
mD <- mD[-1,]

#==Periodicity
f=fft(mTSD, inverse = TRUE)
plot(f);

plot(mTSD);


#1.2 Ploting the differecing time series and statistical properteis
plot(mTSD);
acf(mD,lag.max=48)
#==Test Stationality
adf.test(mTSD);#H0=non-stationary => p>0.05=non-stationary;
#==normalization test
shapiro.test(mTSD);#If p> 0.05, normality can be assumed.
acf(mD,lag.max=25);
pacf(mD,lag.max=25);

qqnorm(mTSD);
qqline(mTSD); 


#2 Model Estimation
estimate1 <- Arima(mTS, order = c(0,1,0),include.drift = TRUE) 
estimateOver <- Arima(mTS, order = c(0,1,8),include.drift = TRUE) 
estimate2 <- Arima(mTS, order = c(1,1,0),include.drift = TRUE) 
estimate3 <- Arima(mTS, order = c(0,1,1),include.drift = TRUE) 
estimate4 <- Arima(mTS, order = c(1,1,1),include.drift = TRUE) 
estimate5 <- Arima(mTS, order = c(1,1,2),include.drift = TRUE) 
estimate6 <- Arima(mTS, order = c(2,1,2),include.drift = TRUE) 
summary(estimate1);
summary(estimate2);
summary(estimate3);
summary(estimate4);
summary(estimate5);
summary(estimate6);

#3 Diagnose Test: Residual=>Normality; Independent; White Noise;
library(forecast)
residual<- estimateOver$residuals
res = as.numeric(residual)
#==Independent
acf(res,lag.max=25);
#==Test Stationality
adf.test(res);#H0=non-stationary => p>0.05=non-stationary;
#==normalization test
shapiro.test(res);#H0=normality => If p>0.05, normality can be assumed.
qqnorm(res);
qqline(res); 
#==The Parameters of Models



#4. Forecast
#Forecast
#==ARIMA(0,1,0)
foreMonthly <- forecast(estimate1, h = 12)
plot(foreMonthly,ylim=c(5500,9000))
par(new = TRUE)
#MT_test <- window(MTS_W,c(2018, 11), c(2019, 10))
plot(MTS_W,ylim=c(5500,9000))

#Forecast
#==ARIMA(0,1,8)
foreMonthly <- forecast(estimateOver, h = 12);
plot(foreMonthly,ylim=c(5500,9000));
par(new = TRUE);
#MT_test <- window(MTS_W,c(2018, 11), c(2019, 10))
plot(MTS_W,ylim=c(5500,9000));

#Forecasting Result Comparison
fore <- forecast(estimate1, h = 12);
forecast<-as.numeric(fore$mean);
test = as.numeric(mTS_test);
accuracy(forecast,test);

fore <- forecast(estimateOver, h = 12);
forecast<-as.numeric(fore$mean);
accuracy(forecast,test);


#5. Discussion: Seasonality 
#Differencing of Seasonal
#==p=12
mTSDS<-diff(mTSD,lag = 12);
mDS<-diff(mD,lag=12);
mDS=mDS[13:47,]
adf.test(mTSDS);#H0=non-stationary => p>0.05=non-stationary;
shapiro.test(mTSDS);#If p> 0.05, normality can be assumed.
acf(mDS,lag.max=25);
pacf(mDS,lag.max=25);

fitP12 <- Arima(mTS, order = c(0,1,0),
                seasonal = list(order = c(0,1,0),period=12),
                include.drift = TRUE)
summary(fitP12);

#==p=6
mTSDS<-diff(mTSD,lag = 6);
mDS<-diff(mD,lag=6);
mDS=mDS[7:47,]
adf.test(mTSDS);#H0=non-stationary => p>0.05=non-stationary;
shapiro.test(mTSDS);#If p> 0.05, normality can be assumed.
acf(mDS,lag.max=25);
pacf(mDS,lag.max=25);

fitP6 <- Arima(mTS, order = c(0,1,0),
               seasonal = list(order = c(0,1,0),period=6),
               include.drift = TRUE);
summary(fitP6);


#==p=4
mTSDS<-diff(mTSD,lag = 4);
mDS<-diff(mD,lag=4);
mDS=mDS[5:47,]
adf.test(mTSDS);#H0=non-stationary => p>0.05=non-stationary;
shapiro.test(mTSDS);#If p> 0.05, normality can be assumed.
acf(mDS,lag.max=25);
pacf(mDS,lag.max=25);

fitP4 <- Arima(mTS, order = c(0,1,0),
               seasonal = list(order = c(1,1,1),period=4),
               include.drift = TRUE)
summary(fitP4);
#==p=18
mTSDS<-diff(mTSD,lag = 18);
mDS<-diff(mD,lag=18);
mDS=mDS[19:47,]
adf.test(mTSDS);#H0=non-stationary => p>0.05=non-stationary;
shapiro.test(mTSDS);#If p> 0.05, normality can be assumed.
acf(mDS,lag.max=25);
pacf(mDS,lag.max=25);

fitP18 <- Arima(mTS, order = c(0,1,0),
               seasonal = list(order = c(0,1,0),period=18),
               include.drift = TRUE)
summary(fitP18);


#==Plotting Forecast Result
#Forecast==fitP4
foreMonthly <- forecast(fitP4, h = 12)
plot(foreMonthly,ylim=c(5500,9000))
par(new = TRUE)
#MT_test <- window(MTS_W,c(2018, 11), c(2019, 10))
plot(MTS_W,ylim=c(5500,9000))

#Forecast==fitP6
foreMonthly <- forecast(fitP6, h = 12)
plot(foreMonthly,ylim=c(5500,9000))
par(new = TRUE)
#MT_test <- window(MTS_W,c(2018, 11), c(2019, 10))
plot(MTS_W,ylim=c(5500,9000))

#Forecast==fitP12
foreMonthly <- forecast(fitP12, h = 12)
plot(foreMonthly,ylim=c(5500,10000))
par(new = TRUE)
#MT_test <- window(MTS_W,c(2018, 11), c(2019, 10))
plot(MTS_W,ylim=c(5500,10000))

#Forecast==fitP18
foreMonthly <- forecast(fitP18, h = 12)
plot(foreMonthly,ylim=c(5500,10000))
par(new = TRUE)
#MT_test <- window(MTS_W,c(2018, 11), c(2019, 10))
plot(MTS_W,ylim=c(5500,10000))


#Forecasting Result Comparison
fore4 <- forecast(fitP4, h = 12);
fc4<-as.numeric(fore4$mean);
accuracy(fc4,test);

fore6 <- forecast(fitP6, h = 12);
fc6<-as.numeric(fore6$mean);
accuracy(fc6,test);

fore12 <- forecast(fitP12, h = 12);
fc12<-as.numeric(fore12$mean);
accuracy(fc12,test);

fore18 <- forecast(fitP18, h = 12);
fc18<-as.numeric(fore18$mean);
accuracy(fc18,test)