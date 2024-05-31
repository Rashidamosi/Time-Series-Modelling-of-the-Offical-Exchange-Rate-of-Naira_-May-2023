#Loading in all libraries as I saw in another project sample
library(ggplot2)
library(plotly)
library(ggfortify)
library(gridExtra)
library(docstring)
library(readr)
library(lubridate)
library("readxl")
library(fpp)
library(astsa)
library(devtools)
library(lubridate)
library(tidyverse)
library(nycflights13)
library(tseries)
library(forecast)
library(ggfortify)
library(Kendall)

#set working directory
setwd("C:/Users/Rashida/Desktop/My Project")
#Reading in excel file
rates <- read_excel("C:/Users/Rashida/Desktop/My Project/Rashida's Exchange rate data one sheet_noaverage.xlsx")
rates=ts(rates$exchange_rates,c(1999,1),c(2019,12),12) #Converting to time series data
rates

plot(rates)


#Unnecessary Aggregate plotting (Now giving me error leaving it)
library(ggfortify)
library(ggplot2)
allrates <- aggregate(exchange_rates~Month+Year,rates,sum)
allrates = aggregate(exchange_rates~Month+Year,rates,sum)
allrates
head(allrates)
tail(allrates)

allrates
plot(allrates)

#To decompose the data, additive & multiplicative
decomposeRes = decompose(rates, type ="add")
plot(decomposeRes)

decomposeRes = decompose(rates, type ="mult")
plot(decomposeRes)

#Autoplotting the decomposition
autoplot(decomposeRes)
#When we don't have any definite trend to our time series data, we may assume our time series will be stationary.
#when we have a definitely increasing or decreasing trend, it is that situation where the mean value can vary or the variance can vary
#But with a non-definite trend, it is reasonable to assume our time series is stationary.
#But we can also go ahead and perform the Augmented Dickey Fuller test which hypothesis say the time series are non-stationary.

#PLOTTING THE TREND LINE
Exchange_Rates = ts(rates, start = c(1999, 1), end=c(2019, 12), frequency=12) #the variable is now Exchange rates
plot(Exchange_Rates)
Exchange_Rates
#to plot lowess line
lines(lowess(time(Exchange_Rates), Exchange_Rates),lwd=3, col=2)


#Turning a non-stationary time series to a stationary time series
library(astsa)
library(forecast)
library("tseries")

#Parsing as time series data to do acf (Autocorrelation function)
class(Exchange_Rates)
#acf
#Auto correlation function and partial auto correlation function
acf(Exchange_Rates) 
pacf(Exchange_Rates)
#all of these lines cross the significant threshold of each lag.

library(astsa)
acf2(Exchange_Rates) #plotting for ACF AND PACF at the same time.

#The Augmented dickey Fuller test now
#To establish if data is stationary or non stationary, do the augmented dicky fuller test.
adf.test(Exchange_Rates)

#from our result, alternative hypothesis: stationary
#Since our data is greater than 0.05 we accept null hypothesis and conclude that our time series data is non-stationary.  
#Once we have discovered our time series is non-stationary. There are different techniques we can apply to make it stationary. 

#One of the techniques is taking first difference to make it stationary
#Now, to take difference

plot(diff(Exchange_Rates)) #first difference
#difference between value at present time and one-time previous
#t-t(-1)

#Acf for first difference
library(astsa)
acf2(diff(Exchange_Rates))
#All significant threshold has been removed after lag 1.

#If those lines cross the dotted lines, it s still highly non-stationary, when u do the first difference only
#a few lines cross it, so it becomes non- stationary.

#Now to compute adf on differenced data
adf.test(diff(Exchange_Rates))

plot(diff(Exchange_Rates), ylab='Differenced exchange rate data')

#To plot on the differenced data, This is the one we did initially. The first differencing we are plotting.
plot(diff(Exchange_Rates), ylab='Differenced exchange rate data')


#THIS PART IS JUST CODE TO MAKE DATA STATIONARY ON MEAN AND DIFFERENCE BECAUSE SOMETIMES IT IS NEEDED.
#Differencing: Make data stationary on mean/remove trend
#To plot the differenced data, This is the one we did initially. The first differencing we are plotting.
#USING LOG TO MAKE STAIONARY ON VARIANCE
#Other things to do to make data more stationary are:::
#You can use log.
#Another condition for a stationary time series is the data needs to have a constant variance.
#differencing: Make data stationary on variance
plot(log10(Exchange_Rates), ylab='Log (Exchange Rates)')

#To make data stationary on both mean and variance. Use this code.
plot(diff(log10(Exchange_Rates)), ylab='Differenced Log(Exchange Rates')


#NOW TO PERFORM THE ADF TEST AGAIN BUT ON DIFFERENCED DATA
adf.test(diff(Exchange_Rates))


#THE RIGHT WAY TO ASSIGN A VARIABLE TO MY STATIONARY DATA TO USE FOR ARIMA
Exchange_Ratesdiff = (diff(Exchange_Rates))
Exchange_Ratesl = (diff(log10(Exchange_Rates)))


###Mr. James for differenced data #############
library(fpp)
fit_arima<-auto.arima(Exchange_Ratesdiff,d=1,D=1,stepwise = FALSE,trace = TRUE)
  
#Fitting models using approximations to speed things up...
#Now re-fitting the best model(s) without approximations...
#Best model: ARIMA(0,1,2)(2,1,0)[12]                    
print(summary(fit_arima))
  
checkresiduals(fit_arima)
  
#normality
shapiro.test(residuals(fit_arima))
f<-forecast(fit_arima,h=24)
f<-forecast(fit_arima,h=60)
f
autoplot(f)




  
##############################  USING THE ARCH & GARCH VIDEO   #########################
#ARCH & GARCH ARE USED WHEN THE SSEIRES OR DATA HAS A NON-CONSTANT VARIANCE
  
#Libraries for ARCH & ARCH
rm(list = ls())
library(FinTS)   #For ARCH Test
library(rugarch)  #For Garch Models
library(e1071)
library(tseries)  #For unit root test
library(dynlm)    #For using lags in models
library(vars)     #for using VAR
library(nlWaldTest) #For testing non-linear Wald Test
library(lmtest)     #For BP Test
library(broom)      #For table presentation
library(car)        #For robust standard errors
library(sandwich)
library(knitr)
library(forecast)
library(ggplot2)
library(pdfetch) #for importing financial data
library(tsbox) 
library (stats)
library(zoo)
library (vrtest)
  
#setting Theme
theme_set(theme_bw())
  
Exchange_Rates
ts.plot(Exchange_Rates) #It has a non-constant variance from the graph
  
#To check if it is normal
hist(Exchange_Rates, main ="Histogram of exchange rates", freq = TRUE, col ="grey")
#Peak is not in the middle so it is not normal.
  
  
#To plot on the differenced data, This is the one we did initially. The first differencing we are plotting.
plot.ts(diff(Exchange_Rates), ylab='Differenced exchange rate data')
  
#Step 1: Test for normality and log transformation
shapiro.test(Exchange_Rates)

#I did the shapiro for the exchange rates directly here, but 
#The initial one I did was to fit ARIMA residuals I used..


####STEP 1: CHECK FOR STATIONARITY, & MAKE STATIONARY.. DONE THAT

#STEP 2: CHECK FOR VOLATILTY CLUSTERING:
plot.ts(Exchange_Ratesdiff)

#If small changes are followed by small changes and large 
#changes are followed by large changes. Then there is
#volatility clustering.

########## TO CHECK THE ARCH EFFECT ###########
ArchTest(Exchange_Ratesdiff)

#ARCH LM-test; Null hypothesis: no ARCH effects

#data:  Exchange_Ratesdiff
#Chi-squared = 27.709, df = 12, p-value = 0.006099

#SINCE OUR Value was less than 0.05. There is an ARCH effect.
#Alternative hypothesis: There is an arch effect. We accept that.


#To check garch order, alpha and Beta
garch(Exchange_Ratesdiff, grad="numerical", trace = FALSE)

#garch(x = Exchange_Ratesdiff, grad = "numerical", trace = FALSE)

#Coefficient(s):
  #a0         a1         b1  
#2.325e+01  1.660e-01  8.496e-13 


#HOW TO CREATE THE GARCH MODEL.
#
Exchange_Ratesdiff_garch = ugarchspec(variance.model = list(garchorder=c(1,1), mean.model = list(arima=c(2,1,0))))
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit

####Making graph to know the imapact of poitive news and negative news
#
exchange_news_garch = newsimpact(Exchange_Ratesdiff_garch_fit)
plot(exchange_news_garch$zx,exchange_news_garch$zy,ylab=exchange_news_garch$yexpr,exchange_news_garch$xexpr,main="News Impact Curve")

#Plot above not working
#Forcast the volatility for 10 days
exchange_garch_forecast = ugarchforecast(Exchange_Ratesdiff_garch_fit, n.head=10)
exchange_garch_forecast

#Forcast the volatility for 30 days
exchange_garch_forecast = ugarchforecast(Exchange_Ratesdiff_garch_fit, n.ahead=100)
exchange_garch_forecast

 ############ Redoing the graphs ############
#The command below gives you various options of graphs
plot(Exchange_Ratesdiff_garch_fit)


########### For Next Video ###########
#Step 1: Volatility Clustering
plot.ts(Exchange_Ratesdiff)

#Step 2: TESTING OF ARCH
ArchTest(Exchange_Ratesdiff)
      #RESULTS
#ARCH LM-test; Null hypothesis: no ARCH effects

#data:  Exchange_Ratesdiff
#Chi-squared = 27.709, df = 12, p-value = 0.006099

#Since p-value is less than 0.05, there is an ARCH EFFECT

#How to find out the suitable ARCH Model
garch(Exchange_Ratesdiff,grad = "numerical", trace=FALSE)
#Coefficient(s):
#a0         a1         b1  
#2.325e+01  1.660e-01  8.496e-13 

#Alpha 1 is associated with error term s-squared, Beta 1 is associated with variance of past
#So with this, it shows GARCH 1,1 model is best fir for the data and most of the times
#It has been found that Garch(1,1) is best.


#Save the output of My suitable Model
xchnage = ugarchspec(variance.model = list(garchorder=c(1,1), mean.model = list(arimaorder = c(2,1,0))))
xchnage

xchange_fit =ugarchfit(xchnage, data= Exchange_Ratesdiff)

#Forecasting for voltility
xchnage_forecast = ugarchforecast(xchange_fit, n.ahead = 20)
xchnage_forecast

#Today's Volatility

#Today Volatility = omega+alpha*yesterday squared residual*yesterday
#variance


#From the more Explantory Video with Model
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(rugarch)




#THAT COOL TUTORIAL

library(quantmod)
library(rugarch)
library(rmgarch)

chartSeries(Exchange_Ratesdiff) #cool looking graph

#UNIVARIATE GARCH MODEL:
#Before exposing the data to R u have to tell the model what u want to
#Estimate

ug_spec_exchange = ugarchspec(mean.model = list(arimaorder = c(2,1,0)))
ug_spec_exchange

ug_spec_exchange2 = ugarchspec(variance.model=list(model="iGARCH", garchorder=c(1,1),mean.model = list(arimaorder = c(2,1,0), include.mean=TRUE), distribution.model="norm", fixed.pars=list(omega=0)
ug_spec_exchange2
                                                                                                   xchnage = ugarchspec(variance.model = list(garchorder=c(1,1), mean.model = list(arimaorder = c(2,1,0))))
#Model Estimation
ug_spec_fit = ugarchfit(spec = ug_spec_exchange, data = Exchange_Ratesdiff)
ug_spec_fit

ug_spec_fit@fit$coef
ug_var <-ug_spec_fit@fit$var
ug_var
ug_res2 <- ug_spec_fit@fit$residuals
ug_res2

plot(ug_res2, type="1")
lines(ug_var, col="green")


ug_fore <- ugarchforecast(ug_spec_fit, n.head=20)
ug_fore

ug_foree <- ug_fore@forecast$sigmaFor
ug_foree
plot(ug_foree, type ="1")





##### Further in April Refitting models to check if I have correct values

#Now creating a model based on that result
#fitting ARIMA MODEL
ARIMA_exchange0 = arima(Exchange_Ratesdiff, order=c(0,1,0))
ARIMA_exchange0

ARIMA_exchangeee = arima(Exchange_Ratesdiff, order = c(0,1,0), seasonal = list(order = c(1,1,0)), method = "CSS")
ARIMA_exchangeee

                       
ARIMA_exchange = arima(Exchange_Ratesdiff, order = c(0,1,0), seasonal = list(order = c(0,1,0)), method = "CSS")
ARIMA_exchange
#AIC(ARIMA_exchange) #This wasn't working so I took the AIC from auto Arima and ran this for log and sigma

#TO GET THE SUMMARY for each model I used the code below, but for AIC I got it from the auto Arima I ran, it gave AIC values. because this summary doesn't give AIC or BIC
print(summary(ARIMA_exchange))


ARIMA_exchange1 = arima(Exchange_Ratesdiff, order = c(0,1,1), seasonal = list(order = c(1,1,0)), method = "CSS")
ARIMA_exchange1

ARIMA_exchange5 = arima(Exchange_Ratesdiff, order = c(2,1,0), seasonal = list(order = c(1,1,0)), method = "CSS")
ARIMA_exchange5
print(summary(ARIMA_exchange5))

ARIMA_exchange2 = arima(Exchange_Ratesdiff, order = c(0,1,1), seasonal = list(order = c(2,1,0)), method = "CSS")
ARIMA_exchange2

ARIMA_exchange3 = arima(Exchange_Ratesdiff, order = c(0,1,3), seasonal = list(order = c(1,1,1)), method = "CSS")
ARIMA_exchange3

ARIMA_exchange4 = arima(Exchange_Ratesdiff, order = c(2,1,0), seasonal = list(order = c(2,1,0)), method = "CSS")
ARIMA_exchange4
print(summary(ARIMA_exchange4))
checkresiduals(ARIMA_exchange4)
AIC(ARIMA_exchange4)

ARIMA_exchange6 = arima(Exchange_Ratesdiff, order = c(3,1,0), seasonal = list(order = c(0,1,0)), method = "CSS")
ARIMA_exchange6

print(summary(ARIMA_exchange6))


ARIMA_exchange7 = arima(Exchange_Ratesdiff, order = c(3,1,0), seasonal = list(order = c(1,1,0)), method = "CSS")
ARIMA_exchange7
print(summary(ARIMA_exchange7))


ARIMA_exchange8 = arima(Exchange_Ratesdiff, order = c(4,1,0),seasonal = list(order = c(1,1,0)), method = "CSS")
ARIMA_exchange8
print(summary(ARIMA_exchange8))


ARIMA_exchange9 = arima(Exchange_Ratesdiff, order = c(4,1,1), seasonal = list(order = c(0,1,0)), method = "CSS")
ARIMA_exchange9
print(summary(ARIMA_exchange9))

ARIMA_exchange10 = arima(Exchange_Ratesdiff, order = c(5,1,0), seasonal = list(order = c(0,1,0)), method = "CSS")
ARIMA_exchange10
print(summary(ARIMA_exchange10))

#Tested this for AIC alone and summary that give RMSE.
ARIMA_exchange100 = arima(Exchange_Ratesdiff, order = c(1,1,1), seasonal = list(order = c(1,1,0)), method = "CSS")
ARIMA_exchange100

print(summary(ARIMA_exchange100))

#Same model but without seasonal
ARIMA_exchange110 = arima(Exchange_Ratesdiff, order=c(1,1,1))
ARIMA_exchange110

print(summary(ARIMA_exchange10))

#Trying to see if smallest RMSE will still be for 210.
print(summary(ARIMA_exchangeee))


print(summary(ARIMA_exchange10))
#
print(summary(ARIMA_exchange9))
#
print(summary(ARIMA_exchange8))
#
print(summary(ARIMA_exchange7))
#
print(summary(ARIMA_exchange6))
#
print(summary(ARIMA_exchange5))
#
print(summary(ARIMA_exchange4))
#
print(summary(ARIMA_exchange3))
#
print(summary(ARIMA_exchange2))
#
print(summary(ARIMA_exchange1))
#
print(summary(ARIMA_exchangeee))
#
print(summary(ARIMA_exchange100))
#
print(summary(ARIMA_exchange))


print(summary(fit_arima))

########CHECKING AIC AND BIC
