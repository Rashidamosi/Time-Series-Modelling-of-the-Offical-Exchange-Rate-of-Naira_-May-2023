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


###Mr. James code for differenced data #############
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


acf(fit_arima) 


##############################  USING THE ARCH & GARCH VIDEO   #########################
#ARCH & GARCH ARE USED WHEN THE SEIRES OR DATA HAS A NON-CONSTANT VARIANCE

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

### Recalling variables so this part stops stressing me:
#set working directory
setwd("C:/Users/Rashida/Desktop/My Project")
#Reading in excel file
rates <- read_excel("C:/Users/Rashida/Desktop/My Project/Rashida's Exchange rate data one sheet_noaverage.xlsx")
rates=ts(rates$exchange_rates,c(1999,1),c(2019,12),12) #Converting to time series data
rates

Exchange_Rates = ts(rates, start = c(1999, 1), end=c(2019, 12), frequency=12) #the variable is now Exchange rates
plot(Exchange_Rates)
Exchange_Rates

plot(diff(Exchange_Rates), ylab='Differenced exchange rate data')

#THE RIGHT WAY TO ASSIGN A VARIABLE TO MY STATIONARY DATA TO USE FOR ARIMA
Exchange_Ratesdiff = (diff(Exchange_Rates))


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


#To check garch order, alpha and Beta and to see which model is suitable.
garch(Exchange_Ratesdiff, grad="numerical", trace = FALSE)

#garch(x = Exchange_Ratesdiff, grad = "numerical", trace = FALSE)

#Coefficient(s):
#a0         a1         b1  
#2.325e+01  1.660e-01  8.496e-13 



#HOW TO CREATE THE GARCH MODEL.
###################
#FIRST ITEM IN C() IS GARCH EFFECT AND eSECOND IS ARCH EFFECT.
Exchange_Ratesdiff_garch = ugarchspec(variance.model = list(garchorder=c(1,1), mean.model = list(arima=c(2,1,0))))
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit

#Code from that explanatory video with model
#comparing explanatory video code with hers to check results
s <- ugarchspec(mean.model = list(armaOrder = c(1,1)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'norm')
m <- ugarchfit(data = Exchange_Ratesdiff, spec = s)
m
#FIRST ITEM IN C() IS GARCH EFFECT AND SECOND IS ARCH EFFECT.

#
Exchange_Ratesdiff_garch = ugarchspec(variance.model = list(garchorder=c(1,1), mean.model = list(arima=c(2,1,0))))
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit
#FIRST ITEM IN C() IS GARCH EFFECT AND eSECOND IS ARCH EFFECT.

#comparing explanatory video code with hers to check results
s <- ugarchspec(mean.model = list(armaOrder = c(0,1)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'norm')
m <- ugarchfit(data = Exchange_Ratesdiff, spec = s)
m
#FIRST ITEM IN C() IS GARCH EFFECT AND SECOND IS ARCH EFFECT.

#comparing explanatory video code with hers to check results
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'norm')
m <- ugarchfit(data = Exchange_Ratesdiff, spec = s)
m
#FIRST ITEM IN C() IS GARCH EFFECT AND SECOND IS ARCH EFFECT.

###################
#FIRST ITEM IN C() IS GARCH EFFECT AND eSECOND IS ARCH EFFECT.
Exchange_Ratesdiff_garch = ugarchspec(variance.model = list(garchorder=c(1,2), mean.model = list(arima=c(2,1,0))))
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit

#hers keep giving me same things so I switched to his code.
s <- ugarchspec(mean.model = list(armaOrder = c(2,1)),
                variance.model = list(model = c(2,1,0)),
                distribution.model = 'norm')
m <- ugarchfit(data = Exchange_Ratesdiff, spec = s)
m
#FIRST ITEM IN C() IS GARCH EFFECT AND SECOND IS ARCH EFFECT.

Exchange_Ratesdiff_garch = ugarchspec(variance.model = list(garchorder=c(2,1), mean.model = list(arima=c(2,1,0))))
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit





#FIRST ITEM IN C() IS GARCH EFFECT AND SECOND IS ARCH EFFECT.
####TRYING ANOTHER UNDERSTANDABLE CODE ::::: CODE I finally stuck to............................
Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
               mean.model=list(arimaOrder=c(2,1,0)),distribution.model="norm")
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit

#This didn't give me much....... Trying to look for more summary, residuals and Ljung-box test...
print(summary(Exchange_Ratesdiff_garch_fitbest))


###TRYING THE UNDERSTANDABLE CODE ANOTHER MODEL: I found this to be the correct code because I did't get beta values when I imputed arch models
#### INITIAL Value was distribution = distribution.model="norm" but I changed it to distribution.model="std" and it gave me better estimates and residuals and all
#### NOTE!!!! THOUGH YOU PUT 2,1,0 ARIMA AS MEAN MODEL. IT STILL ESTIMATES USING THE STANDARD (0,0) MEAN MODEL. IT IS STILL THE SAME NO MATTER WHAT I PUT. 
#### THE STANDARD MEAN MODEL IS OKAY ANYWAYS, THE SMALLER THE BETTER. MOST TIMES AVOID COMPLEX MODELS, EXCEPT IN SOME SITUATIONS.

#I later realized that you can etimate using "norm", the normal distribution, or with "std" student T.
#In one medium article, I realized the Std t distribution gives better results than the normal. So
#It is better to stick with the "std" garch estimates.

Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                                      mean.model=list(arimaOrder=c(2,1,0)),distribution.model="std")
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit

###### Now to estimate more GARCH models
Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,0)),
                                      mean.model=list(arimaOrder=c(2,1,0)),distribution.model="std")
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit

###### Now to estimate more GARCH models
Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(2,0)),
                                      mean.model=list(arimaOrder=c(2,1,0)),distribution.model="std")
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit


#FIRST ITEM IN C() IS GARCH EFFECT AND SECOND IS ARCH EFFECT.

###### Now to estimate more GARCH models
Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(3,0)),
                                      mean.model=list(arimaOrder=c(2,1,0)),distribution.model="std")
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit




###### Now to estimate more GARCH models
Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,2)),
                                      mean.model=list(arimaOrder=c(2,1,0)),distribution.model="std")
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit


###### Now to estimate more GARCH models
Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,3)),
                                      mean.model=list(arimaOrder=c(2,1,0)),distribution.model="std")
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit




###### Now to estimate more GARCH models
Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(2,1)),
                                      mean.model=list(arimaOrder=c(2,1,0)),distribution.model="std")
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit

#####
Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(2,2)),
                                      mean.model=list(arimaOrder=c(2,1,0)),distribution.model="std")
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit

####
Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(2,3)),
                                      mean.model=list(arimaOrder=c(2,1,0)),distribution.model="std")
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit

Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(3,1)),
                                      mean.model=list(arimaOrder=c(2,1,0)),distribution.model="std")
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit


Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(3,2)),
                                      mean.model=list(arimaOrder=c(2,1,0)),distribution.model="std")
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit


Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(3,3)),
                                      mean.model=list(arimaOrder=c(2,1,0)),distribution.model="std")
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit


###### Now I TRIED THIS AND KEPT GETTING HIGH AIC AND TWO BETAS WITH NO ALPHA SO IT DOESN'T MAKE MUCH SENSE TO ESTIMATE THIS OR USE IT OR ADD TO MY WORK.
Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(0,1)),
                                      mean.model=list(arimaOrder=c(2,1,0)),distribution.model="std")
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit


Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(0,2)),
                                      mean.model=list(arimaOrder=c(2,1,0)),distribution.model="std")
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit

Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(0,3)),
                                      mean.model=list(arimaOrder=c(2,1,0)),distribution.model="std")
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit


###### This models are giving me a low AIc so I will add them.

Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(4,0)),
                                      mean.model=list(arimaOrder=c(2,1,0)),distribution.model="std")
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit


Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(4,1)),
                                      mean.model=list(arimaOrder=c(2,1,0)),distribution.model="std")
Exchange_Ratesdiff_garch_fit = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fit





##Best model was found to be GARCH(4,0). With smallest AIC and meeting some other conditions
###### Now to estimate more GARCH models
#But it ws an arch model and I did do fro Garch to see if it woul be better, so what I estimated
#I cannot conclude that this is the best without estimating it's  HARCH version.

Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(4,0)),
                                      mean.model=list(arimaOrder=c(2,1,0)),distribution.model="std")
Exchange_Ratesdiff_garch_fitbest = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fitbest
#I wanted more decimal place so I could see my true beta values, 
print(Exchange_Ratesdiff_garch_fitbest, digits = 7)

#I didn't want to go for a complex model because the simpler the better, So I left out the rest and stuck
#with garch(2,3)

#This doesn't give much.
print(summary(Exchange_Ratesdiff_garch_fitbest))


####################
#Diagnostic checking for my Garch model:
#Correlogram
# Compute the standardized returns...........
#Ljung
# Estimate the model on the last __ obs
Exchange_Ratesdiff_garch1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(2,3)),
                                      mean.model=list(arimaOrder=c(2,1,0)),distribution.model="std")
Exchange_Ratesdiff_garch_fitbest1 = ugarchfit(Exchange_Ratesdiff_garch1, data = Exchange_Ratesdiff)
Exchange_Ratesdiff_garch_fitbest1
#I wanted more decimal place so I could see my true beta values, 
print(Exchange_Ratesdiff_garch_fitbest, digits = 7)


#To see p & t values values of table: It prints out the fist set of P values, that is what we are using....

round(Exchange_Ratesdiff_garch_fitbest1@fit$matcoef, 6)




# Compute the standardized returns
Exchange_Ratesdiff_garch_fitbestret <- residuals(Exchange_Ratesdiff_garch_fitbest1, standardize = TRUE)
Exchange_Ratesdiff_garch_fitbestret


# Compute their sample mean and standard deviation
acf(abs(Exchange_Ratesdiff), 12)
acf(abs(Exchange_Ratesdiff_garch_fitbestret), 12)


# Do the Ljung-Box test on the absolute standardized returns
Box.test(abs(Exchange_Ratesdiff_garch_fitbestret), 12, type = "Ljung-Box")

checkresiduals(abs(Exchange_Ratesdiff_garch_fitbestret))

#DATACAMP code I found for summary...
checkresiduals(naive(Exchange_Ratesdiff_garch_fitbestret))
residuals(Exchange_Ratesdiff_garch_fitbest)

plot(Exchange_Ratesdiff_garch_fitbestret,which="all")

norm(Exchange_Ratesdiff_garch_fitbestret)

print(summary(Exchange_Ratesdiff_garch_fitbestret))

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Histogram(Exchange_Ratesdiff_garch_fitbestret, methods = c("add.normal","add.density" ), 
    colorset = c("gray","red","blue"))


#Now code to forecast volatility
#Forecasting for voltility 1825
xchnage_forecast = ugarchforecast(Exchange_Ratesdiff_garch_fitbest1, n.ahead = 20, method=c("partial","full")[1])
xchnage_forecast

#A better code to give me the exact forecast : USE THIS!!!!!!!!!!!!!!!!!!
sigma(xchnage_forecast)

#ploting volatility
plot(xchnage_forecast, which=2)



###FOr a few ARIMA 
ARIMA_exchangeAR1 = arima(Exchange_Ratesdiff, order = c(1,1,1))
ARIMA_exchangeAR1

ARIMA_exchangeMA2 = arima(Exchange_Ratesdiff, order = c(1,1,0))
ARIMA_exchangeMA2


print(summary(ARIMA_exchangeAR1))
print(summary(ARIMA_exchangeMA2))


############################################## Stop ###############################
#Cool guys video package
library(quantmod)
library(rugarch)
library(rmgarch)


ug_foree <- xchnage_forecast@forecast
ug_foree
plot(ug_foree, type ="1")


#this code bring errors so check to fix.
#ploting volatility
plot(xchnage_forecast, which=2)

#I got the same values
plot(m)
f <- ugarchforecast(fitORspec = m, n.ahead = 20)
plot(fitted(f))
plot(sigma(f))


#TILL HER CODE.

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

