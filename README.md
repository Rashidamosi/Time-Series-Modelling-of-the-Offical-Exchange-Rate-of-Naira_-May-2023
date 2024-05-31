# Time Series Modelling of the Offical Exchange Rate of Naira_ May 2023
 This repository contains the code and documentation for my final year project on Time Series Analysis of the Official Exchange Rate of Naira. Using the ARIMA, SARIMA, ARCH and GARCH models with external presentation on the 6th of May, 2023. 


## About this Project

One of the most essential ways to gauge a country's relative economic health is to look at its foreign exchange rates, and one of the most important indicators of a country's economic health is the foreign exchange rate. This is why it should be closely monitored and studied.

 The monthly record of exchange rates from 1999-2019 was extracted from the records of the Central Bank of Nigeria (CBN) in order to model the Nigerian Naira/USD exchange rates. 

The Autoregressive Integrated Moving Average (ARIMA), Seasonal Autoregressive Integrated Moving Average (SARIMA), the Autoregressive Conditional Heteroskedasticity (ARCH) and Generalized Autoregressive Conditional Heteroskedasticity (GARCH) models were used to model the monthly Nigerian Naira/USD exchange rate from 1999-2019. The initial series was non-stationary, and became stationary at first difference. Several ARIMA models were fitted and the variant of the ARIMA model, SARIMA (2,1,0) (2,1,0) proved to be the best and most significant model for the exchange rate series in this study. But the results showed possible fluctuation intervals of Nigerian Naira to the US Dollar, and from the analysis revealed presence of volatility in the series and a non-constant variance over time which the SARIMA model could not capture. In addition, an ARCH effect was tested for and found, and several ARCH and GARCH models were fitted using the Student's t-distribution, as it was observed that the series did not follow a normal distribution. The symmetric GARCH (2,3) proved to be the best model to analyze and predict volatility in this study. A comparison between the SARIMA (2,1,0) (2,1,0) and GARCH (2,3) model was made and the GARCH model proved better at capturing the dynamics of the exchange rate series. Furthermore, a twenty-days forecast was made using the best model in this study, the GARCH (2,3) model.




