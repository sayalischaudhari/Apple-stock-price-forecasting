# Apple-stock-price-forecasting
To learn about Time Series data and Time Series Forecasting Models

### Problem Description:
Aim to forecast future stock price of apple by using the past stock price data of Apple.

### Dataset Overview-
•	Open - the price of the stock at the beginning of the trading day (it need not be the closing price of the previous trading day)

•	High - the highest price of the stock on that trading day

•	Low - lowest price of the stock on that trading day

•	Close - the price of the stock at closing time 

•	Volume - indicates how many stocks were traded

•	Adjusted close (abreviated as “adjusted” by getSymbols()) - the closing price of the stock that adjusts the price of the stock for  corporate actions

### Steps carried out:

web crawled the live stock data of apple from yahoo using the qunatmod package in R as quantmod provides number of useful feature in financial modelling.

Explored the dataset using autocorrelation coefficient and seasonal plot and applied various forecasting techniques. 

Performed various time series models ARIMA model and Holt's Winter to forecast and proved Simple Exponential Smoothing to be the best method by gaining RMSE score of 1.09 among all the methods.
