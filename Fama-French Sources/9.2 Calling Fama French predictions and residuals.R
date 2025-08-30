#########################################################
#########################################################
### Getting predictions and residuals for Fama French models ###
#########################################################
#########################################################

#first we have to source the previous code with the FF model UDFs that give us predicted values
source("C:/Users/purev/Desktop/Wealth Management Investment Risk Management and Optimization in R/Lessons Material/7.03/Code Templates & Full Code/8.2 Fama French predictions and residuals - source this file before running the next one.R") #WE NEED TO SOURCE THE 8.2 FILE SCRIPT (replace \ with /)
#we get 2 UDFs in our environment - each function has 3 inputs, ticker, from date and to date
#each function outputs a data frame with residuals, actuals and predictions
#########################################################
#########################################################
### Using Fama French 3 Factor model  ###################
#########################################################
#########################################################
#calling the Fama French 3F model UDF for WFC
IXN_FF3F <- fama_french_3F_pred_res(ticker="IXN", from_date='2020-01-02', to_date='2024-07-01') #at least 3 years from today  
IXN_FF3F$actuals
IXN_FF3F$model_pred

#calling the Fama French 3F model UDF for WFC
QQQ_FF3F <- fama_french_3F_pred_res(ticker="QQQ",  from_date='2020-01-02', to_date='2024-07-01')
QQQ_FF3F$actuals
QQQ_FF3F$model_pred
#calling the Fama French 3F model UDF for SPY
VNQ_FF3F <- fama_french_3F_pred_res(ticker="VNQ",  from_date='2020-01-02', to_date='2024-07-01')
VNQ_FF3F$actuals
VNQ_FF3F$model_pred
#########################################################
#########################################################
### Using Fama French 5 Factor model  ###################
#########################################################
#########################################################
#calling the Fama French 3F model UDF for TSLA
IXN_FF5F <- fama_french_5F_pred_res(ticker="IXN", from_date='2010-01-31', to_date='2021-02-20')
IXN_FF5F$actuals
IXN_FF5F$model_pred
#calling the Fama French 3F model UDF for WFC
QQQ_FF5F <- fama_french_5F_pred_res(ticker="QQQ", from_date='2010-01-31', to_date='2021-02-20')
QQQ_FF5F$actuals
QQQ_FF5F$model_pred
#calling the Fama French 3F model UDF for SPY
VNQ_FF5F <- fama_french_5F_pred_res(ticker="VNQ", from_date='2010-01-31', to_date='2021-02-20')
VNQ_FF5F$actuals
VNQ_FF5F$model_pred