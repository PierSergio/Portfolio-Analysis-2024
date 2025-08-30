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
#calling the Fama French 3F model UDF for IXN
IXN_FF3F <- fama_french_3F_pred_res(ticker="IXN", from_date='2020-01-02', to_date='2024-07-01') #at least 3 years from today  
IXN_FF3F$actuals
IXN_FF3F$model_pred

#calling the Fama French 3F model UDF for QQQ
QQQ_FF3F <- fama_french_3F_pred_res(ticker="QQQ",  from_date='2020-01-02', to_date='2024-07-01')
QQQ_FF3F$actuals
QQQ_FF3F$model_pred

#calling the Fama French 3F model UDF for VNQ
VNQ_FF3F <- fama_french_3F_pred_res(ticker="VNQ",  from_date='2020-01-02', to_date='2024-07-01')
VNQ_FF3F$actuals
VNQ_FF3F$model_pred

#########################################################
#########################################################
### Using Fama French 5 Factor model  ###################
#########################################################
#########################################################
#calling the Fama French 3F model UDF for IXN
IXN_FF5F <- fama_french_5F_pred_res(ticker="IXN", from_date='2010-01-31', to_date='2021-02-20')
IXN_FF5F$actuals
IXN_FF5F$model_pred
#calling the Fama French 3F model UDF for QQQ
QQQ_FF5F <- fama_french_5F_pred_res(ticker="QQQ", from_date='2010-01-31', to_date='2021-02-20')
QQQ_FF5F$actuals
QQQ_FF5F$model_pred
#calling the Fama French 3F model UDF for VNQ
VNQ_FF5F <- fama_french_5F_pred_res(ticker="VNQ", from_date='2010-01-31', to_date='2021-02-20')
VNQ_FF5F$actuals
VNQ_FF5F$model_pred


######### PRINTING FOR THE REPORT ###############

FF3_IXN <- data.frame(
  residuals = c(-2.567890e-02, 7.633636e-03, 1.982717e-02, 6.995556e-03, -3.231554e-04, 1.394164e-02, 4.243848e-03, -2.219995e-02),
  actuals = c(-0.0264910280, 0.0096041670, 0.0162872329, 0.0046801209, -0.0012704998, 0.0148410655, 0.0034818965, -0.0217903787),
  model_pred = c(-8.121248e-04, 1.970531e-03, -3.539939e-03, -2.315436e-03, -9.473444e-04, 8.994263e-04, -7.619513e-04, 4.095665e-04)
)

print(data)

FF3_QQQ<- data.frame(
  residuals = c(0.0012063179, -0.0051575944, 0.0079634370, 0.0055710988, -0.0085826132, -0.0125795867, -0.0041226509, 0.0084427443),
  actuals = c(-0.0001974255, -0.004498421, 0.009456445, 0.003777659, -0.007048392, -0.00107134, -0.001860229, 0.005369325),
  model_pred = c(-0.0014037434, 0.0006591731, 0.0014930081, -0.0017934394, 0.0015342211, 0.001866189, 0.0022624215, -0.0030734189)
)

print(FF3_QQQ)

FF3_VNQ <- data.frame(
  residuals = c(-8.136000e-03, -2.185966e-02, -6.273373e-05, -5.481429e-03, -1.050084e-02, 1.360411e-02, 1.701159e-02, -4.770401e-04),
  actuals = c(-0.0090089554, -0.0215311268, 0.0007335193, -0.0065966459, -0.0097147579, 0.0146529359, 0.0187247776, -0.0022824733),
  model_pred = c(-8.729554e-04, 3.285365e-04, 7.962530e-04, -1.115217e-03, 7.860775e-04, 1.048828e-03, 1.713187e-03, -1.805433e-03)
)