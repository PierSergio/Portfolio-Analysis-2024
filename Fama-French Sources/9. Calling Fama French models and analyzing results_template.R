#########################################################
#########################################################
### Using Fama French models and interpreting results ###
#########################################################
#########################################################

#first we have to source the previous code with the FF model UDFs
source("xxx")
#we get 2 UDFs in our environment - each function has 3 inputs, ticker, from date and to date
#each function outputs a list with 2 objects : 
#object [[1]] :: data frame with time series of cumulative model errors and cumulative ticker returns
#object [[2]] :: fama french model that has all the factor coefficients and p-values

#########################################################
#########################################################
### Using Fama French 3 Factor model  ###################
#########################################################
#########################################################
#calling the Fama French 3F model UDF for TSLA
xxx <- fama_french_3F(ticker="xxx", from_date='xxx', to_date='xxx')
summary(xxxx[[xxx]])#looking at factor loading - are any statisitcally significant
#now let's visualize the model error and the cumulative stock returns
ggplot(data=xxx[[xxx]])+
  xxx(aes(x=Date, y=xxx), color="red4")+
  geom_line(aes(x=Date, y=xxx), color="blue") #red is the error and blue is the stock return
#calling the Fama French 3F model UDF for WFC
WFC_FF3F <- fama_french_3F(ticker="xxx", from_date='2010-01-31', to_date='2021-02-20')
summary(WFC_FF3F[[2]])
  
ggplot(data=WFC_FF3F[[1]])+
  geom_line(aes(x=Date, y=rr_spf), color="red4")+
  geom_line(aes(x=Date, y=tr_cum), color="blue")

#calling the Fama French 3F model UDF for SPY
SPY_FF3F <- fama_french_3F(ticker="xxx", from_date='2010-01-31', to_date='2021-02-20')
summary(SPY_FF3F[[2]])

ggplot(data=SPY_FF3F[[1]])+
  geom_line(aes(x=Date, y=rr_spf), color="red4")+
  geom_line(aes(x=Date, y=tr_cum), color="blue")


#########################################################
#########################################################
### Using Fama French 5 Factor model  ###################
#########################################################
#########################################################
#calling the Fama French 3F model UDF for TSLA
xxx <- fama_french_5F(ticker="xxx", from_date='xxx', to_date='xxx')
summary(xxx[[xxx]])#looking at factor loading - are any statisitcally significant
#now let's visualize the model error and the cumulative stock returns
ggplot(data=TSLA_FF5F[[xxx]])+
  geom_line(aes(x=Date, y=xxx), color="red4")+
  geom_line(aes(x=Date, y=xxx), color="blue") #red is the error and blue is the stock return
#calling the Fama French 3F model UDF for WFC
WFC_FF5F <- fama_french_5F(ticker="xxx", from_date='2010-01-31', to_date='2021-02-20')
summary(WFC_FF5F[[2]])

ggplot(data=WFC_FF5F[[1]])+
  geom_line(aes(x=Date, y=rr_spf), color="red4")+
  geom_line(aes(x=Date, y=tr_cum), color="blue")

#calling the Fama French 3F model UDF for SPY
SPY_FF5F <- fama_french_5F(ticker="xxx", from_date='2010-01-31', to_date='2021-02-20')
summary(SPY_FF5F[[2]])

ggplot(data=SPY_FF5F[[1]])+
  geom_line(aes(x=Date, y=rr_spf), color="red4")+
  geom_line(aes(x=Date, y=tr_cum), color="blue")