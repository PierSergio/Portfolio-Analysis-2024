library(quantmod)
library(tseries)
library(stats)
library(quadprog)

enddate <- "2024-7-1"
t<-215 #The first time you run this, you'll see the error so adjust the t with the requested number
#to find t you need the error
myvector <- c()
nstocks <- 5
pricinglist <- as.data.frame(matrix(ncol=nstocks, nrow=t))
colnames(pricinglist) <- c("IXN","QQQ", "IJH", "GBTC", "GLD") 
#the pricinglist data starts form 2016-8-22 - this is the first row

for (i in 1:(ncol(pricinglist))){
  current_ticker <- colnames(pricinglist)[i]
  newtable <- getSymbols(current_ticker, src = "yahoo", from="2023-8-22", to=enddate, auto.assign=FALSE)
  pricinglist[,i] <- newtable[,6]
}

#forecasting the next price using a backpropagation training algorithm in a neural network. 
# a Autoregressive Model of fourth order AR4 was used.


newpricingdataset <- pricinglist

#creating a dataset with monthly ROR for each day using continuous compounding
dailyROR <- as.data.frame(matrix(ncol=ncol(newpricingdataset), nrow=nrow(newpricingdataset)-25))
colnames(dailyROR) <- colnames(pricinglist)
for (c in 1:(ncol(newpricingdataset))){
  for (r in 1:(nrow(newpricingdataset)-25)){
    dailyROR[r,c] <- log(as.numeric(newpricingdataset[(r+25),c])/as.numeric(newpricingdataset[r,c]))
  }
}
#The most current expected return for n+25 (n is today) is in the last row of the above dataset

#calculating Expected(R) for all securities 
averet <- as.matrix(dailyROR[nrow(dailyROR),], nrow=1)
#calculating covariance matrix
rcov <- cov(dailyROR[(nrow(dailyROR)-125):(nrow(dailyROR)),]) #125 stands for 6 trading months
target.r <- 1/1000
#using solver to get to optimal weights


effFrontier = function(averet, rcov, nports, shorts, wmax, wmin)
{
  mxret <- max(averet)
  mnret <- -mxret
  n.assets <- ncol(averet)
  reshigh <- rep(wmax, n.assets)
  reslow <- rep(wmin, n.assets)
  min.rets <- seq(mnret, mxret, length.out=nports)
  vol <- rep(NA, nports)
  ret <- rep(NA, nports)
  pw <- data.frame(matrix(ncol=nports, nrow=n.assets))
  for (i in 1:nports)
  {
    port.sol <- NULL
    try(port.sol <- portfolio.optim(x=averet, pm=min.rets[i], covmat=rcov,   reshigh = reshigh, reslow= reslow, shorts=F)
        , silent=T)
    if(!is.null(port.sol))
    {
      vol[i] <- sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ret[i] <- averet %*% port.sol$pw
      pw[,i] <- port.sol$pw
    }
  }
  return(list(vol=vol, ret = ret, weights = pw))
  
}

maxSharpe <- function(averet, rcov, shorts=F, wmax=0.2, min.weight=0.01)
{
  optim.callback=function(param, averet, rcov, reshigh, reslow, shorts)
  { 
    port.sol = NULL
    try(port.sol <- portfolio.optim(x=averet, pm=param, covmat=rcov,
                                    reshigh=reshigh, reslow=reslow, shorts=shorts),silent=T)
    if(is.null(port.sol)) { ratio= 10^9} else 
    {
      m.return <- averet %*% port.sol$pw
      m.risk <- sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ratio <- m.return/m.risk
      assign("w", port.sol$pw, inherits=T)
    }
    return(ratio)
  }
  
  ef <- effFrontier(averet=averet, rcov=rcov, shorts=shorts, wmax=wmax, nports = 1000, wmin=min.weight)
  n <- ncol(averet)
  reshigh <- rep(wmax, n)
  reslow <- rep(min.weight, n)
  
  max.sh <- which.max(ef$ret/ef$vol)
  
 
  if(is.na(ef$ret[max.sh-1])){lowerinterval<-ef$ret[max.sh]}else{lowerinterval <- ef$ret[max.sh-1]}
  if(is.na(ef$ret[max.sh+1])){upperinterval<-ef$ret[max.sh]}else{upperinterval <- ef$ret[max.sh+1]}
  
  w <- rep(0, ncol(averet))
  xmin <- optimize(f=optim.callback, interval = c(lowerinterval, upper=upperinterval), 
                   averet=averet, rcov=rcov, reshigh=reshigh, reslow=reslow, shorts=shorts)
  return(w)
  return(xmin)
}

z <- maxSharpe(averet, rcov, shorts=F, wmax=0.4)


