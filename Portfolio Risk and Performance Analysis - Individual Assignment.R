##############################################
## A1: Final Coding Assignment  ##############
## Client : Ultra High Net Worth client ######
##############################################
## Author : Pier Sergio Caltabiano ###########
## pcaltabiano@student.hult.edu ##############
## Hult International Business School ########
##############################################


# In first place, just for the personal preference of the analyst, a data frame and
# a pie chart with the portfolio assets weights is created. (I like to visualize things before
# working on them)

original_pf_visualization <- data.frame(
  Ticker = c("IXN", "QQQ", "IEF", "VNQ", "GLD"),                                              # These are market symbols of the assets 
  Name = c("Ishares Global Tech Etf", "NASDAQ 100", "iShares 7-10 Year Treasury Bond ETF",    # These are the actual names of the assets
           "Vanguard Real Estate ETF", "SPDR Gold Shares"),
  Percentage_allocation = c("17.5%", "22.1%", "28.5%", "8.9%", "23%"),                        # These are the weights in the portfolio 
  Asset_class = c("Equity", "Equity", "Fixed Income", "Real Assets", "Commodities")           # These are the assets classes
)

# Weights percentages need to be converted in numeric values
original_pf_visualization$Percentage_allocation <- as.numeric(sub("%", "", original_pf_visualization$Percentage_allocation))

# Let's visualize in a pie chart
library(ggplot2)

ggplot(original_pf_visualization, aes(x = "", y = Percentage_allocation, fill = Name)) +     # Creating the pie chart and labeling 
  geom_bar(stat = "identity", width = 1) +                                                   # Assigning the slices to the actual percentage value
  coord_polar(theta = "y") +                                                                 # Building the pie
  labs(title = "Allocation Percentages of Assets in the Original Portofolio") +              # Adding a title
  theme_void() +                                                                             # Assigning colors
  theme(legend.title = element_blank())                                                      # Removing default title legend 


# Considering the fact that Fixed Income Asset Class is only the 28.5% of the pf
# And considering that according to Morningstar, not only, the VNQ ETF is quite risky
# but also the Gold in the last years has been more volatile that equities, 
# The portfolio can be considered as aggressive. Clearly there more aggressive portfolios 
# but this is risky enough to be considered as such. 


# Note: 
# Gold’s annualized volatility over the last 30 years is 15.44% –
# not dramatically higher than the S&P 500–which posted a 14.32% annualized volatility over the
# same period! 

# Now let's check returns
# Let's install useful libraries for this scope

library(quantmod)
library(dplyr)    

# Downloading prices from the Yahoo finance portal

Equities1 <- getSymbols("IXN", auto.assign = F)    # Ishares Global Tech Etf
Equities2 <- getSymbols("QQQ", auto.assign = F)        # NASDAQ 100
Fixed_Income <- getSymbols("IEF", auto.assign = F)     # iShares 7-10 Year Treasury Bond ETF
Real_Estate <- getSymbols("VNQ", auto.assign = F)      # Vanguard Real Estate ETF
Commodity <- getSymbols("GLD", auto.assign = F)        # SPDR Gold Shares
  
# Let's merge in a single object

joined_prices <- merge.xts(Equities1, Equities2, Fixed_Income, Real_Estate, Commodity)

# Only adjusted prices are required, let's extract them

joined_prices_adj <- joined_prices[,seq(from=6, to=ncol(joined_prices), by=6)]

# Now it is possible to calculate the return!

# Converting the xts data frame to a regular data frame and then creating a new variable

joined_returns_loop <- as.data.frame(joined_prices_adj)  # Conversion
joined_returns_loop$log_ret <- c()                       # It is required to define an empty variable
joined_returns_loop$log_ret[1] <- NA                     # Filling the first month with NA

# Now creating the user defined function to get returns is coded

window_returns <- function(x, t){                        # t in days, x is your variable
  compounded <- rep(NA, each=(t-1))                      # Creating an empty vector for returns
  for(i in t:length(x)){                                 # Starting from the 2nd day 
    compounded[i] <- log(x[i]/ x[i-t+1])                 # The i-t+1 defines the time window 
  }                                                      # Closing the i-loop
  return(compounded)                                     # Function Output
}                                                        # Closing window_returns function

# Let's get our returns for 12, 18 and 24 Months!
# Let's start with 12 Months

joined_returns_loop$Eq1_12M_ret <- window_returns(x=joined_returns_loop$IXN.Adjusted , t=250)      # Annual returns for Ishares Global Tech Etf
joined_returns_loop$Eq2_12M_ret <- window_returns(x=joined_returns_loop$QQQ.Adjusted , t=250)      # NASDAQ 100
joined_returns_loop$Fix_Inc_12M_ret <- window_returns(x=joined_returns_loop$IEF.Adjusted , t=250)  # iShares 7-10 Year Treasury Bond ETF
joined_returns_loop$RE_12M_ret <- window_returns(x=joined_returns_loop$VNQ.Adjusted , t=250)       # Vanguard Real Estate ETF
joined_returns_loop$Comm_12M_ret <- window_returns(x=joined_returns_loop$GLD.Adjusted , t=250)     # SPDR Gold Shares

# Now 18 months

joined_returns_loop$Eq1_18M_ret <- window_returns(x=joined_returns_loop$IXN.Adjusted , t=375)      # 18M returns for Ishares Global Tech Etf
joined_returns_loop$Eq2_18M_ret <- window_returns(x=joined_returns_loop$QQQ.Adjusted , t=375)      # NASDAQ 100
joined_returns_loop$Fix_Inc_18M_ret <- window_returns(x=joined_returns_loop$IEF.Adjusted , t=375)  # iShares 7-10 Year Treasury Bond ETF
joined_returns_loop$RE_18M_ret <- window_returns(x=joined_returns_loop$VNQ.Adjusted , t=375)       # Vanguard Real Estate ETF
joined_returns_loop$Comm_18M_ret <- window_returns(x=joined_returns_loop$GLD.Adjusted , t=375)     # SPDR Gold Shares

# And now 24

joined_returns_loop$Eq1_24M_ret <- window_returns(x=joined_returns_loop$IXN.Adjusted , t=500)      # 24M returns for Ishares Global Tech Etf
joined_returns_loop$Eq2_24M_ret <- window_returns(x=joined_returns_loop$QQQ.Adjusted , t=500)      # NASDAQ 100
joined_returns_loop$Fix_Inc_24M_ret <- window_returns(x=joined_returns_loop$IEF.Adjusted , t=500)  # iShares 7-10 Year Treasury Bond ETF
joined_returns_loop$RE_24M_ret <- window_returns(x=joined_returns_loop$VNQ.Adjusted , t=500)       # Vanguard Real Estate ETF
joined_returns_loop$Comm_24M_ret <- window_returns(x=joined_returns_loop$GLD.Adjusted , t=500)     # SPDR Gold Shares

# Calculating the return for the portfolio
# Assigning weights 

EQ1_w <- 0.175 # Ishares Global Tech Etf
EQ2_w <- 0.221 # NASDAQ 100
FI_w <- 0.285  # iShares 7-10 Year Treasury Bond ETF
RE_w <- 0.089  # Vanguard Real Estate ETF
C_w <- 0.23    # SPDR Gold Shares

#Assuming weights are consistent trough time it possible to calculate the return for the entire 
#portfolio for 12, 18 and 24 Months

joined_returns_loop <- as.data.frame(joined_returns_loop) %>%
  mutate(portfolio12M = EQ1_w*Eq1_12M_ret + EQ2_w*Eq2_12M_ret+
           FI_w*Fix_Inc_12M_ret + RE_w*RE_12M_ret + C_w*Comm_12M_ret)%>%        #12 months 
  mutate(portfolio18M = EQ1_w*Eq1_18M_ret + EQ2_w*Eq2_18M_ret+
           FI_w*Fix_Inc_18M_ret + RE_w*RE_18M_ret + C_w*Comm_18M_ret)%>%        #18 months
  mutate(portfolio24M = EQ1_w*Eq1_24M_ret + EQ2_w*Eq2_24M_ret+
           FI_w*Fix_Inc_24M_ret + RE_w*RE_24M_ret + C_w*Comm_24M_ret)           #24 months


# Now we want to know the sharpe ratio for the stocks and the whole portfolio

EQ1_monthly_return <- monthlyReturn(getSymbols("IXN", auto.assign = F))         # Ishares Global Tech Etf
EQ2_monthly_return <- monthlyReturn(getSymbols("QQQ", auto.assign = F))         # NASDAQ 100
FI_monthly_return <- monthlyReturn(getSymbols("IEF", auto.assign = F))          # iShares 7-10 Year Treasury Bond ETF
RE_monthly_return <- monthlyReturn(getSymbols("VNQ", auto.assign = F))          # Vanguard Real Estate ETF
C_monthly_return <- monthlyReturn(getSymbols("GLD", auto.assign = F))           # SPDR Gold Shares

# Merging returns together

joined_monthly_returns <- merge.xts(EQ1_monthly_return,
                                    EQ2_monthly_return,
                                    FI_monthly_return,
                                    RE_monthly_return,
                                    C_monthly_return )

# Creating the time index and risk free objects

time_index <- nrow(joined_monthly_returns) #210 months so -12 months = 199

risk_free <- 0.0001

# Calculating expected returns for the assets

monthly_exp_ret_Eq1 <- mean(joined_monthly_returns$monthly.returns[time_index:(time_index-11)])       # Ishares Global Tech Etf
monthly_exp_ret_Eq2 <- mean(joined_monthly_returns$monthly.returns.1[time_index:(time_index-11)])     # NASDAQ 100
monthly_exp_ret_Fix_Inc <- mean(joined_monthly_returns$monthly.returns.2[time_index:(time_index-11)]) # iShares 7-10 Year Treasury Bond ETF
monthly_exp_ret_RE <- mean(joined_monthly_returns$monthly.returns.3[time_index:(time_index-11)])      # Vanguard Real Estate ETF
monthly_exp_ret_Comm <- mean(joined_monthly_returns$monthly.returns.4[time_index:(time_index-11)])    # SPDR Gold Shares

# Calculating sigma for the assets

EQ1_sigma <- sd(joined_monthly_returns$monthly.returns[time_index:(time_index-11)])*sqrt(12)          # Ishares Global Tech Etf
EQ2_sigma <- sd(joined_monthly_returns$monthly.returns.1[time_index:(time_index-11)])*sqrt(12)        # NASDAQ 100
FI_sigma <- sd(joined_monthly_returns$monthly.returns.2[time_index:(time_index-11)])*sqrt(12)         # iShares 7-10 Year Treasury Bond ETF
RE_sigma <- sd(joined_monthly_returns$monthly.returns.3[time_index:(time_index-11)])*sqrt(12)         # Vanguard Real Estate ETF
C_sigma <- sd(joined_monthly_returns$monthly.returns.4[time_index:(time_index-11)])*sqrt(12)          # SPDR Gold Shares

# Calculating Sharpe ratios for the assets

sharpe_ret_Eq1 <- (((1+monthly_exp_ret_Eq1)^12)-1 - risk_free)/EQ1_sigma        # Ishares Global Tech Etf
sharpe_ret_Eq2 <- (((1+monthly_exp_ret_Eq2)^12)-1 - risk_free)/EQ2_sigma        # NASDAQ 100
sharpe_ret_Fix_Inc <- (((1+monthly_exp_ret_Fix_Inc)^12)-1 - risk_free)/FI_sigma # iShares 7-10 Year Treasury Bond ETF
sharpe_exp_ret_RE <- (((1+monthly_exp_ret_RE)^12)-1 - risk_free)/RE_sigma       # Vanguard Real Estate ETF
sharpe_exp_ret_Comm <- (((1+monthly_exp_ret_Comm)^12)-1 - risk_free)/C_sigma    # SPDR Gold Shares

# Let's see how much is the Sharpe for the entire portfolio
# Since the xts built until has NA values is not possible to calculate std. dev.
# So the original method explained in class is used

joined_ret_for_pf_calc <- merge.xts(EQ1_monthly_return, # Ishares Global Tech Etf
                                    EQ2_monthly_return, # NASDAQ 100
                                    FI_monthly_return,  # iShares 7-10 Year Treasury Bond ETF
                                    RE_monthly_return,  # Vanguard Real Estate ETF
                                    C_monthly_return)   # SPDR Gold Shares

# Adding again the portfolio returns 

joined_ret_for_pf_calc <- as.data.frame(joined_ret_for_pf_calc)%>%
  mutate(portfolio = EQ1_w * monthly.returns +
           EQ2_w * monthly.returns.1 +
           FI_w * monthly.returns.2 +
           RE_w * monthly.returns.3 +
           C_w * monthly.returns.4)

# Let's calculate sigma of the pf

pf_sigma <- sd(joined_ret_for_pf_calc$portfolio[time_index:(time_index-11)])*sqrt(12) 

# Let's calculate expected value of the pf

pf_exp <- mean(joined_ret_for_pf_calc$portfolio[time_index:(time_index-11)])

# And now Sharpe Ratio of pf

pf_sharpe <- (((1+pf_exp)^12)-1 - risk_free)/pf_sigma

# Let's visualize

plot_data <- data.frame(
  Asset = c("IXN", "QQQ", "IEF","VNQ", "GLD", "Portfolio"),
  Expected_Return = c(((1+monthly_exp_ret_Eq1)^12)-1, ((1+monthly_exp_ret_Eq2)^12)-1, ((1+monthly_exp_ret_Fix_Inc)^12)-1, ((1+monthly_exp_ret_RE)^12)-1, ((1+monthly_exp_ret_Comm)^12)-1, ((1+pf_exp)^12)-1),
  Risk = c(EQ1_sigma, EQ2_sigma, FI_sigma, RE_sigma, C_sigma, pf_sigma)
)

# Plot the scatter plot

ggplot(plot_data, aes(x = Risk, y = Expected_Return, color = Asset)) +          # Defining axes 
  geom_point(size = 4) +                                                        # Defining points size
  geom_text(aes(label = Asset), vjust = -1, show.legend = F ) +                 # Defining labels and graphical adjustments
  labs(title = "Expected Return vs Volatility for Each Asset and the Assets",
       x = "Volatility (Annualized Standard Deviation)",
       y = "Expected Return (Annualized)") +
  theme_minimal()+                                                              # Defining the theme
  theme(legend.position = "none")                                               # Hides the legend

# Let's check for tracking error 
# A good benchmark could be the AOA iShares Core Aggressive Allocation ETF (80% Equities, 20% Bond)

# As first thing it is necessary to add the returns of the benchmark to rhe monthly returns xts

benchmark_monthly_returns <- monthlyReturn(getSymbols("AOA", auto.assign = F))
joined_monthly_returns <- merge.xts(joined_monthly_returns,
                                    benchmark_monthly_returns) # joined_monthly returns has been used because using joined_ret_for_pf_calc the IDE crashed 

# Now calculating the sigma for the Benchmark 

Benchmark_sigma<- sd(joined_monthly_returns$monthly.returns.5[time_index:(time_index-11)])*sqrt(12) 

# Let's merge the benchmark data with the portfolio ones

joined_monthly_returns <- merge.xts(joined_monthly_returns,                    
                                    joined_ret_for_pf_calc$portfolio)

# Applying the Tracking Error Formula 

te_EQ1 <- sd(joined_monthly_returns$monthly.returns[time_index:(time_index-11)]-
               joined_monthly_returns$monthly.returns.5[time_index:(time_index-11)])*sqrt(12)       # Ishares Global Tech Etf
te_EQ2 <- sd(joined_monthly_returns$monthly.returns.1[time_index:(time_index-11)]-
               joined_monthly_returns$monthly.returns.5[time_index:(time_index-11)])*sqrt(12)       # NASDAQ 100
te_FI <-  sd(joined_monthly_returns$monthly.returns.2[time_index:(time_index-11)]-
               joined_monthly_returns$monthly.returns.5[time_index:(time_index-11)])*sqrt(12)       # iShares 7-10 Year Treasury Bond ETF
te_RE <- sd(joined_monthly_returns$monthly.returns.3[time_index:(time_index-11)]-
              joined_monthly_returns$monthly.returns.5[time_index:(time_index-11)])*sqrt(12)        # Vanguard Real Estate ETF
te_C <- sd(joined_monthly_returns$monthly.returns.4[time_index:(time_index-11)]-
             joined_monthly_returns$monthly.returns.5[time_index:(time_index-11)])*sqrt(12)         # SPDR Gold Shares
pf_te <- sd(joined_monthly_returns$joined_ret_for_pf_calc.portfolio[(time_index-11):time_index] -
              joined_monthly_returns$monthly.returns.5[(time_index-11):time_index]) * sqrt(12)      # Portfolio

# Continuing the Risk Analysis let's check the correlation among the pf assets

Correlations <- as.data.frame(cor(joined_monthly_returns[time_index:(time_index-11),]))

# Wow! Correlation is considerably high among the majority of the assets! This need to be fixed. 

# Let's build a semi CAPM model to quantify the risk (as seen in class is not anymore possible to use the CAPM as predictive model)
# To properly use the CAPM it is not correct to use the AOA since this is not properly a market is an ETF
# So the SP500 will be taken in consideration

SPY_monthly_returns <- monthlyReturn(getSymbols("SPY", from = "2007-01-31", to = "2024-07-02", auto.assign = F, ))
colnames(SPY_monthly_returns) <- "SPY_benchmark_ret" # This is done to avoid R to crash because it would not find the correct sequence for naming the column in the joined_monthly_returns xts (last column, the porfolio one, has a customized name) 
joined_monthly_returns <- merge.xts(joined_monthly_returns,
                                    SPY_monthly_returns)


last_12_months <- joined_monthly_returns[time_index:(time_index-11),] # Specifying which time frame to use for our CAPM

EQ1_CAPM <-lm(joined_monthly_returns$monthly.returns~joined_monthly_returns$SPY_benchmark_ret, data=last_12_months)          #Regression for Ishares Global Tech Etf
EQ2_CAPM <-lm(joined_monthly_returns$monthly.returns.1~joined_monthly_returns$SPY_benchmark_ret, data=last_12_months)        #Regression for Nasdaq 100
FI_CAPM <-lm(joined_monthly_returns$monthly.returns.2~joined_monthly_returns$SPY_benchmark_ret, data=last_12_months)         #Regression for iShares 7-10 Year Treasury Bond ETF
RE_CAPM <-lm(joined_monthly_returns$monthly.returns.3~joined_monthly_returns$SPY_benchmark_ret, data=last_12_months)         #Regression for Vanguard Real Estate ETF
GLD_CAPM <-lm(joined_monthly_returns$monthly.returns.4~joined_monthly_returns$SPY_benchmark_ret, data=last_12_months)        #Regression for SPDR Gold Shares

# Let's quickly visualize 

summary(EQ1_CAPM ) #P-Value ok, R^2 = 0.8 very good, Market Beta = 1.13 Slightly riskier than the SPY
summary(EQ2_CAPM ) #P-Value ok, R^2 = 0.8 very good, Market Beta = 1.09 Slightly riskier than the SPY
summary(FI_CAPM )  #P-Value 0.2, R^2 = 0.06 Garbage Model, in the last 12 months as seen in the correlation table but the model doesn't hold so 
# the variance in the EIF returns cannot be explained by the market returns (should make senses)
summary(RE_CAPM )  #P-Value ok, R^2 = 0.57 good, Market Beta = 1.09 Slightly riskier than the SPY
summary(GLD_CAPM ) #P-Value 0.17, R^2 = 0.008 Garbage Model, Gold is not correlated with the other assets

# Let's calculate the Treynor Ratio

# Clearly it should be calculated only for the working models

Treynor_EQ1 <- (((1+monthly_exp_ret_Eq1)^12)-1 - risk_free)/EQ1_CAPM$coefficients[2]  # Treynor for for Ishares Global Tech Etf
Treynor_EQ2 <- (((1+monthly_exp_ret_Eq2)^12)-1 - risk_free)/EQ2_CAPM$coefficients[2]  # Treynor for NASDAQ 100
Treynor_RE  <- (((1+monthly_exp_ret_RE)^12)-1 - risk_free)/FI_CAPM$coefficients[2]    # Treynor for Vanguard Real Estate ETF

# As last indicator Sortino Ratio is calculated

Sortino_Eq1 <- (((1+monthly_exp_ret_Eq1)^12)-1 - risk_free)/(sd(joined_monthly_returns$monthly.returns[time_index:(time_index-11)][joined_monthly_returns$monthly.returns[time_index:(time_index-11)] > 0])*sqrt(12))
Sortino_Eq2 <- (((1+monthly_exp_ret_Eq2)^12)-1 - risk_free)/(sd(joined_monthly_returns$monthly.returns[time_index:(time_index-11)][joined_monthly_returns$monthly.returns[time_index:(time_index-11)] > 0])*sqrt(12))
Sortino_Fix_Inc <- (((1+monthly_exp_ret_Fix_Inc)^12)-1 - risk_free)/(sd(joined_monthly_returns$monthly.returns[time_index:(time_index-11)][joined_monthly_returns$monthly.returns[time_index:(time_index-11)] > 0])*sqrt(12))
Sortino_ret_RE <- (((1+monthly_exp_ret_RE)^12)-1 - risk_free)/(sd(joined_monthly_returns$monthly.returns[time_index:(time_index-11)][joined_monthly_returns$monthly.returns[time_index:(time_index-11)] > 0])*sqrt(12))  
Sortino_Comm <- (((1+monthly_exp_ret_Comm)^12)-1 - risk_free)/(sd(joined_monthly_returns$monthly.returns[time_index:(time_index-11)][joined_monthly_returns$monthly.returns[time_index:(time_index-11)] > 0])*sqrt(12))
Sortino_pf <- (((1+pf_exp)^12)-1 - risk_free)/(sd(joined_ret_for_pf_calc$portfolio[time_index:(time_index-11)][joined_ret_for_pf_calc$portfolio[time_index:(time_index-11)] > 0])*sqrt(12))

################################################################################
##################################  DATA VIZ  ##################################
################################################################################


################# RETURNS ###################

# Function to plot a series with a trend line

plot_with_trend <- function(dates, returns, color, title) {
  plot(dates, returns, type = "l", col = color, lwd = 2, xlab = "Days", ylab = "Annual Return", main = title)
  fit <- lm(returns ~ dates)
  abline(fit, col = color, lty = 2)
}

# Set up the plotting area to have 3 rows and 2 columns

par(mfrow = c(1, 1))
dates <- index(joined_returns_loop)[(time_index-11):time_index]

# Plot each ETF in a separate plot  a the trend line

plot_with_trend(index(joined_returns_loop), joined_returns_loop$Eq1_12M_ret, "blue", "Ishares Global Tech Etf (IXN)")
plot_with_trend(index(joined_returns_loop), joined_returns_loop$Eq2_12M_ret, "green", "NASDAQ 100 (QQQ)")
plot_with_trend(index(joined_returns_loop), joined_returns_loop$Fix_Inc_12M_ret, "red", "iShares 7-10 Year Treasury Bond ETF (IEF)")
plot_with_trend(index(joined_returns_loop), joined_returns_loop$RE_12M_ret, "purple", "Vanguard Real Estate ETF (VNQ)")
plot_with_trend(index(joined_returns_loop), joined_returns_loop$Comm_12M_ret, "orange", "SPDR Gold Shares (GLD)")

# Reset the plotting area to default

par(mfrow = c(1, 1))

############# SIGMA ##############

# Let's plot the expected value and the sigma for each asset

data_risk1 <- data.frame(
  Asset_Class = c("IXN", "QQQ", "IEF", "VNQ", "GLD","PORT"),
  Expected_Return = c(monthly_exp_ret_Eq1, monthly_exp_ret_Eq2, monthly_exp_ret_Fix_Inc, monthly_exp_ret_RE, monthly_exp_ret_Comm,pf_exp),
  Risk = c(EQ1_sigma, EQ2_sigma, FI_sigma, RE_sigma, C_sigma, pf_sigma)
)

dev.off() # this is here to solve the error it appeared when the plot was launched (source StackOverflow)

# Scatter plot

library(ggplot2)

ggplot(data_risk1, aes(x = Risk, y = Expected_Return, label = Asset_Class, color = Asset_Class)) +
  geom_point(size = 4) +
  geom_text(vjust = -1) +
  labs(x = "Risk (sigma)", y = "Expected Value") +
  ggtitle("Risk vs Expected Value for Different Asset Classes") +
  theme_minimal() +
  theme(legend.position = "none")

############# TE ###############

# Let's plot the expected value and the TE for each asset

data_risk_2 <- data.frame(
  Asset_Class = c("IXN", "QQQ", "IEF", "VNQ", "GLD"),
  Expected_Return = c(monthly_exp_ret_Eq1, monthly_exp_ret_Eq2, monthly_exp_ret_Fix_Inc, monthly_exp_ret_RE, monthly_exp_ret_Comm),
  Risk2 = c(te_EQ1, te_EQ2,te_FI, te_RE, te_C )
)

dev.off()

# Scatter plot

library(ggplot2)

ggplot(data_risk_2, aes(x = Risk2, y = Expected_Return, label = Asset_Class, color = Asset_Class)) +
  geom_point(size = 4) +
  geom_text(vjust = -1) +
  labs(x = "Risk (Tracking Error)", y = "Expected Value") +
  ggtitle("Risk vs Expected Value for Different Asset Classes") +
  theme_minimal() +
  theme(legend.position = "none")

############ SIGMA AND TE FOR EXPORTING ###################

Sigmas <- as.data.frame(c(EQ1_sigma, EQ2_sigma, FI_sigma, RE_sigma, C_sigma, pf_sigma ))
TEs <- as.data.frame(c(te_EQ1, te_EQ2, te_FI, te_RE, te_C, pf_te ))

write.csv(Sigmas, "sigmas")
write.csv(TEs, "Tes")
print(Sigmas)
print(TEs)

########### CORRELATIONS EXPORTING ##############

install.packages("openxlsx")
library(openxlsx)
write.xlsx(Correlations, "corr")

########## BETAS ###################

# Let's plot the expected value and the Betas for each asset

data_risk_3 <- data.frame(
  Asset_Class = c("IXN", "QQQ", "VNQ"),
  Expected_Return = c(monthly_exp_ret_Eq1, monthly_exp_ret_Eq2, monthly_exp_ret_RE),
  Risk3 = c(EQ1_CAPM$coefficients[2], EQ2_CAPM$coefficients[2], RE_CAPM$coefficients[2])
)
dev.off()

# Scatter plot
library(ggplot2)

ggplot(data_risk_3, aes(x = Risk3, y = Expected_Return, label = Asset_Class, color = Asset_Class)) +
  geom_point(size = 4) +
  geom_text(vjust = 2) +
  labs(x = "Risk (Beta)", y = "Expected Value") +
  ggtitle("Risk vs Expected Value for Different Asset Classes") +
  theme_minimal() +
  theme(legend.position = "none")

########### RATIOS ###########

# Creating a data frame for the ratios

ratios_df <- data.frame(
  Asset_Class = c("IXN", "QQQ", "IEF", "VNQ", "GLD", "Portfolio"),
  Sharpe_Ratio = c(sharpe_ret_Eq1, sharpe_ret_Eq2, sharpe_ret_Fix_Inc, sharpe_exp_ret_RE, sharpe_exp_ret_Comm, pf_sharpe),
  Sortino_Ratio = c(Sortino_Eq1, Sortino_Eq2, Sortino_Fix_Inc, Sortino_ret_RE, Sortino_Comm, Sortino_pf),  # Portfolio Sortino Ratio not calculated earlier
  Treynor_Ratio = c(Treynor_EQ1, Treynor_EQ2, NA, Treynor_RE, NA, NA)  # Treynor Ratio not available for some assets
)

# Print the ratios table to check it 

print(ratios_df)

write.xlsx(ratios_df, "Ratios")


##################################################
#### FINAL E(X), SIGMA AND SHARPE CALCULATION ####
##################################################

FINAL_EQ1_ret <- monthlyReturn(getSymbols("IXN", auto.assign = F)) # iShares Global Tech Etf
FINAL_EQ2_ret <- monthlyReturn(getSymbols("QQQ", auto.assign = F)) # NASDAQ 100
FINAL_C_ret <- monthlyReturn(getSymbols("GLD", auto.assign = F))   # SPDR Gold Shares

FINAL_EQ1_w <- 0.40  #weight for IXN
FINAL_EQ2_w <- 0.342 #weight for QQQ
FINAL_C_w <- 0.238   #weight for GLD

# Let's merge

FINAL_joined_returns <-  merge.xts(FINAL_EQ1_ret, FINAL_EQ2_ret, FINAL_C_ret)

# Adding again the portfolio returns 

FINAL_pf_ret <- as.data.frame(FINAL_joined_returns)%>%
  mutate(portfolio = FINAL_EQ1_w * monthly.returns +
           FINAL_EQ2_w * monthly.returns.1 +
           FINAL_C_w * monthly.returns.2)

# Expected value 

FINAL_pf_exp <- mean(FINAL_pf_ret$portfolio[time_index:(time_index-11)])

# Sigma

FINAL_pf_sigma <- sd(FINAL_pf_ret$portfolio[time_index:(time_index-11)])*sqrt(12)

# Sharpe

FINAL_pf_sharpe <- (((1+FINAL_pf_exp)^12)-1 - risk_free)/pf_sigma

#### FINAL E(X), SIGMA AND SHARPE CALCULATION - simulation 2

FINAL_EQ1_ret <- monthlyReturn(getSymbols("IXN", auto.assign = F)) # iShares Global Tech Etf
FINAL_C_ret <- monthlyReturn(getSymbols("GLD", auto.assign = F)) # SPDR Gold Shares

FINAL2_EQ1_w <- 0.74 #weight for IXN
FINAL2_C_w <- 0.234 #weight for GLD

# Let's merge
FINAL_joined_returns2 <-  merge.xts(FINAL_EQ1_ret, FINAL_C_ret)

# Adding again the portfolio returns 

FINAL2_pf_ret <- as.data.frame(FINAL_joined_returns2)%>%
  mutate(portfolio = FINAL_EQ1_w * monthly.returns +
           FINAL_C_w * monthly.returns.1)

# Expected value 

FINAL_pf_exp2 <- mean(FINAL2_pf_ret$portfolio[time_index:(time_index-11)])

# Sigma

FINAL_pf_sigma2 <- sd(FINAL2_pf_ret$portfolio[time_index:(time_index-11)])*sqrt(12)

# Sharpe

FINAL_pf_sharpe2 <- (((1+FINAL_pf_exp2)^12)-1 - risk_free)/pf_sigma

