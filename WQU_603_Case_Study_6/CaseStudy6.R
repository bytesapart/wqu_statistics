# Require the Quantmod package
require(quantmod)

# Get the 1 Year DJIA Data
DJIA_index <- getSymbols(Symbols = "DJIA",
                         src = "yahoo",
                         from = "2016-03-31",
                         to = "2017-03-31",
                         retrun.class = "xts")

# Get the components
DJIA_tickers <- c("AAPL", "AXP", "BA", "CAT", "CSCO",
                  "CVX", "KO", "DD", "XOM", "GE",
                  "GS", "HD", "IBM", "INTC", "JNJ",
                  "JPM", "MCD", "MMM", "MRK", "MSFT",
                  "NKE", "PFE", "PG", "TRV", "UNH",
                  "UTX", "V", "VZ", "WMT", "DIS")

DJIA_components <- getSymbols(Symbols = DJIA_tickers,
                              src = "yahoo",
                              from = "2016-03-31",
                              to = "2017-03-31",
                              return.class = "xts")

# Get the Daily returns
# DJIA Index Daily Return
daily_DJIA_index <- dailyReturn(DJIA)

# DJIA Components Daily Returns
for(i in 1:length(DJIA_tickers)) {
  assign(paste(DJIA_tickers[i], "_daily_DJIA_component", sep=""),
         dailyReturn(eval(as.name(DJIA_tickers[i]))))
}

# Function to calculate Linear Model
calculate_linear_models<- function(daily_index_return, daily_stock_return){
  daily_index_return <- as.vector(daily_index_return)
  daily_stock_return <- as.vector(daily_stock_return)
  map_dataframe <- data.frame(dija_daily = daily_index_return,
                              component_daily = daily_stock_return)
  linear_model <- lm(daily_index_return ~ daily_stock_return, data = map_dataframe)
  alpha <- coef(linear_model)["(Intercept)"]
  beta <- coef(linear_model)["daily_stock_return"]
  return (c(alpha, beta))
}

# Init Variables
component_alphas <- c()
component_betas <- c()

for(i in 1:length(DJIA_tickers)) {
  stock_linear_reg <- calculate_linear_models(daily_DJIA_index,
                                              eval(as.name(paste(DJIA_tickers[i],"_daily_DJIA_component",sep = ""))))
  print(paste('The alpha for stock ', DJIA_tickers[i], ' is ', stock_linear_reg[1], sep=''))
  print(paste('The beta for stock ', DJIA_tickers[i], ' is ', stock_linear_reg[2], sep=''))
  
  component_alphas <- c(component_alphas, stock_linear_reg[1])
  component_betas <- c(component_betas, stock_linear_reg[2])
}

# Plot the graphs
hist(component_alphas, breaks=20, main="Alpha Distribution")
hist(component_betas, breaks=20, main="Beta Distribution")