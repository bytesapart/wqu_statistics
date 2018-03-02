# Install the quantmod package
# install.packages('quantmod')
# install.packages('graphics')

# Require the package quantmod
require('quantmod')
library('graphics')
# require('PortfolioAnalytics')
# require('PerformanceAnalytics')

# Set the Tickers for which the data needs to be fetched
Tickers <- c("MSFT", "GOOG", "FB", "AMZN", "CSCO")

# Get the 1 Year Data for each Ticker
Ticker.Data <- getSymbols(Symbols = Tickers,
                          return.class = "xts",
                          from = "2016-03-19",
                          to = Sys.Date())

# Set every ticker's Close Price
MSFT.Adj <- Ad(MSFT)
GOOG.Adj <- Ad(GOOG)
FB.Adj <- Ad(FB)
AMZN.Adj <- Ad(AMZN)
CSCO.Adj <- Ad(CSCO)

# Get the Monthly Return of for each Ticker
MSFT.Monthly <- monthlyReturn(MSFT.Adj)
GOOG.Monthly <- monthlyReturn(GOOG.Adj)
FB.Monthly <- monthlyReturn(FB.Adj)
AMZN.Monthly <- monthlyReturn(AMZN.Adj)
CSCO.Monthly <- monthlyReturn(CSCO.Adj)

# Create the Combined Portfolio
NUMBER_OF_COMBOS = 3  # Constant Variable
port <- combn(c("MSFT.Monthly", "GOOG.Monthly", "FB.Monthly", "AMZN.Monthly", "CSCO.Monthly"), NUMBER_OF_COMBOS)

# Construct Portfolio
for(i in 1:as.numeric(length(port) / NUMBER_OF_COMBOS)) {
  assign(paste("portfolio", i, sep=""),rowMeans(merge(eval(as.name(port[1,i])), eval(as.name(port[2,i])), eval(as.name(port[3,i])))))
}

# Cumulative_Sum of All Portfolios
for(i in 1:as.numeric(length(port) / NUMBER_OF_COMBOS)){
  assign(paste("portfolio_cumsum", i, sep=""), cumsum(eval(as.name(paste("portfolio", i, sep="")))))
}

# Set Colors
coloring = rainbow(as.numeric(length(port) / NUMBER_OF_COMBOS))

plot(0, 0, xlim = c(1,13), ylim = c(-5,35),
     xlab = "Month", ylab = "Cumulative Monthly Return", type = "n")
# lines(c(portfolio_cumsum1 * 100, portfolio_cumsum2 * 100, portfolio_cumsum3 * 100, portfolio_cumsum4 * 100, portfolio_cumsum5 * 100,
#      portfolio_cumsum6 * 100, portfolio_cumsum7 * 100, portfolio_cumsum8 * 100, portfolio_cumsum9 * 100, portfolio_cumsum10 * 100))

# Calculate the mean, median, standard deviation and plot them
for(i in 1:as.numeric(length(port) / NUMBER_OF_COMBOS)){
  assign(paste("mean_portfolio", i, sep=""), mean(eval(as.name(paste("portfolio", i, sep="")))))
  assign(paste("median_portfolio", i, sep=""), median(eval(as.name(paste("portfolio", i, sep="")))))
  assign(paste("std_portfolio", i, sep=""), sd(eval(as.name(paste("portfolio", i, sep="")))))
}

variance_sum = sum(std_portfolio1 ** 2, std_portfolio2 ** 2, std_portfolio3 ** 2, std_portfolio4 ** 2, std_portfolio5 ** 2,
                   std_portfolio6 ** 2, std_portfolio7 ** 2, std_portfolio8 ** 2, std_portfolio9 ** 2, std_portfolio10 ** 2)

# Plot the lines on the graph
for(i in 1:length(coloring)) {
  lines(eval(as.name(paste("portfolio_cumsum", i ,sep=""))) * 100, col = coloring[i])
  points(eval(as.name(paste("mean_portfolio", i ,sep=""))) * 100, col = coloring[i])
  text(1, 30 - 2 * i, 
       paste(
         paste("portfolio", i ,sep=""), 
         'Monthy return stats:',
         'Mean = ', format(round(eval(as.name(paste("mean_portfolio", i ,sep=""))) * 100, 2), nsmall = 2),
         'Median = ', format(round(eval(as.name(paste("median_portfolio", i ,sep=""))) * 100, 2), nsmall = 2),
         'SD = ', format(round(eval(as.name(paste("std_portfolio", i ,sep=""))) * 100, 2), nsmall = 2)
       ),
       cex=0.5)  
}

legend(x = "bottomright", # location of legend within plot area
       c("Portfolio1", "Portfolio2", "Portfolio3","Portfolio4", "Portfolio5",
         "Portfolio6", "Portfolio7", "Portfolio8", "Portfolio9", "Portfolio10"),
       col = coloring,
       lwd = c(2, 2, 2),
       cex = 0.4)

# Plot the Histogram
hist(portfolio1, breaks = 6, prob = TRUE)
hist(portfolio2, breaks = 6, prob = TRUE)
hist(portfolio3, breaks = 6, prob = TRUE)
hist(portfolio4, breaks = 6, prob = TRUE)
hist(portfolio5, breaks = 6, prob = TRUE)
hist(portfolio6, breaks = 6, prob = TRUE)
hist(portfolio7, breaks = 6, prob = TRUE)
hist(portfolio8, breaks = 6, prob = TRUE)
hist(portfolio9, breaks = 6, prob = TRUE)
hist(portfolio10, breaks = 6, prob = TRUE)
