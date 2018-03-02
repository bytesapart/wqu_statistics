# Install the quantmod package
# install.packages('quantmod')

# Require the package quantmod
require('quantmod')

# Get the 2 Year NASDAQ Composite Data
nasdaq.composite <- getSymbols(Symbols = "^IXIC",
                               return.class = "xts",
                               from = "2015-03-10",
                               to = Sys.Date())

# Get the 1 Year Daily Returns 
daily.returns <- dailyReturn(IXIC, subset = "2016-03-10::")

# Get the Adjusted Close
adjusted.close <- Ad(IXIC)

# Plot a Line Chart
line.chart <- lineChart(Ad(IXIC))

# Plot a histogram
histogram <- hist(daily.returns, 
                  breaks = 25, 
                  prob = TRUE,
                  col = "peachpuff",
                  border = "black",
                  xlab = "Daily Returns",
                  main = "Histogram of Daily Returns for IXIC")

# Plot the Standard Deviation lines on the histogram
lines(density(daily.returns), lwd = 2, col = "chocolate3")

# Plot the line for Mean on the Histogram
abline(v = mean(daily.returns),
       col = "royalblue",
       lwd = 2)

# Plot the line for Median on the Histogram
abline(v = median(daily.returns),
       col = "red",
       lwd = 2)

# Add a legend to the plot
legend(x = "topright", # location of legend within plot area
       c("Density plot", "Mean", "Median"),
       col = c("chocolate3", "royalblue", "red"),
       lwd = c(2, 2, 2),
       cex = 0.75)
