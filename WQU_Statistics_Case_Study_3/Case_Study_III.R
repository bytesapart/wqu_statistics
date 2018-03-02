# Given,
# s[t+1] = s[t] * exp ((r - 0.5 * (sig ^ 2)) * dt + sigma * epsilon * sqrt(dt))
# where,
# s[t] is the stock price at t
# S[t+1] is the stock price at t+1
# r is the expected annual stock return
# sigma is the annualized volatility of the underlying stock
# T is time in Years
# n is the number of steps involved in calculation
# dt is the unit step size, that is, dt = T/n
# epsilon is random value from rnom()

# Initialize the variables to the values given in the problem
st <- 10  # The stock price at 't', that is, the first stock price
r <- 0.15  # Expected anual stock return
sigma <- 0.2  # Annualized Volatility of the stock
t <- 1  # Time in years
n <- 100  # Number of steps, in this case, 100
epsilon <- 0.15  # Random value from a normal distribution

# Function to calculate price movement
calculate_price_movement = function(st, r, sigma, t, n , rand_eps_flag) {
  # Create a vector to contain the price_movements_at_st+1
  st_plus_one <- vector()
  # Since our vector consists of t,t+1,t+2...t+n, therefore the first
  # element of our vector will be st. Therefore,
  st_plus_one[1] <- st
  # See if random epsilon is true, if so, then use the random epsilon
  # Calculate dt
  dt <- t / n
  # Use a for loop and calculate st+1 till steps
  for(i in 1:n) {
    if(rand_eps_flag) {
      epsilon <- rnorm(1)
    } else {
      epsilon <- 0.15
    }
    st_plus_one[i + 1] <- st_plus_one[i] * exp(((r - (0.5 * (sigma ^ 2))) * dt + (sigma * epsilon * sqrt(dt)))) 
  }
  return (st_plus_one)
}

# Calculate value for given_epsilon_prices where the epsilon is given in the question
given_epsilon_prices <- calculate_price_movement(st,r,sigma,t,n,FALSE)

# Plot the graph of given_epsilon_prices
plot(given_epsilon_prices,
     main = 'Given epsilon in the question for each time step',
     xlab = 'Step', 
     ylab = 'Stock Price', 
     xlim = c(0, 100), 
     ylim = c(0, 20), 
     type='l',
     col="chocolate")

# Calculate for random epsilon
random_epsilon_prices <- calculate_price_movement(st,r,sigma,t,n,TRUE)
# Plot the graph of price_plus_one
plot(random_epsilon_prices, 
     main = 'Random epsilon using rnom() in each time step',
     xlab = 'Step', 
     ylab = 'Stock Price', 
     xlim = c(0, 100), 
     ylim = c(0, 20), 
     type='l',
     col="magenta")

# Plot for 5 trials of 100 steps each
colors_of_lines <- c("red", "blue", "green", "yellow", "orange")
the_trials <- matrix(nrow = 101,
                     ncol = 5,
                     byrow = FALSE)

for(i in 1:5) {
  the_trials[, i] <- calculate_price_movement(st, r, sigma, t, n, TRUE)
}

matplot(the_trials, 
     main = 'Five Trials with Random Elipson in each time step',
     xlab = 'Step', 
     ylab = 'Stock Price', 
     xlim = c(0, 100), 
     ylim = c(0, 20), 
     type='l',
     col=colors_of_lines)
legend("bottomright", inset=.05, legend=c("Trial 1", "Trial 2", "Trial 3", "Trial 4", "Trial 5"), lty = 1, col=colors_of_lines, horiz=TRUE, cex = 0.8)