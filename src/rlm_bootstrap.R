# Loading libraries
library(MASS)
library(bootstrap)

set.seed(1)

# Loading data
data <- read.csv("data/data_100452172.csv")
nobs <- nrow(data)  # No. of observations

# Build the RLM
rlm_model <- rlm(y ~ x1 + x2 + x3, data = data)
coef(summary(rlm_model))

# Bootstrap resampling
rrpair <- function(x, xdata) {
  rlm(y ~ x1 + x2 + x3, data = xdata[x,])$coefficients  # Extract coefficients
}

B <- 1000
bootstrap_results <- bootstrap(x = 1:nobs, nboot = B, theta = rrpair, xdata = data)

# Function to calculate percentile confidence intervals
calculate_ci <- function(estimates) {
  lower <- quantile(estimates, 0.025)  # Lower bound (2.5%)
  upper <- quantile(estimates, 0.975)  # Upper bound (97.5%)
  return(c(lower, upper))
}

# Calculate the CIs for each of the coefficients
boot_ci <- apply(bootstrap_results$thetastar, MARGIN = 2, calculate_ci)
head(boot_ci)
