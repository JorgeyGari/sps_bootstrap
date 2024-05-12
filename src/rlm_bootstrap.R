# Loading libraries
library(MASS)
library(bootstrap)

set.seed(1)

# Loading data
data <- read.csv("data/data_100452172.csv")
nobs <- nrow(data)  # No. of observations

# PART 1: Bootstrap resampling for robust linear regression model
# Build the RLM
rlm_model <- rlm(y ~ x1 + x2 + x3, data = data)
coef_summary <- coef(summary(rlm_model))

# Bootstrap resampling
rrpair <- function(x, xdata) {
  rlm(y ~ x1 + x2 + x3, data = xdata[x,])$coefficients  # Extract coefficients
}

B <- 170  # Number of bootstrap samples
bootstrap_results <- bootstrap(x = 1:nobs, nboot = B, theta = rrpair, xdata = data)

# Function to calculate percentile confidence intervals
calculate_ci <- function(estimates) {
  lower <- quantile(estimates, 0.025)  # Lower bound (2.5%)
  upper <- quantile(estimates, 0.975)  # Upper bound (97.5%)
  return(c(lower, upper))
}

# Calculate the CIs for each of the coefficients
boot_ci <- apply(bootstrap_results$thetastar, MARGIN = 1, calculate_ci)
head(boot_ci) # x3 is the least significant variable
# TODO: Is this the right way to determine the least significant variable?

# PART 2: Backward elimination
B <- 48 # TODO: How to choose the number of bootstrap samples?
# Bootstrap resampling
rrpair <- function(x, xdata) {
  rlm(y ~ x1 + x2, data = xdata[x,])$coefficients  # Extract coefficients
}
rlm_model <- rlm(y ~ x1 + x2, data = data[, -4])

# PART 3: Confidence intervals for the remaining variables
bootstrap_results <- bootstrap(x = 1:nobs, nboot = B, theta = rrpair, xdata = data[, -4])
boot_ci <- apply(bootstrap_results$thetastar, MARGIN = 1, calculate_ci)
head(boot_ci) # Both x1 and x2 are significant

# PART 4: Prediction
# TODO: Mean response?
b_0 <- mean(boot_ci[, 1])
b_1 <- mean(boot_ci[, 2])
b_2 <- mean(boot_ci[, 3])
rlm_model.predict <- predict(rlm_model, newdata = data.frame(x1 = 14, x2 = 14))
rlm_model.predict
b_0 + b_1 * 14 + b_2 * 14
