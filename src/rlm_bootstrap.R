# Loading libraries
library(MASS)
library(bootstrap)

set.seed(1)

# Loading data
data <- read.csv("data/data_100452273.csv")
nobs <- nrow(data) # No. of observations

# PART 1: Bootstrap resampling for robust linear regression model
# Build the RLM
maxit <- 50
rlm_model <- rlm(y ~ x1 + x2 + x3, data = data, maxit = maxit)
coef <- rlm_model$coefficients
coef_summary <- coef(summary(rlm_model))

# Bootstrap resampling
rrpair <- function(x, xdata) {
  rlm(y ~ x1 + x2 + x3, data = xdata[x, ], maxit = maxit)$coefficients # Extract coefficients
}

B <- 2000 # Number of bootstrap samples
bootstrap_results <-
  bootstrap(x = 1:nobs, nboot = B, theta = rrpair, xdata = data)$thetastar

# Function to calculate percentile confidence intervals
# FIXME: Basic CI
calculate_ci <- function(estimates) {
  ci_intercept <- 2 * coef['(Intercept)'] - quantile(estimates[1,], c(0.975, 0.025))
  ci_x1 <- 2 * coef['x1'] - quantile(estimates[2,], c(0.975, 0.025))
  ci_x2 <- 2 * coef['x2'] - quantile(estimates[3,], c(0.975, 0.025))
  ci_x3 <- 2 * coef['x3'] - quantile(estimates[4,], c(0.975, 0.025))
  
  ci_table <- cbind(ci_intercept, ci_x1, ci_x2, ci_x3)
  
  return(ci_table)
}

# Calculate the CIs for each of the coefficients
boot_ci <- calculate_ci(bootstrap_results)
boot_ci # x3 is not significant because the 0 is contained in the CI

# PART 2: Backward elimination
# FIXME: Basic CI
# Bootstrap resampling
rrpair <- function(x, xdata) {
  rlm(y ~ x1 + x2, data = xdata[x, ], maxit = maxit)$coefficients # Extract coefficients
}
rlm_model <- rlm(y ~ x1 + x2, data = data[, -4], maxit = maxit)

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
