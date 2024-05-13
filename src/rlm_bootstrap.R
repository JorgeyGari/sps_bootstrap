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
estimates <- bootstrap(x = 1:nobs, nboot = B, theta = rrpair, xdata = data)$thetastar

# Calculate the CIs for each of the coefficients
ci_intercept <- 2 * coef['(Intercept)'] - quantile(estimates[1,], c(0.975, 0.025))
ci_x1 <- 2 * coef['x1'] - quantile(estimates[2,], c(0.975, 0.025))
ci_x2 <- 2 * coef['x2'] - quantile(estimates[3,], c(0.975, 0.025))
ci_x3 <- 2 * coef['x3'] - quantile(estimates[4,], c(0.975, 0.025))

# Combine the CIs into a table
boot_ci <- cbind(ci_intercept, ci_x1, ci_x2, ci_x3)
boot_ci # x3 is not significant because the 0 is contained in the CI

# PART 2: Backward elimination
# Bootstrap resampling
rrpair <- function(x, xdata) {
  rlm(y ~ x1 + x2, data = xdata[x, ], maxit = maxit)$coefficients # Extract coefficients
}
rlm_model <- rlm(y ~ x1 + x2, data = data[, -4], maxit = maxit)

# PART 3: Confidence intervals for the remaining variables
estimates <- bootstrap(x = 1:nobs, nboot = B, theta = rrpair, xdata = data[, -4])$thetastar
ci_intercept <- 2 * coef['(Intercept)'] - quantile(estimates[1,], c(0.975, 0.025))
ci_x1 <- 2 * coef['x1'] - quantile(estimates[2,], c(0.975, 0.025))
ci_x2 <- 2 * coef['x2'] - quantile(estimates[3,], c(0.975, 0.025))

boot_ci <- cbind(ci_intercept, ci_x1, ci_x2)

boot_ci # Both x1 and x2 are significant

# PART 4: Prediction
b_0 <- mean(boot_ci[, 1])
b_1 <- mean(boot_ci[, 2])
b_2 <- mean(boot_ci[, 3])
prediction <- b_0 + b_1 * 14 + b_2 * 14
print(prediction)
