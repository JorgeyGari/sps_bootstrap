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
y_hat <- b_0 + b_1 * 14 + b_2 * 14

# Confidence level (e.g., 95%)
confidence_level <- 0.95

# Z-value for the given confidence level
z_value <- qnorm((1 + confidence_level) / 2)

# Standard errors for b_0, b_1, and b_2
se_b0 <- sd(estimates[1,])
se_b1 <- sd(estimates[2,])
se_b2 <- sd(estimates[3,])

# Calculate standard error for y_hat
se_y_hat <- sqrt((se_b0)^2 + (14 * se_b1)^2 + (14 * se_b2)^2)

# Calculate margin of error
margin_of_error <- z_value * se_y_hat

# Calculate lower and upper bounds of the confidence interval for y_hat
lower_bound <- y_hat - margin_of_error
upper_bound <- y_hat + margin_of_error

# Confidence interval for y_hat
ci_y_hat <- c(lower_bound, upper_bound)

