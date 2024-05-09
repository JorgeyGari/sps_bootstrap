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


# Initial model with all covariates
initial_model <- rlm(y ~ x1 + x2 + x3, data = data)

# Function to fit model with selected covariates
fit_model <- function(covariates) {
  formula <- as.formula(paste("y ~", paste(covariates, collapse = "+")))
  return(rlm(formula, data = data))
}

# Backward elimination
covariates <- c("x1", "x2", "x3")
while (length(covariates) > 0) {
  # Fit model with selected covariates
  current_model <- fit_model(covariates)
  
  # Calculate bootstrap confidence intervals for coefficients
  bootstrap_results <- bootstrap(x = 1:nobs, nboot = B, theta = rrpair, xdata = data)
  boot_ci <- apply(bootstrap_results$thetastar, MARGIN = 2, calculate_ci)
  
  # Check which covariates to remove
  remove_covariates <- covariates[boot_ci[1, ] > 0 & boot_ci[2, ] < 0]  # Remove covariates with confidence intervals including zero
  
  # If no covariates to remove, break the loop
  if (length(remove_covariates) == 0) {
    break
  }
  
  # Update covariates by removing non-significant ones
  covariates <- setdiff(covariates, remove_covariates)
}

# Print the final selected covariates
print("Final selected covariates:")
print(covariates)

# Print the final regression model
final_model <- fit_model(covariates)
print(summary(final_model))

