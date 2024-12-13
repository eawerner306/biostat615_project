# This script is for implementing LARS algorithm
# Using LARS package
# Work in progress

LARS_lassosolve <- function(X, y, lambda) {
  # Ensure 'lars' is loaded
  if (!requireNamespace("lars", quietly = TRUE)) {
    stop("The 'lars' package is required but not installed.")
  }

  # Fit the model with lars
  fit <- lars::lars(X, y, type = "lasso", intercept = TRUE)

  # Extract coefficients for chosen lambda
  # Note: lars does not directly support lambda, so we approximate it
  beta_path <- predict(fit, s = lambda, type = "coefficients", mode = "lambda")
  beta <- beta_path$coefficients  # Coefficients without intercept

  # Calculate the intercept separately
  intercept <- mean(y) - colMeans(X) %*% beta

  # Combine intercept and coefficients
  beta_with_intercept <- c(intercept, beta)

  # Convergence status (lars does not explicitly handle convergence)
  # assume convergence if a solution is retruned
  has_converged <- !is.null(beta_path)

  # Return the result as a list
  return(list(
    beta = beta_with_intercept,
    convergence = has_converged
  ))
}

# set.seed(123)
# X <- matrix(rnorm(100*10), 100, 10)
# y <- rnorm(100)
# lambda <- 0.1
#
# print(LARS_lassosolve(X,y,lambda))
