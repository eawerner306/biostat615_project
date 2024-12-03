

lars_lassosolve <- function(X, y, lambda) {
  # Ensure the 'lars' package is available
  if (!requireNamespace("lars", quietly = TRUE)) {
    stop("The 'lars' package is required but not installed.")
  }
  
  # Fit the model using lars
  fit <- lars::lars(X, y, type = "lasso", intercept = TRUE)
  
  # Extract coefficients for the specified lambda
  # Note: lars does not directly support lambda, so we approximate it
  beta_path <- predict(fit, s = lambda, type = "coefficients", mode = "lambda")
  beta <- beta_path$coefficients  # Coefficients without intercept
  
  # Calculate the intercept separately
  intercept <- mean(y) - colMeans(X) %*% beta
  
  # Combine intercept and coefficients
  beta_with_intercept <- c(intercept, beta)
  
  # Convergence status (lars does not explicitly handle convergence)
  # Here, we assume convergence if a solution is returned
  has_converged <- !is.null(beta_path)
  
  # Return the result as a list
  return(list(
    beta = beta_with_intercept,
    convergence = has_converged
  ))
}
