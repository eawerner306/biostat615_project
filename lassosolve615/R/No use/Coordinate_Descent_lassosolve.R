

Coordinate_Descent_lassosolve <- function(X, y, lambda, max_iter = 1000, tol = 1e-6) {
  # Ensure required package is available
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("The 'glmnet' package is required but not installed.")
  }
  
  # Fit the model using glmnet
  fit <- glmnet::glmnet(
    X, y, alpha = 1, lambda = lambda,
    intercept = TRUE, maxit = max_iter, thresh = tol
  )
  
  # Extract results
  intercept <- fit$a0[1]                        # Intercept term
  beta <- as.vector(fit$beta[, 1])              # Coefficients without intercept
  full_beta <- c(intercept, beta)               # Combine intercept and coefficients
  names(full_beta) <- NULL                      # Remove names (e.g., "s0")
  iterations <- fit$npasses                     # Number of passes (iterations)
  
  # Convergence check
  has_converged <- iterations < max_iter
  
  # Return result
  return( list(
    beta = full_beta,
    iter = iterations,
    convergence = has_converged
  ))
  
}
