SLA_solver <- function(X, y, lambda, alpha = 10, max_iter = 1000, tol = 1e-6) {
  # Dimensions
  n <- nrow(X)
  p <- ncol(X)
  
  # Precompute constants
  XtX <- crossprod(X) / n       # X'X / n
  Xty <- crossprod(X, y) / n    # X'y / n
  sigma_max_sq <- max(eigen(XtX, symmetric = TRUE, only.values = TRUE)$values)
  mu <- 1 / (sigma_max_sq + lambda * alpha / 2)
  
  # Define the correct gradient of the surrogate function (∇φα)
  surrogate_gradient <- function(beta) {
    grad <- tanh(alpha * beta / 2)
    return(grad)
  }
  
  # Initialization
  beta <- rep(0, p)  # β^(0)
  beta_prev <- rep(0, p)  # β^(−1)
  start_time <- Sys.time()
  # Iterative SLA optimization
  for (iter in 1:max_iter) {
    # Extrapolated step
    w <- beta + (iter - 2) / (iter + 1) * (beta - beta_prev)
    
    # Gradient calculation
    gradient <- XtX %*% w - Xty + lambda * surrogate_gradient(w)
    
    # Update step
    beta_new <- w - mu * gradient
    
    # Check convergence
    if (sqrt(sum((beta_new - beta)^2)) < tol) {
      return(list(beta = beta_new, iter = iter, convergence = TRUE, 
                  time = Sys.time() - start_time))
    }
    
    # Update variables for the next iteration
    beta_prev <- beta
    beta <- beta_new
  }
  
  # Return results if not converged within max_iter
  return(list(beta = beta, iter = max_iter, convergence = FALSE, 
              time = Sys.time() - start_time))
}
