PFA_lassosolve <- function(X, y, lambda, max_iter = 1000, tol = 1e-6) {
  # Ensure the data dimensions are valid
  n <- nrow(X)
  p <- ncol(X)
  
  # Standardize X and y
  X_means <- colMeans(X)
  X_sds <- apply(X, 2, sd)
  X_std <- scale(X, center = X_means, scale = X_sds)
  y_mean <- mean(y)
  y_centered <- y - y_mean
  
  # Add intercept column to X
  X_aug <- cbind(1, X_std)  # Add a column of 1s for the intercept
  p_aug <- ncol(X_aug)      # Update the number of features (including intercept)
  
  # Initialize coefficients
  beta <- rep(0, p_aug)
  
  # Precompute X'X and X'y
  XtX <- crossprod(X_aug) / n
  Xty <- crossprod(X_aug, y_centered) / n
  
  # Path-following algorithm
  for (iter in 1:max_iter) {
    beta_prev <- beta
    
    for (j in 2:p_aug) {  # Skip intercept in regularization
      # Compute the partial residual
      r_j <- Xty[j] - sum(XtX[j, -j] * beta[-j])
      
      # Soft-threshold update
      beta[j] <- sign(r_j) * max(0, abs(r_j) - lambda) / XtX[j, j]
    }
    
    # Update intercept (not regularized)
    beta[1] <- Xty[1] - sum(XtX[1, -1] * beta[-1])
    
    # Check for convergence
    if (sqrt(sum((beta - beta_prev)^2)) < tol) {
      # Rescale beta back to original scale
      beta_rescaled <- beta
      beta_rescaled[-1] <- beta_rescaled[-1] / X_sds  # Adjust for standardization
      beta_rescaled[1] <- beta[1] + y_mean - sum((X_means / X_sds) * beta[-1])  # Adjust intercept
      
      return(list(beta = beta_rescaled, iter = iter, convergence = TRUE))
    }
  }
  
  # If max_iter is reached without convergence
  beta_rescaled <- beta
  beta_rescaled[-1] <- beta_rescaled[-1] / X_sds
  beta_rescaled[1] <- beta[1] + y_mean - sum((X_means / X_sds) * beta[-1])
  
  return(list(beta = beta_rescaled, iter = max_iter, convergence = FALSE))
}

# # Example usage
# set.seed(123)
# n <- 100
# p <- 10
# X <- matrix(rnorm(n * p), n, p)
# beta_true <- c(1, -1, rep(0, p - 2))
# y <- X %*% beta_true + rnorm(n)
# lambda <- 0.1

# result <- PFA_lassosolve(X, y, lambda)
# print(result)
