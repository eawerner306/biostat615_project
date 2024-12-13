# # This is SLA implementation
#
# # Input:
# # X: design matrix (n x p)
# # y: response vector (n x 1)
# # lambda: regularization parameter
# # alpha: smoothness hyper-parameter controlling the closeness of φ_α to |x|
# # max_iter: maximum number of iterations
# # tol: tolerance for convergence
#
# # Output:
# # beta: estimated coefficients
# # iter: number of iterations performed
# # convergence: boolean indicating whether convergence was achieved

# Ethan
SLA_lassosolve <- function(X, y, lambda, alpha = 10, max_iter = 1000, tol = 1e-6) {
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
#
# # Example usage
# set.seed(123)
# n <- 100
# p <- 10
# X <- matrix(rnorm(n * p), n, p)
# beta_true <- c(1, -1, rep(0, p - 2))
# y <- X %*% beta_true + rnorm(n)
# lambda <- 0.1
#
# result <- SLA_lassosolve(X, y, lambda)
# print(result)


# jack
SLA_lassosolve <- function(X, y, lambda = NULL, alpha = 10, max_iter = 1000, tol = 1e-6) {
  # SLA_lassosolve: Smooth Lasso Algorithm for Lasso regression
  # Inputs:
  #   X        : Predictor matrix (n x p)
  #   y        : Response vector (n x 1)
  #   lambda   : Regularization parameter for Lasso penalty
  #   alpha    : Parameter for smoothing (larger alpha -> closer to L1 penalty)
  #   max_iter : Maximum number of iterations
  #   tol      : Tolerance for convergence
  # Outputs:
  #   beta        : Estimated coefficients (including intercept)
  #   iter        : Number of iterations performed
  #   convergence : Boolean indicating if the algorithm converged

  # Standardize X and y
  X_means <- colMeans(X)
  X_sds <- apply(X, 2, sd)
  X_std <- scale(X, center = X_means, scale = X_sds)
  y_mean <- mean(y)
  y_centered <- y - y_mean

  # Add intercept column to X
  X_aug <- cbind(1, X_std)  # Include intercept term

  # Dimensions
  n <- nrow(X_aug)
  p <- ncol(X_aug)

  # Precompute constants
  XtX <- crossprod(X_aug) / n  # (X^T X) / n
  Xty <- crossprod(X_aug, y_centered) / n  # (X^T y) / n

  # Default lambda calculation
  if (is.null(lambda)) {
    lambda <- max(abs(Xty[-1])) / n  # Default lambda based on maximum correlation
  }

  # Compute maximum eigenvalue for step size
  eigenvalues <- eigen(XtX, symmetric = TRUE, only.values = TRUE)$values
  sigma_max_sq <- max(eigenvalues)

  # Step size
  mu <- 1 / (sigma_max_sq + lambda * alpha / 2)

  # Smooth surrogate gradient function
  # surrogate_gradient <- function(beta) {
  #   # Smooth approximation of the L1 penalty gradient
  #   safe_beta <- ifelse(abs(beta) < 1e-8, 1e-8, beta)  # Avoid division by zero
  #   exp_alpha_beta <- exp(alpha * safe_beta)
  #   exp_neg_alpha_beta <- exp(-alpha * safe_beta)
  #   term1 <- -2 * log(1 + exp_neg_alpha_beta) / safe_beta^2
  #   term2 <- (2 * alpha * exp_alpha_beta) / (safe_beta * (1 + exp_alpha_beta)) - 1
  #   v <- term1 + term2
  #   v[!is.finite(v)] <- 0  # Handle non-finite values
  #   return(lambda * v)
  # }
  surrogate_gradient <- function(beta) {
    grad <- tanh(alpha * beta / 2)
    return(grad)
  }

  # Initialization
  beta <- rep(0, p)  # Initial beta (including intercept)
  beta_prev <- beta
  convergence <- FALSE

  # SLA Iterations
  for (iter in 1:max_iter) {
    # Extrapolation parameter
    gamma <- (iter - 1) / (iter + 2)

    # Extrapolated point
    w <- beta + gamma * (beta - beta_prev)

    # Gradient computation
    gradient <- XtX %*% w - Xty + surrogate_gradient(w)

    # Update beta
    beta_new <- w - mu * gradient

    # Check convergence
    if (sqrt(sum((beta_new - beta)^2)) < tol) {
      convergence <- TRUE
      beta <- beta_new
      break
    }

    # Update beta values for next iteration
    beta_prev <- beta
    beta <- beta_new
  }

  # Rescale coefficients to original scale
  beta_rescaled <- beta
  beta_rescaled[-1] <- beta_rescaled[-1] / X_sds  # Adjust for standardization
  intercept <- beta_rescaled[1] - sum((X_means / X_sds) * beta[-1])  # Adjust intercept
  beta_rescaled[1] <- intercept + y_mean  # Final intercept

  # Return results
  return(list(beta = beta_rescaled, iter = iter, convergence = convergence))
}
