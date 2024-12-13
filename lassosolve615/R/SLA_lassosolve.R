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
