# This is for CGDA implementation
# We have custom implementation to know number of iterations done
# Glmnet does not inherently contain this information

# Input:
# X: design matrix (n x p)
# y: response vector (n x 1)
# lambda: regularization parameter
# max_iter: maximum number of iterations
# tol: tolerance for convergence

# Output:
# beta: estimated coefficients
# iter: number of iterations performed
# convergence: boolean indicating whether convergence was achieved

CGDA_lassosolve <- function(X, y, lambda, max_iter = 1000, tol = 1e-6) {

  # Add intercept column to X
  X <- cbind(1, X)  # 添加一列全 1 表示截距
  n <- nrow(X)
  p <- ncol(X)

  # Precompute X'X and X'y
  XtX <- crossprod(X)
  Xty <- crossprod(X, y)

  # Initialize beta
  beta <- rep(0, p)  # β^(0)

  # Soft-thresholding function
  soft_threshold <- function(z, gamma) {
    sign(z) * pmax(0, abs(z) - gamma)
  }

  # CGDA Iterations
  for (iter in 1:max_iter) {
    beta_prev <- beta

    # Coordinate-wise updates
    for (j in 1:p) {
      # Compute the partial residual
      r_j <- Xty[j] - sum(XtX[j, -j] * beta[-j])

      # Update β_j^(k+1)
      beta[j] <- soft_threshold(r_j / n, lambda) / (XtX[j, j] / n)
    }

    # Check convergence
    if (sqrt(sum((beta - beta_prev)^2)) < tol) {
      return(list(beta = beta, iter = iter, convergence = TRUE))
    }
  }

  # Return results
  return(list(beta = beta, iter = max_iter, convergence = FALSE))
}

# set.seed(123)

# # Example data
# n <- 100
# p <- 10
# X <- matrix(rnorm(n * p), n, p)           # 设计矩阵
# beta_true <- c(5, 1, -1, rep(0, p - 2))  # 包括截距项
# y <- X %*% beta_true[-1] + beta_true[1] + rnorm(n)  # 响应变量

# # Regularization parameter
# lambda <- 0.1

# # Solve using CGDA with intercept column
# result <- CGDA_lassosolve(X, y, lambda)
# print(result)
