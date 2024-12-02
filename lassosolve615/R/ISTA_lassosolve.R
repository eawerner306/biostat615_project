# ISTA implementation file

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

# ISTA_lassosolve <- function(X, y, lambda, max_iter = 1000, tol = 1e-6) {

#   n <- nrow(X)
#   p <- ncol(X)

#   # Precompute X'X and X'y
#   XtX <- crossprod(X)
#   Xty <- crossprod(X, y)

#   # Lipschitz constant (L = max eigenvalue of XtX / n)
#   L <- max(eigen(XtX / n)$values)

#   # Initialize beta
#   beta <- rep(0, p)

#   # Soft-thresholding function
#   soft_threshold <- function(z, gamma) {
#     sign(z) * pmax(0, abs(z) - gamma)
#   }

#   # ISTA Iterations
#   for (iter in 1:max_iter) {
#     # Gradient
#     gradient <- (XtX %*% beta - Xty) / n
#     beta_new <- soft_threshold(beta - (1 / L) * gradient, lambda / L)

#     # Check convergence
#     if (sqrt(sum((beta_new - beta)^2)) < tol) {
#       return(list(beta = beta_new, iter = iter, convergence = TRUE))
#     }

#     # Update beta
#     beta <- beta_new
#   }

#   return(list(beta = beta, iter = max_iter, convergence = FALSE))
# }

# # # Example usage
# # set.seed(123)
# # n <- 100
# # p <- 10
# # X <- matrix(rnorm(n * p), n, p)
# # beta_true <- c(1, -1, rep(0, p - 2))
# # y <- X %*% beta_true + rnorm(n)
# # lambda <- 0.1
# #
# # result <- ISTA_lassosolve(X, y, lambda)
# # print(result)

# ISTA_lassosolve <- function(X, y, lambda, max_iter = 1000, tol = 1e-6) {

#   n <- nrow(X)
#   p <- ncol(X)

#   # Add intercept column to X (if not already added)
#   X <- cbind(1, X)  # 添加一列全 1 的常数列作为截距项
#   p <- ncol(X)      # 更新特征数量

#   # Precompute X'X and X'y
#   XtX <- crossprod(X)
#   Xty <- crossprod(X, y)

#   # Lipschitz constant (L = max eigenvalue of XtX / n)
#   L <- max(eigen(XtX / n)$values)

#   # Initialize beta
#   beta <- rep(0, p)

#   # Soft-thresholding function
#   soft_threshold <- function(z, gamma) {
#     sign(z) * pmax(0, abs(z) - gamma)
#   }

#   # ISTA Iterations
#   for (iter in 1:max_iter) {
#     # Gradient
#     gradient <- (XtX %*% beta - Xty) / n
#     beta_new <- soft_threshold(beta - (1 / L) * gradient, lambda / L)

#     # Check convergence
#     if (sqrt(sum((beta_new - beta)^2)) < tol) {
#       return(list(beta = beta_new, iter = iter, convergence = TRUE))
#     }

#     # Update beta
#     beta <- beta_new
#   }

#   return(list(beta = beta, iter = max_iter, convergence = FALSE))
# }

# set.seed(123)

# # 模拟数据
# n <- 100  # 样本数
# p <- 10   # 特征数
# X <- matrix(rnorm(n * p), n, p)           # 特征矩阵
# beta_true <- c(5, 1, -1, rep(0, p - 2))  # 包括截距项（5）和稀疏系数
# y <- X %*% beta_true[-1] + beta_true[1] + rnorm(n)  # 生成响应变量

# # 正则化参数
# lambda <- 0.1

# # 调用 ISTA_lassosolve
# result <- ISTA_lassosolve(X, y, lambda)


# ISTA_lassosolve <- function(X, y, lambda, max_iter = 1000, tol = 1e-6) {

#   n <- nrow(X)
#   p <- ncol(X)

#   # Center X and y
#   X_mean <- colMeans(X)
#   y_mean <- mean(y)
#   X_centered <- scale(X, center = X_mean, scale = FALSE)
#   y_centered <- y - y_mean

#   # Precompute X'X and X'y
#   XtX <- crossprod(X_centered) / n
#   Xty <- crossprod(X_centered, y_centered) / n

#   # Lipschitz constant
#   L <- max(eigen(XtX, symmetric = TRUE, only.values = TRUE)$values)

#   # Initialize beta
#   beta <- rep(0, p)

#   # Soft-thresholding function
#   soft_threshold <- function(z, gamma) {
#     sign(z) * pmax(0, abs(z) - gamma)
#   }

#   # ISTA Iterations
#   for (iter in 1:max_iter) {
#     gradient <- XtX %*% beta - Xty
#     beta_new <- soft_threshold(beta - (1 / L) * gradient, lambda / L)

#     # Check convergence
#     if (sqrt(sum((beta_new - beta)^2)) < tol) {
#       intercept <- y_mean - sum(X_mean * beta_new)
#       full_beta <- c(intercept, beta_new)  # Combine intercept and coefficients
#       return(list(beta = full_beta, iter = iter, convergence = TRUE))
#     }

#     beta <- beta_new
#   }

#   # Final intercept calculation
#   intercept <- y_mean - sum(X_mean * beta)
#   full_beta <- c(intercept, beta)

#   return(list(beta = full_beta, iter = max_iter, convergence = FALSE))
# }

ISTA_lassosolve <- function(X, y, lambda, max_iter = 1000, tol = 1e-6) {
  n <- nrow(X)
  p <- ncol(X)

  # Center X and y for numerical stability
  X_mean <- colMeans(X)
  y_mean <- mean(y)
  X_centered <- scale(X, center = X_mean, scale = FALSE)
  y_centered <- y - y_mean

  # Precompute X'X and X'y
  XtX <- crossprod(X_centered) / n
  Xty <- crossprod(X_centered, y_centered) / n

  # Lipschitz constant (L = max eigenvalue of XtX)
  L <- max(eigen(XtX, symmetric = TRUE, only.values = TRUE)$values)

  # Initialize beta (use Ridge regression initialization for better accuracy)
  beta <- solve(XtX + diag(lambda, p), Xty)

  # Soft-thresholding function
  soft_threshold <- function(z, gamma) {
    sign(z) * pmax(0, abs(z) - gamma)
  }

  # ISTA Iterations
  for (iter in 1:max_iter) {
    # Gradient calculation
    gradient <- XtX %*% beta - Xty
    beta_new <- soft_threshold(beta - (1 / L) * gradient, lambda / L)

    # Check convergence using relative error
    if (sqrt(sum((beta_new - beta)^2)) / max(1, sqrt(sum(beta^2))) < tol) {
      intercept <- y_mean - sum(X_mean * beta_new)  # Restore intercept
      full_beta <- c(intercept, beta_new)          # Combine intercept and coefficients
      return(list(beta = full_beta, iter = iter, convergence = TRUE))
    }

    # Update beta
    beta <- beta_new
  }

  # Final intercept calculation
  intercept <- y_mean - sum(X_mean * beta)
  full_beta <- c(intercept, beta)

  return(list(beta = full_beta, iter = max_iter, convergence = FALSE))
}


