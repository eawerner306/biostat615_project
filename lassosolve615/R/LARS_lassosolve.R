

# lars_lassosolve <- function(X, y, lambda) {
#   # Ensure the 'lars' package is available
#   if (!requireNamespace("lars", quietly = TRUE)) {
#     stop("The 'lars' package is required but not installed.")
#   }
  
#   # Fit the model using lars
#   fit <- lars::lars(X, y, type = "lasso", intercept = TRUE)
  
#   # Extract coefficients for the specified lambda
#   # Note: lars does not directly support lambda, so we approximate it
#   beta_path <- predict(fit, s = lambda, type = "coefficients", mode = "lambda")
#   beta <- beta_path$coefficients  # Coefficients without intercept
  
#   # Calculate the intercept separately
#   intercept <- mean(y) - colMeans(X) %*% beta
  
#   # Combine intercept and coefficients
#   beta_with_intercept <- c(intercept, beta)
  
#   # Convergence status (lars does not explicitly handle convergence)
#   # Here, we assume convergence if a solution is returned
#   has_converged <- !is.null(beta_path)
  
#   # Return the result as a list
#   return(list(
#     beta = beta_with_intercept,
#     convergence = has_converged
#   ))
# }


lars_lassosolve <- function(X, y, lambda) {
  # 标准化特征矩阵和响应变量
  X_mean <- colMeans(X)
  X_sd <- apply(X, 2, sd)
  X_scaled <- scale(X, center = X_mean, scale = X_sd)
  y_mean <- mean(y)
  y_scaled <- y - y_mean

  n <- nrow(X_scaled)
  p <- ncol(X_scaled)

  # 初始化变量
  active_set <- c()  # 活跃集
  inactive_set <- seq_len(p)  # 非活跃集
  beta <- rep(0, p)  # 回归系数
  residual <- y_scaled  # 残差
  iteration <- 0  # 迭代计数

  # LARS 主循环
  while (length(inactive_set) > 0) {
    iteration <- iteration + 1

    # 计算残差与每个特征的相关性
    correlations <- crossprod(X_scaled, residual)

    # 找到最大相关性的特征
    max_corr <- max(abs(correlations[inactive_set]))
    if (max_corr < lambda) break  # 满足稀疏性条件，退出

    active_feature <- which(abs(correlations) == max_corr)[1]
    active_set <- c(active_set, active_feature)
    inactive_set <- setdiff(inactive_set, active_feature)

    # 构建活跃集的子矩阵
    X_active <- X_scaled[, active_set, drop = FALSE]

    # 计算步长
    G_active <- crossprod(X_active)
    beta_active <- solve(G_active) %*% crossprod(X_active, y_scaled)

    # 更新残差
    residual <- y_scaled - X_active %*% beta_active
  }

  # 将 beta_active 写入 beta
  beta[active_set] <- beta_active

  # 去标准化回归系数
  beta_unscaled <- beta / X_sd

  # 计算截距
  intercept <- y_mean - sum(beta_unscaled * X_mean)

  # 返回结果
  return(list(
    beta = c(intercept, beta_unscaled),
    iterations = iteration,
    convergence = TRUE
  ))
}
