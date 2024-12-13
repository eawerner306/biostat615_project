library(Matrix)

generate_simulated_data <- function(data_size = "small", feature_ratio = "low", sparsity_level = "low", seed = 9248) {
  set.seed(seed)
  
  # Define the ranges for feature size, data size, and sparsity
  # feature_size_map <- list("low" = 50, "medium" = 500, "high" = 1500)
  data_size_map <- list("small" = 500, "medium" = 5000, "large" = 50000, "very_large" = 200000)
  feature_ratio_map <- list("low" = 0.1, "normal" = 0.5, "high" = 1, "high_dimensional" = 2)
  sparsity_map <- list("low" = 0.3, "medium" = 0.65, "high" = 0.9)  # Sparsity as zero proportion
  
  # Validate inputs
  # if (!(feature_size %in% names(feature_size_map))) {
  #   stop("Invalid feature_size. Must be one of: low, medium, high.")
  # }
  if (!(data_size %in% names(data_size_map))) {
    stop("Invalid data_size. Must be one of: small, medium, large, very_large.")
  }
  if (!(feature_ratio %in% names(feature_ratio_map))) {
    stop("Invalid feature_ratio. Must be one of: low, normal, high, high_dimensional.")
  }
  if (!(sparsity_level %in% names(sparsity_map))) {
    stop("Invalid sparsity_level. Must be one of: low, medium, high.")
  }
  
  # Get the number of features and samples
  # p <- feature_size_map[[feature_size]]
  n <- data_size_map[[data_size]]
  p <- n * feature_ratio_map[[feature_ratio]]
  
  # Sparsity level (proportion of zero elements in X)
  sparsity <- sparsity_map[[sparsity_level]]
  
  # Generate the feature matrix (X) with the specified sparsity
  X <- matrix(0, nrow = n, ncol = p)  # Initialize with zeros
  num_non_zero <- round((1 - sparsity) * n * p)  # Total non-zero elements
  non_zero_indices <- sample(1 : (n * p), size = num_non_zero, replace = FALSE)
  X[non_zero_indices] <- rnorm(num_non_zero)  # Fill non-zero entries with random values
  
  # Generate the true sparse coefficients (beta)
  beta <- rep(0, p)
  num_non_zero_beta <- max(1, round(0.1 * p))  # Assume 10% of beta are non-zero
  non_zero_beta_indices <- sample(1 : p, size = num_non_zero_beta, replace = FALSE)
  beta[non_zero_beta_indices] <- runif(length(non_zero_beta_indices), min = -1, max = 1)
  
  # Generate the response vector (y) with some noise
  y <- X %*% beta + rnorm(n)
  
  # Return the generated data
  return(list(X = X, y = y, beta = beta))
}

# small data_size
data_sll <- generate_simulated_data(data_size = "small", feature_ratio = "low", sparsity_level = "low")
data_slm <- generate_simulated_data(data_size = "small", feature_ratio = "low", sparsity_level = "medium")
data_slh <- generate_simulated_data(data_size = "small", feature_ratio = "low", sparsity_level = "high")

data_snl <- generate_simulated_data(data_size = "small", feature_ratio = "normal", sparsity_level = "low")
data_snm <- generate_simulated_data(data_size = "small", feature_ratio = "normal", sparsity_level = "medium")
data_snh <- generate_simulated_data(data_size = "small", feature_ratio = "normal", sparsity_level = "high")

data_shl <- generate_simulated_data(data_size = "small", feature_ratio = "high", sparsity_level = "low")
data_shm <- generate_simulated_data(data_size = "small", feature_ratio = "high", sparsity_level = "medium")
data_shh <- generate_simulated_data(data_size = "small", feature_ratio = "high", sparsity_level = "high")

# data_shdl <- generate_simulated_data(data_size = "small", feature_ratio = "high_dimensional", sparsity_level = "low")
# data_shdm <- generate_simulated_data(data_size = "small", feature_ratio = "high_dimensional", sparsity_level = "medium")
# data_shdh <- generate_simulated_data(data_size = "small", feature_ratio = "high_dimensional", sparsity_level = "high")

# medium data_size
data_mll <- generate_simulated_data(data_size = "medium", feature_ratio = "low", sparsity_level = "low")
data_mlm <- generate_simulated_data(data_size = "medium", feature_ratio = "low", sparsity_level = "medium")
data_mlh <- generate_simulated_data(data_size = "medium", feature_ratio = "low", sparsity_level = "high")

data_mnl <- generate_simulated_data(data_size = "medium", feature_ratio = "normal", sparsity_level = "low")
data_mnm <- generate_simulated_data(data_size = "medium", feature_ratio = "normal", sparsity_level = "medium")
data_mnh <- generate_simulated_data(data_size = "medium", feature_ratio = "normal", sparsity_level = "high")

data_mhl <- generate_simulated_data(data_size = "medium", feature_ratio = "high", sparsity_level = "low")
data_mhm <- generate_simulated_data(data_size = "medium", feature_ratio = "high", sparsity_level = "medium")
data_mhh <- generate_simulated_data(data_size = "medium", feature_ratio = "high", sparsity_level = "high")

# data_mhdl <- generate_simulated_data(data_size = "medium", feature_ratio = "high_dimensional", sparsity_level = "low")
# data_mhdm <- generate_simulated_data(data_size = "medium", feature_ratio = "high_dimensional", sparsity_level = "medium")
# data_mhdh <- generate_simulated_data(data_size = "medium", feature_ratio = "high_dimensional", sparsity_level = "high")

# large data_size
data_lll <- generate_simulated_data(data_size = "large", feature_ratio = "low", sparsity_level = "low")
data_llm <- generate_simulated_data(data_size = "large", feature_ratio = "low", sparsity_level = "medium")
data_llh <- generate_simulated_data(data_size = "large", feature_ratio = "low", sparsity_level = "high")

data_lnl <- generate_simulated_data(data_size = "large", feature_ratio = "normal", sparsity_level = "low")
data_lnm <- generate_simulated_data(data_size = "large", feature_ratio = "normal", sparsity_level = "medium")
data_lnh <- generate_simulated_data(data_size = "large", feature_ratio = "normal", sparsity_level = "high")

data_lhl <- generate_simulated_data(data_size = "large", feature_ratio = "high", sparsity_level = "low")
data_lhm <- generate_simulated_data(data_size = "large", feature_ratio = "high", sparsity_level = "medium")
data_lhh <- generate_simulated_data(data_size = "large", feature_ratio = "high", sparsity_level = "high")

# data_lhdl <- generate_simulated_data(data_size = "large", feature_ratio = "high_dimensional", sparsity_level = "low")
# data_lhdm <- generate_simulated_data(data_size = "large", feature_ratio = "high_dimensional", sparsity_level = "medium")
# data_lhdh <- generate_simulated_data(data_size = "large", feature_ratio = "high_dimensional", sparsity_level = "high")

# very_large data_size
data_vlll <- generate_simulated_data(data_size = "very_large", feature_ratio = "low", sparsity_level = "low")
data_vllm <- generate_simulated_data(data_size = "very_large", feature_ratio = "low", sparsity_level = "medium")
data_vllh <- generate_simulated_data(data_size = "very_large", feature_ratio = "low", sparsity_level = "high")

data_vlnl <- generate_simulated_data(data_size = "very_large", feature_ratio = "normal", sparsity_level = "low")
data_vlnm <- generate_simulated_data(data_size = "very_large", feature_ratio = "normal", sparsity_level = "medium")
data_vlnh <- generate_simulated_data(data_size = "very_large", feature_ratio = "normal", sparsity_level = "high")

data_vlhl <- generate_simulated_data(data_size = "very_large", feature_ratio = "high", sparsity_level = "low")
data_vlhm <- generate_simulated_data(data_size = "very_large", feature_ratio = "high", sparsity_level = "medium")
data_vlhh <- generate_simulated_data(data_size = "very_large", feature_ratio = "high", sparsity_level = "high")

# data_vlhdl <- generate_simulated_data(data_size = "very_large", feature_ratio = "high_dimensional", sparsity_level = "low")
# data_vlhdm <- generate_simulated_data(data_size = "very_large", feature_ratio = "high_dimensional", sparsity_level = "medium")
# data_vlhdh <- generate_simulated_data(data_size = "very_large", feature_ratio = "high_dimensional", sparsity_level = "high")

library(glmnet)

compare_lasso_methods <- function(X, y, lambda) {
  methods <- c("CGDA", "ISTA", "FISTA", "LARS", "PFA", "SLA")
  results <- list()

  for (method in methods) {
    cat("\nRunning lasso with method:", method, "\n")

    # Measure system time for robust_lasso
    robust_time <- system.time(
      robust_result <- robust_lasso(X, y, lambda, method = method)
    )
    # print(robust_result$method, robust_result$fit$iter, robust_result$fit$convergence)
    print(robust_result$method)
    print(robust_result$fit$iter)
    print(robust_result$fit$convergence)

    # Extract predictions and residuals from robust_lasso
    robust_beta <- robust_result$fit$beta
    y_pred_robust <- cbind(1, X) %*% robust_beta  # Include intercept
    residuals_robust <- y - y_pred_robust

    # Calculate metrics for robust_lasso
    mse_robust <- mean(residuals_robust^2)
    r2_robust <- 1 - sum(residuals_robust^2) / sum((y - mean(y))^2)

    # Store results for robust_lasso
    results[[method]] <- list(
      time = robust_time["elapsed"],
      mse = mse_robust,
      r2 = r2_robust
    )

    # Plot residuals for robust_lasso
    plot(y_pred_robust, residuals_robust,
         main = paste("Residual Plot for robust_lasso (", method, ")"),
         xlab = "Predicted Values", ylab = "Residuals",
         col = "blue", pch = 20)
    abline(h = 0, col = "red", lty = 2)
  }

  # Run glmnet
  cat("\nRunning glmnet\n")
  glmnet_time <- system.time(
    glmnet_result <- glmnet(X, y, alpha = 1, lambda = lambda, intercept = TRUE)
  )

  # Extract predictions and residuals from glmnet
  glmnet_beta <- as.vector(coef(glmnet_result, s = lambda))
  y_pred_glmnet <- cbind(1, X) %*% glmnet_beta  # Include intercept
  residuals_glmnet <- y - y_pred_glmnet

  # Calculate metrics for glmnet
  mse_glmnet <- mean(residuals_glmnet^2)
  r2_glmnet <- 1 - sum(residuals_glmnet^2) / sum((y - mean(y))^2)

  # Store results for glmnet
  results[["glmnet"]] <- list(
    time = glmnet_time["elapsed"],
    mse = mse_glmnet,
    r2 = r2_glmnet
  )

  # Plot residuals for glmnet
  plot(y_pred_glmnet, residuals_glmnet,
       main = "Residual Plot for glmnet",
       xlab = "Predicted Values", ylab = "Residuals",
       col = "green", pch = 20)
  abline(h = 0, col = "red", lty = 2)

  # Print and return results
  print(results)
  return(results)
}

lasso_sll <- compare_lasso_methods(data_sll$X, data_sll$y, lambda = 0.1)
lasso_slm <- compare_lasso_methods(data_slm$X, data_slm$y, lambda = 0.1)
lasso_slh <- compare_lasso_methods(data_slh$X, data_slh$y, lambda = 0.1)
lasso_snl <- compare_lasso_methods(data_snl$X, data_snl$y, lambda = 0.1)
lasso_snm <- compare_lasso_methods(data_snm$X, data_snm$y, lambda = 0.1)
lasso_snh <- compare_lasso_methods(data_snh$X, data_snh$y, lambda = 0.1)
lasso_shl <- compare_lasso_methods(data_shl$X, data_shl$y, lambda = 0.1)
lasso_shm <- compare_lasso_methods(data_shm$X, data_shm$y, lambda = 0.1)
lasso_shh <- compare_lasso_methods(data_shh$X, data_shh$y, lambda = 0.1)

lasso_mll <- compare_lasso_methods(data_mll$X, data_mll$y, lambda = 0.1)
lasso_mlm <- compare_lasso_methods(data_mlm$X, data_mlm$y, lambda = 0.1)
lasso_mlh <- compare_lasso_methods(data_mlh$X, data_mlh$y, lambda = 0.1)
lasso_mnl <- compare_lasso_methods(data_mnl$X, data_mnl$y, lambda = 0.1)
lasso_mnm <- compare_lasso_methods(data_mnm$X, data_mnm$y, lambda = 0.1)
lasso_mnh <- compare_lasso_methods(data_mnh$X, data_mnh$y, lambda = 0.1)
lasso_mhl <- compare_lasso_methods(data_mhl$X, data_mhl$y, lambda = 0.1)
lasso_mhm <- compare_lasso_methods(data_mhm$X, data_mhm$y, lambda = 0.1)
lasso_mhh <- compare_lasso_methods(data_mhh$X, data_mhh$y, lambda = 0.1)

