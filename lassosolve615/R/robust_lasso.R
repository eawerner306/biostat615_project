#' Dynamic lasso problem solve
#' Robustly and evaluates given data for best algorithm to handle LASSO
#'
#' @param x matrix of predictors
#' @param y vector of responses
#' @param lambda vector of lambda values for regularization in glmnet
#' @param method different types of algorithms
#' @return a fitted model for the chosen algorithm
#' @export
#'

# TODO: add comments and description
# TODO: uniform the output format (glmnet, lars)
# # TODO: add the option to consider the intercept term
# # beta: include intercept
# library(here)
# source(here("R/CGDA_lassosolve.R"))
# source(here("R/FISTA_lassosolve.R"))
# source(here("R/ISTA_lassosolve.R"))
# source(here("R/PFA_lassosolve.R"))
# source(here("R/SLA_lassosolve.R"))
# source(here("R/LARS_lassosolve.R"))

choose_lasso_algorithm <- function(X, y, lambda = 0.1, priority = NULL,
                                   data_size = NULL, feature_size = NULL, sparsity_level = NULL) {
  # Helper function to calculate sparsity
  calculate_sparsity <- function(data) {
    total_elements <- length(data)
    non_zero_elements <- sum(data != 0)
    sparsity <- 1 - (non_zero_elements / total_elements)
    return(sparsity)
  }

  # Default priority
  if (is.null(priority)) {
    priority <- "accuracy"
  }

  # Dataset properties
  n_samples <- nrow(X)
  n_features <- ncol(X)
  sparsity <- calculate_sparsity(X)

  # Determine data size
  if (is.null(data_size)) {
    data_size <- ifelse(n_samples < 1000, "small",
                        ifelse(n_samples < 10000, "medium",
                               ifelse(n_samples < 100000, "large", "very_large")))
  }

  # Determine feature size
  if (is.null(feature_size)) {
    feature_size <- ifelse(n_features < 100, "low",
                           ifelse(n_features < 1000, "medium", "high"))
  }

  # Determine sparsity level
  if (is.null(sparsity_level)) {
    sparsity_level <- ifelse(sparsity > 0.8, "high",
                             ifelse(sparsity > 0.5, "medium", "low"))
  }

  # Algorithm selection based on priority and data characteristics
  if (priority == "accuracy") {
    if (data_size == "small" && sparsity_level == "high") {
      return("LARS")
    } else if (data_size %in% c("medium", "large") && sparsity_level == "medium") {
      return("FISTA")
    } else {
      return("ISTA")
    }
  } else if (priority == "speed") {
    if (data_size == "very_large" || sparsity_level == "low") {
      return("SLA")
    } else if (data_size %in% c("medium", "large")) {
      return("CGDA")
    } else {
      return("FISTA")
    }
  } else if (priority == "sparsity") {
    if (sparsity_level == "high" && feature_size == "high") {
      return("PFA")
    } else if (sparsity_level == "high") {
      return("LARS")
    } else {
      return("CGDA")
    }
  } else if (priority == "scalability") {
    if (data_size == "very_large") {
      return("SLA")
    } else if (data_size %in% c("medium", "large")) {
      return("CGDA")
    } else {
      return("FISTA")
    }
  }

  # Default output
  return("No suitable algorithm found. Please check your inputs.")
}

# Robust Lasso Wrapper
robust_lasso <- function(X, y, lambda = 0.1, method = "auto", priority = NULL,
                         data_size = NULL, feature_size = NULL, sparsity_level = NULL) {
  # Choose algorithm
  if (method != "auto" && !is.null(priority)) {
    warning("The priority argument is only used when method is set to 'auto'.")
  }

  if (is.null(priority)) {
    priority <- "accuracy"
  }

  if (method == "auto") {
    method <- choose_lasso_algorithm(X, y, lambda, priority, data_size, feature_size, sparsity_level)
  } else {
    method <- method
  }

  # Run algorithm
  fit <- switch(
    method,
    "LARS" = LARS_lassosolve(X, y, lambda = lambda),
    "PFA" = PFA_lassosolve(X, y, lambda = lambda),
    "ISTA" = ISTA_lassosolve(X, y, lambda = lambda),
    "FISTA" = FISTA_lassosolve(X, y, lambda = lambda),
    "CGDA" = CGDA_lassosolve(X, y, lambda = lambda),
    "SLA" = SLA_lassosolve(X, y, lambda = lambda),
    stop("Unknown algorithm")
  )

  # Return the result
  return(list(method = method, fit = fit))
}
