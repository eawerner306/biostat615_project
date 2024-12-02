# TODO: add comments and description
# TODO: uniform the output format (glmnet, lars)
# TODO: add the option to consider the intercept term
# beta: include intercept
library(here)
#source("CGDA_lassosolve.R")
source(here("R/CGDA_lassosolve.R"))
source(here("R/FISTA_lassosolve.R"))
source(here("R/ISTA_lassosolve.R"))
source(here("R/Coordinate_Descent_lassosolve.R"))
# source(here("R/PFA_lassosolve.R"))
source(here("R/SLA_lassosolve.R"))
library(glmnet)

choose_lasso_algorithm <- function(X, y, lambda, priority = "accuracy", 
                                   data_size = NULL, feature_size = NULL, 
                                   sparsity = NULL, compute_resources = NULL, precision = NULL) {
  

  # auto mode
  if (is.null(data_size)) {
    n_samples <- nrow(X)
    data_size <- ifelse(n_samples < 1000, "small",
                        ifelse(n_samples < 10000, "medium",
                               ifelse(n_samples < 100000, "large", "very_large")))
  }
  

  if (is.null(feature_size)) {
    n_features <- ncol(X)
    feature_size <- ifelse(n_features < 100, "low",
                           ifelse(n_features < 1000, "medium", "high"))
  }
  
  if (is.null(sparsity)) {
    sparsity <- ifelse(lambda > 0.1, "high",
                       ifelse(lambda > 0.01, "medium", "low"))
  }
  
  if (is.null(compute_resources)) {
    compute_resources <- "medium"  # 默认计算资源为中等
  }
  
  if (is.null(precision)) {
    precision <- ifelse(priority == "accuracy", "high",
                        ifelse(priority == "speed", "low", "medium"))
  }
  
  # choosing the algorithm based on the priority
  if (priority == "speed") {
    if (data_size == "very_large" || compute_resources == "low") {
      return("SLA")
    } else if (data_size %in% c("medium", "large") && sparsity == "high") {
      return("Coordinate_Descent")
    } else {
      return("FISTA")
    }
  }
  
  if (priority == "accuracy") {
    if (precision == "high" && sparsity == "high") {
      if (data_size == "small" && feature_size == "high") {
        return("LARS")
      } else if (data_size %in% c("medium", "large")) {
        return("FISTA")
      }
    } else if (precision == "medium") {
      return("Coordinate_Descent")
    } else {
      return("ISTA")
    }
  }
  
  if (priority == "sparsity") {
    if (sparsity == "high") {
      if (data_size == "small" && feature_size == "high") {
        return("LARS")
      } else if (feature_size == "high") {
        return("PFA")
      } else {
        return("Coordinate_Descent")
      }
    } else {
      return("SLA")
    }
  }
  
  if (priority == "scalability") {
    if (data_size == "very_large") {
      return("SLA")
    } else if (data_size == "large" && sparsity == "high") {
      return("Coordinate_Descent")
    } else {
      return("CGDA")
    }
  }
  
  # default output
  return("No suitable algorithm found. Please check your inputs.")
}

# Robust Lasso with PFA integration
robust_lasso <- function(X, y, lambda = NULL, method = "auto", priority = NULL, 
                                   data_size = NULL, feature_size = NULL, 
                                   sparsity = NULL, compute_resources = NULL, precision = NULL) {
  
  if (method!="auto"&&!is.null(priority)){
    warning("The priority argument is only used when method is set to 'auto'.")
  }
  if (is.null(priority)){
    priority <- "accuracy"
  }

  # Deciding which algorithm gets chosen
  if (method == "auto") {
    method <- choose_lasso_algorithm(X, y, lambda, priority, 
                                   data_size = NULL, feature_size = NULL, 
                                   sparsity = NULL, compute_resources = NULL, precision = NULL)
  }
  
  # Fitting the model
  fit <- switch(
    method,
    "Coordinate_Descent" = Coordinate_Descent_lassosolve(X, y, lambda = lambda),
    "LARS" = lars::lars(X, y),
    "FISTA" = FISTA_lassosolve(X, y, lambda = lambda),
    "SLA" = SLA_lassosolve(X, y, lambda = lambda), 
    "CGDA" = CGDA_lassosolve(X, y, lambda = lambda),
    "ISTA" = ISTA_lassosolve(X, y, lambda = lambda),
    "PFA" = PFA_lassosolve(X, y, lambda = lambda),
    stop("Unknown method")
  )

  return(list(method = method, fit = fit))
}



