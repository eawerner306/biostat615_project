# TODO: add comments and description
# TODO: uniform the output format (glmnet, lars)
# TODO: add the option to consider the intercept term
# beta: include intercept
library(here)
#source("CGDA_lassosolve.R")
source(here("R/CGDA_lassosolve.R"))
source(here("R/FISTA_lassosolve.R"))
source(here("R/ISTA_lassosolve.R"))
# source(here("R/Coordinate_Descent_lassosolve.R"))
source(here("R/PFA_lassosolve.R"))
source(here("R/SLA_lassosolve.R"))
source(here("R/LARS_lassosolve.R"))
# library(glmnet)

# choose_lasso_algorithm <- function(X, y, lambda, priority = "accuracy", 
#                                    data_size = NULL, feature_size = NULL, 
#                                    sparsity = NULL, compute_resources = NULL, precision = NULL) {
  

#   # auto mode
#   if (is.null(data_size)) {
#     n_samples <- nrow(X)
#     data_size <- ifelse(n_samples < 1000, "small",
#                         ifelse(n_samples < 10000, "medium",
#                                ifelse(n_samples < 100000, "large", "very_large")))
#   }
  

#   if (is.null(feature_size)) {
#     n_features <- ncol(X)
#     feature_size <- ifelse(n_features < 100, "low",
#                            ifelse(n_features < 1000, "medium", "high"))
#   }
  
#   if (is.null(sparsity)) {
#     sparsity <- ifelse(lambda > 0.1, "high",
#                        ifelse(lambda > 0.01, "medium", "low"))
#   }
  
#   if (is.null(compute_resources)) {
#     compute_resources <- "medium"  # 默认计算资源为中等
#   }
  
#   if (is.null(precision)) {
#     precision <- ifelse(priority == "accuracy", "high",
#                         ifelse(priority == "speed", "low", "medium"))
#   }
  
#   # choosing the algorithm based on the priority
#   if (priority == "speed") {
#     if (data_size == "very_large" || compute_resources == "low") {
#       return("SLA")
#     } else if (data_size %in% c("medium", "large") && sparsity == "high") {
#       return("Coordinate_Descent")
#     } else {
#       return("FISTA")
#     }
#   }
  
#   if (priority == "accuracy") {
#     if (precision == "high" && sparsity == "high") {
#       if (data_size == "small" && feature_size == "high") {
#         return("LARS")
#       } else if (data_size %in% c("medium", "large")) {
#         return("FISTA")
#       }
#     } else if (precision == "medium") {
#       return("Coordinate_Descent")
#     } else {
#       return("ISTA")
#     }
#   }
  
#   if (priority == "sparsity") {
#     if (sparsity == "high") {
#       if (data_size == "small" && feature_size == "high") {
#         return("LARS")
#       } else if (feature_size == "high") {
#         return("PFA")
#       } else {
#         return("Coordinate_Descent")
#       }
#     } else {
#       return("SLA")
#     }
#   }
  
#   if (priority == "scalability") {
#     if (data_size == "very_large") {
#       return("SLA")
#     } else if (data_size == "large" && sparsity == "high") {
#       return("Coordinate_Descent")
#     } else {
#       return("CGDA")
#     }
#   }
  
#   # default output
#   return("No suitable algorithm found. Please check your inputs.")
# }

# # Robust Lasso with PFA integration
# robust_lasso <- function(X, y, lambda = NULL, method = "auto", priority = NULL, 
#                                    data_size = NULL, feature_size = NULL, 
#                                    sparsity = NULL, compute_resources = NULL, precision = NULL) {
  
#   if (method!="auto"&&!is.null(priority)){
#     warning("The priority argument is only used when method is set to 'auto'.")
#   }
#   if (is.null(priority)){
#     priority <- "accuracy"
#   }

#   # Deciding which algorithm gets chosen
#   if (method == "auto") {
#     method <- choose_lasso_algorithm(X, y, lambda, priority, 
#                                    data_size = NULL, feature_size = NULL, 
#                                    sparsity = NULL, compute_resources = NULL, precision = NULL)
#   }
  
#   # Fitting the model
#   fit <- switch(
#     method,
#     "Coordinate_Descent" = Coordinate_Descent_lassosolve(X, y, lambda = lambda),
#     "LARS" = lars::lars(X, y),
#     "FISTA" = FISTA_lassosolve(X, y, lambda = lambda),
#     "SLA" = SLA_lassosolve(X, y, lambda = lambda), 
#     "CGDA" = CGDA_lassosolve(X, y, lambda = lambda),
#     "ISTA" = ISTA_lassosolve(X, y, lambda = lambda),
#     "PFA" = PFA_lassosolve(X, y, lambda = lambda),
#     stop("Unknown method")
#   )

#   return(list(method = method, fit = fit))
# }


# choose_lasso_algorithm <- function(X, y, lambda, priority = "accuracy", 
#                                    data_size = NULL, feature_size = NULL, 
#                                    sparsity = NULL, compute_resources = NULL, precision = NULL) {
  
#   # Determine data size automatically if not provided
#   if (is.null(data_size)) {
#     n_samples <- nrow(X)
#     data_size <- ifelse(n_samples < 1000, "small",
#                         ifelse(n_samples < 10000, "medium",
#                                ifelse(n_samples < 100000, "large", "very_large")))
#   }
  
#   # Determine feature size automatically if not provided
#   if (is.null(feature_size)) {
#     n_features <- ncol(X)
#     feature_size <- ifelse(n_features < 100, "low",
#                            ifelse(n_features < 1000, "medium", "high"))
#   }
  
#   # Estimate sparsity automatically if not provided
#   if (is.null(sparsity)) {
#     sparsity <- ifelse(lambda > 0.1, "high",
#                        ifelse(lambda > 0.01, "medium", "low"))
                      
#   }
  
#   # Default compute resources and precision
#   if (is.null(compute_resources)) {
#     compute_resources <- "medium"
#   }
#   if (is.null(precision)) {
#     precision <- ifelse(priority == "accuracy", "high",
#                         ifelse(priority == "speed", "low", "medium"))
#   }
  
#   # Algorithm selection based on priority
#   if (priority == "speed") {
#     if (data_size == "very_large" || compute_resources == "low") {
#       return("SLA")
#     } else if (data_size %in% c("medium", "large") && sparsity == "high") {
#       return("CGDA")
#     } else {
#       return("FISTA")
#     }
#   }
  
#   if (priority == "accuracy") {
#     if (precision == "high" && sparsity == "high") {
#       if (data_size == "small" && feature_size == "high") {
#         return("LARS")
#       } else if (data_size %in% c("medium", "large")) {
#         return("FISTA")
#       }
#     } else if (precision == "medium") {
#       return("CGDA")
#     } else if (precision == "low" && sparsity == "medium" && data_size == "small") {
#       return("ISTA")  # Only select ISTA for small data, medium sparsity, and low precision
#     } else {
#       return("FISTA")
#     }
#   }
  
#   if (priority == "sparsity") {
#     if (sparsity == "high") {
#       if (data_size == "small" && feature_size == "high") {
#         return("LARS")
#       } else if (feature_size == "high") {
#         return("PFA")
#       } else {
#         return("CGDA")
#       }
#     } else {
#       return("SLA")
#     }
#   }
  
#   if (priority == "scalability") {
#     if (data_size == "very_large") {
#       return("SLA")
#     } else if (data_size == "large" && sparsity == "high") {
#       return("CGDA")
#     } else {
#       return("FISTA")
#     }
#   }
  
#   # Default output
#   return("No suitable algorithm found. Please check your inputs.")
# }


# # Robust Lasso Wrapper
# robust_lasso <- function(X, y, lambda = NULL, method = "auto", priority = NULL, 
#                          data_size = NULL, feature_size = NULL, 
#                          sparsity = NULL, compute_resources = NULL, precision = NULL) {
  
#   # Handle priority and method
#   if (method != "auto" && !is.null(priority)) {
#     warning("The priority argument is only used when method is set to 'auto'.")
#   }
#   if (is.null(priority)) {
#     priority <- "accuracy"
#   }
  
#   # Select algorithm automatically if method is 'auto'
#   if (method == "auto") {
#     method <- choose_lasso_algorithm(X, y, lambda, priority, 
#                                      data_size, feature_size, sparsity, compute_resources, precision)
#   }
  
#   print(method)

#   # Fit the model using the chosen algorithm
#   fit <- switch(
#     method,
#     # "LARS" = lars::lars(X, y),
#     "LARS" = lars_lassosolve(X, y, lambda = lambda),
#     "FISTA" = FISTA_lassosolve(X, y, lambda = lambda),
#     "ISTA" = ISTA_lassosolve(X, y, lambda = lambda),
#     "SLA" = SLA_lassosolve(X, y, lambda = lambda), 
#     "CGDA" = CGDA_lassosolve(X, y, lambda = lambda),
#     "PFA" = PFA_lassosolve(X, y, lambda = lambda),
#     stop("Unknown method")
#   )
  
#   return(list(method = method, fit = fit))
# }

choose_lasso_algorithm <- function(X, y, lambda = 0.1, priority = NULL, 
                                      data_size = NULL, feature_size = NULL, sparsity_level = NULL) {
  # Default priorities: accuracy, speed, sparsity, scalability
  # lambda: regularization parameter
  
  if(is.null(priority)) {
    priority <- "accuracy"
  }

  # Helper function to calculate sparsity
  calculate_sparsity <- function(data) {
    total_elements <- length(data)
    non_zero_elements <- sum(data != 0)
    sparsity <- 1 - (non_zero_elements / total_elements)
    return(sparsity)
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
  if(is.null(feature_size)) {
    feature_size <- ifelse(n_features < 100, "low",
                           ifelse(n_features < 1000, "medium", "high"))
  }

  
  # Determine sparsity level
  if(is.null(sparsity_level)) {
    sparsity_level <- ifelse(sparsity > 0.8, "high",
                             ifelse(sparsity > 0.5, "medium", "low"))
  }
  
  
  # Select algorithm based on priority
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
      # return("CGDA")
      return("LARS")
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

# Example Wrapper for Lasso
robust_lasso <- function(X, y, lambda = 0.1,method = "auto", priority = NULL,
                        data_size = NULL, feature_size = NULL, sparsity_level = NULL) {
  # Choose algorithm
  if (method != "auto" && !is.null(priority)) {
    warning("The priority argument is only used when method is set to 'auto'.")
  }

  if (is.null(priority)) {
    priority <- "accuracy"
  }

  if(method == "auto") {
  method <- choose_lasso_algorithm(X, y, lambda, priority, data_size, feature_size, sparsity_level)
  }else(method = method)
  # Run selected algorithm
  fit <- switch(
    method,
    "LARS" = lars_lassosolve(X, y, lambda = lambda),
    "PFA" = PFA_lassosolve(X, y, lambda = lambda),
    "ISTA" = ISTA_lassosolve(X, y, lambda = lambda),
    "FISTA" = FISTA_lassosolve(X, y, lambda = lambda),
    "CGDA" = CGDA_lassosolve(X, y, lambda = lambda),
    "SLA" = SLA_lassosolve(X, y, lambda = lambda),
    stop("Unknown algorithm")
  )
  
  # Return result
  return(list(method = method, fit = fit))
}
