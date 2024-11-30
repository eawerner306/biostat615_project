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

library(GA)

robust_lasso <- function(x, y, lambda = NULL, method = "auto", options = list()) {
  n <- nrow(x)
  p <- ncol(p)
  sparsity <- sum(abs(colMeans(X)) < 1e-5) / p

  # Deciding which algorithm gets chosen
  # future algorithms that could be chosen: ista, fista, sla
  if (method == "auto") {
    # arbitrary numbers for small matrices
    if (p > 10 * n) {
      chosen_method <- "fista" #TODO: implement FISTA
    } else if (p <= n & sparsity > 0.7) {
      chosen_method <- "coordinate_descent"
    } else if (p <= n & sparsity <= 0.7) {
      algorithm <- "ista" #TODO: implement ISTA
    } else if (p < 500 & p <= n) {
      chosen_method <- "lars"
    } else {
      chosen_method <- "fista"
    }

    # if (p > 10000 || n > 5000) {
    #   chosen_method <- "coordinate_descent"
    # } else if (is.null(lambda)) {
    #   chosen_method <- "lars"
    # } else {
    #   chosen_method <- "coordinate_descent"
    # }
  } else {
    chosen_method <- method
  }

  # Fitting the mode
  fit <- switch(
    # add new algorithms added to the list
    chosen_method,
    "coordinate_descent" = glmnet::glmnet(x,y,lambda=lambda),
    "lars" = lars::lars(x,y),
    "fista" = glmnet::glmnet(x,y,lambda=lambda,standardize=FALSE),
    "ga" = {
      lower_bounds <- rep(-10, p)
      upper_bounds <- rep(10, p)

      ga_fit <- GA::ga(
        type = "real-valued",
        fitness = fitness_function,
        lower = lower_bounds,
        upper = upper_bounds,
        popSize = options$popSize %||% 50,
        maxiter = options$maxiter %||% 100,
        run = options$run %||% 50,
        parallel = options$parallel %||% TRUE
      )
      ga_fit
    },
    stop("Unknown method")
  )

  return(list(method = chosen_method, fit = fit))
}
