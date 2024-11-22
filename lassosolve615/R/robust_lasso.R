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

robust_lasso <- function(x, y, lambda = NULL, method = "auto", options = list()) {
  n <- nrow(x)
  p <- ncol(p)

  # Deciding which algorithm gets chosen
  # future algorithms that could be chosen: ista, fista, sla
  if (method == "auto") {
    # arbitrary numbers for small matrices
    if (p > 10000 || n > 5000) {
      chosen_method <- "coordinate_descent"
    } else if (is.null(lambda)) {
      chosen_method <- "lars"
    } else {
      chosen_method <- "coordinate_descent"
    }
  } else {
    chosen_method <- method
  }

  # Fitting the mode
  fit <- switch(
    # add new algorithms added to the list
    chosen_method,
    "coordinate_descent" = glmnet::glmnet(x,y,lambda=lambda),
    "lars" = lars::lars(x,y),
    stop("Unknown method")
  )

  return(list(method = chosen_method, fit = fit))
}
