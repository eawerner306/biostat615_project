ISTA_solver <- function(X, y, lambda, max_iter, tol) {
  x <- rep(0, ncol(X))
  beta <- numeric(max_iter)
  L <- norm(X, "2")^2
  start_time <- Sys.time()
  for (iter in 1 : max_iter) {
    x_new <- soft_thresh(x + t(X) %*% (y - X %*% x) / L, lambda / L)
    beta[iter] <- 0.5 * norm(X %*% x_new - y, "F")^2 + lambda * sum(abs(x_new))
    
    if (sqrt(sum((x_new - x)^2)) < tol) {
      return(list(x = x_new, beta = beta[1 : iter], 
                  time = Sys.time() - start_time, 
                  convergence = TRUE, iter = iter))
    }
    x <- x_new
  }
  return(list(x = x, beta = beta, time = Sys.time() - start_time, 
              convergence = FALSE, iter = max_iter))
}

soft_thresh <- function(x, lambda) {
  sign(x) * pmax(abs(x) - lambda, 0)
}

set.seed(9248)
m <- 500
n <- 2000
X <- matrix(rnorm(m * n), nrow = m)
x0 <- runif(n)
x0[x0 < 0.9] <- 0
y <- X %*% x0
lambda <- 0.5

result_ista <- ISTA_solver(X, y, lambda, max_iter = 100000, tol = 1e-5)
result_ista

