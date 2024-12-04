FISTA_solver <- function(X, y, lambda, max_iter, tol = 1e-6) {
  x <- rep(0, ncol(X))
  z <- x
  t <- 1
  pobj <- numeric(max_iter)
  L <- norm(X, "2")^2
  start_time <- Sys.time()
  
  for (iter in 1:max_iter) {
    x_old <- x
    z <- z + t(X) %*% (y - X %*% z) / L
    x_new <- soft_thresh(z, lambda / L)
    t_old <- t
    t <- (1 + sqrt(1 + 4 * t^2)) / 2
    z <- x_new + ((t_old - 1) / t) * (x_new - x_old)
    pobj[iter] <- 0.5 * norm(X %*% x_new - y, "F")^2 + lambda * sum(abs(x_new))
    
    # Check convergence
    if (sqrt(sum((x_new - x)^2)) < tol) {
      return(list(x = x_new, pobj = pobj[1 : iter], 
                  time = Sys.time() - start_time, 
                  convergence = TRUE, iter = iter))
    }
    x <- x_new
  }
  
  list(x = x, pobj = pobj, time = Sys.time() - start_time, 
       convergence = FALSE, iter = max_iter)
}

soft_thresh <- function(x, lambda) {
  sign(x) * pmax(abs(x) - lambda, 0)
}

set.seed(9248)
m <- 50
n <- 200
X <- matrix(rnorm(m * n), nrow = m)
x0 <- runif(n)
x0[x0 < 0.9] <- 0
y <- X %*% x0
lambda <- 0.5

result_fista <- FISTA_solver(X, y, lambda, max_iter = 10000, tol = 1e-5)
result_fista
