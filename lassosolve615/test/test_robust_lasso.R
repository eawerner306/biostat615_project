x <- runif(100) * 10
y <- x + rnorm(100)
robust_lasso(x, y, method = "ga")