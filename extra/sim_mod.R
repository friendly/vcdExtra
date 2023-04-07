#' Simulate data from a multinomial logistic regression example
#' From: https://data.library.virginia.edu/simulating-multinomial-logistic-regression-data/

sim_mod <- function(n){
  # generate predictors
  x <- runif(n = n, min = 0.5, max = 3)
  g <- sample(c("a", "b"), size = n, replace = TRUE)
  # linear predictors
  lp2 <- 3 + -2*x + -0.7*(g == "b")
  lp3 <- -2 + 1.2*x + -0.3*(g == "b")
  # probabilities
  den <- (1 + exp(lp2) + exp(lp3))
  p1 <- 1/den
  p2 <- exp(lp2)/den
  p3 <- exp(lp3)/den
  P <- cbind(p1, p2, p3)
  y <- apply(P, MARGIN = 1, function(x)sample(x = 1:3, size = 1, prob = x))
  d <- data.frame(y = factor(y), x, g)
  d
  }