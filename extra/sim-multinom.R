# simulate a multinomial logistic model
# from: https://stats.stackexchange.com/questions/103728/simulating-multinomial-logit-data-with-r


n <- 1000           # Number of observations
k <- 3              # Number of variables
d <- 4              # Number of categories
size <- 15          # Expected size of each outcome (number of balls selected)

set.seed(17)
xnames <-  paste0("X", seq_len(k))
beta <- matrix(rnorm((k+1) * d), ncol = d, 
               dimnames=list(c("Intercept", xnames), seq_len(d))) 
beta = beta - beta[, 1] # Standardize: category 1 is the reference category
X <- matrix(runif(n * k), n, dimnames = list(NULL, xnames))

normalize <- function(h) h / rowSums(h)

p <- normalize(exp(cbind(1, X) %*% beta))

s <- 1 + rpois(n, size-1)
y <- t(sapply(seq_len(n), function(i) rmultinom(1, s[i], p[i, ])))

library(nnet)
fit <- multinom(y ~ X)
coef(fit)

#' ------------------------------------------------
#' 

install.packages("HTLR")
library(HTLR)
set.seed(12345)
dat <- gendata_MLR(n = 100, NC=4, p = 3)
ggplot2::qplot(dat$y, bins = 6)
corrplot::corrplot(cor(dat$X))

