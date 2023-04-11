# Simulate data from a multinomial logistic regression model
# taken from HTLR::gendata_MLR()

simMultinom <-
  function (n, 
            p, 
#            NC = 3, 
            levels = LETTERS[1:3],
            X = NULL, 
            betas = NULL,
            nu = 2, 
            w = 1 
            ) 
  {
    NC <- length(levels)

    if (is.null(X)) {
    X <- matrix(rnorm(n * p), n, p)
    colnames(X) <- paste0("X", 1:p)
    }
    else {
      p <- ncol(X)
    }

    if (is.null(betas)) {
      sigmasbt <- 1/rgamma(p, nu/2, nu/2) * w
      betas <- replicate(NC, rnorm(p + 1)) * c(0, sqrt(sigmasbt))
    }

    deltas <- betas[, -1, drop = FALSE] - betas[, 1]
    lv <- cbind(1, X) %*% betas
#    probs <- exp(lv - as.vector(HTLR:::log_sum_exp(lv)))
    probs <- exp(lv - log(sum(exp(lv))))
    y <- apply(probs, 1, function(prob) {
      sample(levels, 1, TRUE, prob)
    })
    list(X = X, 
         y = y, 
         deltas = deltas)
  }

# test

dat1 <- simMultinom(n=100, p=3)

head(cbind(y=dat1$y, as.data.frame(dat1$X)))
dat1$deltas

# substantive example
party <- c("NDP", "Grn", "Lib", "PC")

set.seed(47)
n <- 100
R <- matrix(c(4, 0.5, 0.3,
              0.5, 2, 0.1,
              0.3, 0.1, 1), 
            nrow = 3, ncol = 3, byrow = TRUE)
mu <- c(20, 10, 3)

X <- MASS::mvrnorm(n, mu = mu, Sigma = R)
colnames(X) <- c("income", "educ", "kids")
X <- round(X)
head(X)

betas <- matrix(c(1, 2, 1,
                  1, 1, -1,
                  1, 0.5, 0.5),
                nrow = 3, ncol = 3, byrow = TRUE) 

choice <- simMultinom(n = 100, levels = party)
#                      X = X)

head(cbind(y=choice$y, as.data.frame(choice$X)))
choice$deltas

table(choice$y)
