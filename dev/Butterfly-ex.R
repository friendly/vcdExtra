#' ---
#' title: Visualizing the Butterfly data
#' ---
#' 
#
library(vcdExtra)
library(Hmisc)

data("Butterfly", package = "vcdExtra")
Butterfly

#' ## make a nice barplot
bp <- barplot(Butterfly,
        xlab = "Number of times caught",
        ylab = "Number of species")

#' ## find weighted mean & std.
butter.df <- as.data.frame(Butterfly, stringsAsFactors=FALSE)
x <- as.numeric(butter.df[,1])
freq <- as.numeric(butter.df[,2])
butter.mean <- wtd.mean(x = x, weights = freq)
butter.std  <- wtd.var(x = x, weights = freq) |> sqrt()


#' ## add to plot
abline(v=bp[1] + butter.mean, lwd = 2, col = "red")
arrows(x0 = bp[1] + butter.mean - butter.std,
       x1 = bp[1] + butter.mean + butter.std,
       y0 = 5, y1 = 5,
       lwd = 2, col = "red",
       angle = 15, code = 3)


#'  ## Poissonness plot
#' A "poissonness plot" plots a 'count metameter', \phi(n_k), against k
#' such that this will follow a straight line for a Poisson distribution
#'
#  \phi(n_k) = \log(k! n_k / N) = - \lambda + \log(k) *k
#'
#' If the points fit a straight line, slope and intercept give estimates of \lambda
#'   slope = \log(\lambda)   ---> \hat{\lambda} = exp (slope)
#'   intercept = -\lambda    --->  \hat{\lambda} = - intercept

distplot(Butterfly, type = "poisson",
         xlab = "Number of times caught",
         main = "Poissonness plot for Butterfly data")

# ## Fit the poisson distribution
#
fit <- goodfit(Butterfly, type = "poisson")

print(fit, digits = 3)

# chis
summary(fit)

# plot the goodfit object
plot(fit, shade = TRUE,
     xlab = "Number of times caught")


# ord plot
Ord_plot(Butterfly,
         gp = gpar(cex=1), pch = 16)

# -----------------------------------------
# ## estimate number of uncaught species

yt <- butter.df[, "Freq"] |> as.numeric()

# Eqn 6.19: Corbet's formula
Et <- function(t=1/2, y=yt) {
  n <- length(y)
  signs <- rep( c(1, -1), out.length = n )
  terms <- y * t^(1:n) * signs
  sum(terms)
}

SDt <- function(t=1/2, y = yt) {
  n <- length(y)
  x = 1:n
  terms <- y * t^(2*x)
  sum(terms) |> sqrt()
}

Et(y = yt)
SDt(y = yt)

# Table 6.3
sapply((1:10)/10, FUN=Et, y = yt) |> round(digits=3)

sapply((1:10)/10, FUN=SDt, y = yt) |> round(digits=3)


