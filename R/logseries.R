#' The Logarithmic Series Distribution
#'
#' The logarithmic series distribution is a long-tailed distribution introduced
#' by Fisher etal. (1943) in connection with data on the abundance of
#' individuals classified by species.
#'
#' These functions provide the density, distribution function, quantile
#' function and random generation for the logarithmic series distribution with
#' parameter `prob`.
#'
#' The logarithmic series distribution with `prob` = \eqn{p} has density
#' \deqn{ p ( x ) = \alpha p^x / x } for \eqn{x = 1, 2, \dots},
#' where
#' \eqn{\alpha= -1 / \log(1 - p)} and \eqn{0 < p < 1}.
#' % Note that counts `x==2` cannot occur.
#'
#' @aliases Logseries dlogseries plogseries qlogseries rlogseries
#' @param x,q vector of quantiles representing the number of events.
#' @param prob parameter for the distribution, `0 < prob < 1`
#' @param log,log.p logical; if TRUE, probabilities `p` are given as `log(p)`
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.
#' @param p vector of probabilities
#' @param max.value maximum value returned by `qlogseries`
#' @param n number of observations for `rlogseries`
#'
#' @return `dlogseries` gives the density,
#'         `plogseries` gives the cumulative distribution function,
#'         `qlogseries` gives the quantile function, and
#'         `rlogseries` generates random deviates.
#'
#'
#' @author Michael Friendly, using original code modified from the
#'         `gmlss.dist` package by Mikis Stasinopoulos.
#' @seealso \code{\link[stats]{Distributions}}
#'
#' @references
#' <https://en.wikipedia.org/wiki/Logarithmic_distribution>
#'
#' Fisher, R. A. and Corbet, A. S. and Williams, C. B. (1943). The relation
#' between the number of species and the number of individuals *Journal of
#' Animal Ecology*, 12, 42-58.
#'
#' @keywords distribution
#' @examples
#'
#' XL <-expand.grid(x=1:5, p=c(0.33, 0.66, 0.99))
#' lgs.df <- data.frame(XL, prob=dlogseries(XL[,"x"], XL[,"p"]))
#' lgs.df$p = factor(lgs.df$p)
#' str(lgs.df)
#'
#' require(lattice)
#' mycol <- palette()[2:4]
#' xyplot( prob ~ x, data=lgs.df, groups=p,
#' 	xlab=list('Number of events (k)', cex=1.25),
#' 	ylab=list('Probability',  cex=1.25),
#' 	type='b', pch=15:17, lwd=2, cex=1.25, col=mycol,
#' 	key = list(
#' 					title = 'p',
#' 					points = list(pch=15:17, col=mycol, cex=1.25),
#' 					lines = list(lwd=2, col=mycol),
#' 					text = list(levels(lgs.df$p)),
#' 					x=0.9, y=0.98, corner=c(x=1, y=1)
#' 					)
#' 	)
#'
#'
#' # random numbers
#' hist(rlogseries(200, prob=.4), xlab='x')
#' hist(rlogseries(200, prob=.8), xlab='x')
#'
#'
#'
#-----------------------------------------------------------------------------------------
#' @rdname logseries
#' @export
dlogseries<-function(x, prob = 0.5, log = FALSE)
 {
          if (any(prob <= 0) | any(prob >= 1) )  stop(paste("prob must be greater than 0 and less than 1", "\n", ""))
          if (any(x <= 0) )  stop(paste("x must be >0", "\n", ""))
       logfy <- x*log(prob)-log(x)-log(-log(1-prob))
      if(log == FALSE) fy <- exp(logfy) else fy <- logfy
          fy
  }

#----------------------------------------------------------------------------------------
#' @rdname logseries
#' @export
plogseries <- function(q, prob = 0.5, lower.tail = TRUE, log.p = FALSE)
  {
          if (any(prob <= 0) | any(prob >= 1) )  stop(paste("prob must be greater than 0 and less than 1", "\n", ""))
   if (any(q <= 0) )  stop(paste("q must be >0", "\n", ""))
        ly <- length(q)
       FFF <- rep(0,ly)
       nmu <- rep(prob, length = ly)
         j <- seq(along=q)
   for (i in j)
      {
        y.y <- q[i]
         mm <- nmu[i]
     allval <- seq(1,y.y)
     pdfall <- dlogseries(allval, prob = mm, log = FALSE)
     FFF[i] <- sum(pdfall)
      }
      cdf <- FFF
      cdf <- if(lower.tail==TRUE) cdf else 1-cdf
      cdf <- if(log.p==FALSE) cdf else log(cdf)
      cdf
  }
#----------------------------------------------------------------------------------------

#' @rdname logseries
#' @export
qlogseries <- function(p, prob=0.5,  lower.tail = TRUE, log.p = FALSE,
                 max.value = 10000)
  {
          if (any(prob <= 0) | any(prob >= 1) )  stop(paste("prob must be greater than 0 and less than 1", "\n", ""))
          if (any(p < 0) | any(p > 1.0001))  stop(paste("p must be between 0 and 1", "\n", ""))
          if (log.p==TRUE) p <- exp(p) else p <- p
          if (lower.tail==TRUE) p <- p else p <- 1-p
           ly <- length(p)
          QQQ <- rep(0,ly)
          nmu <- rep(prob, length = ly)
       for (i in seq(along=p))
      {
       cumpro <- 0
     if (p[i]+0.000000001 >= 1) QQQ[i] <- Inf
     else
        {
            for (j in seq(from = 1, to = max.value))
            {
            cumpro <-  plogseries(j, prob = nmu[i], log.p = FALSE)
           QQQ[i] <- j
       if  (p[i] <= cumpro ) break
            }
        }
      }
          QQQ
   }
#----------------------------------------------------------------------------------------

#' @rdname logseries
#' @export
rlogseries <- function(n, prob = 0.5)
  {
          if (any(prob <= 0) | any(prob >= 1) )  stop(paste("prob must be greater than 0 and less than 1", "\n", ""))
          if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))
          n <- ceiling(n)
          p <- runif(n)
          r <- qlogseries(p, prob=prob)
          r
  }

