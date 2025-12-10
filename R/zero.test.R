# Score test for zero inflation in Poisson data
#https://stats.stackexchange.com/questions/118322/how-to-test-for-zero-inflation-in-a-dataset

# References:
# Broek, Jan van den. 1995. ?A Score Test for Zero Inflation in a Poisson Distribution.? Biometrics 51 (2): 738?43. doi:10.2307/2532959.
# Yang, Zhao, James W. Hardin, and Cheryl L. Addy. 2010. ?Score Tests for Zero-Inflation in Overdispersed Count Data.? Communications in Statistics - Theory and Methods 39 (11): 2008?30. doi:10.1080/03610920902948228

# Van den Broek, J. (1995). A Score Test for Zero Inflation in a Poisson Distribution. Biometrics, 51(2), 738-743. doi:10.2307/2532959





#' Score test for zero inflation in Poisson data
#'
#' Carries out a simple score test (van den Broek, 1995) for excess zeros in an
#' otherwise Poisson distribution of counts.  It gives a \eqn{\chi^2_1}
#' statistic on one degree of freedom.
#'
#' The test first calculates the rate estimate from the mean,
#' \eqn{\hat{\lambda} = \bar{x}}. The number of observed zeros, \eqn{n_0} is
#' then compared with the expected number, \eqn{n \hat{p_0}}, where
#' \eqn{\hat{p}_0=\exp[-\hat{\lambda}]}. Then the test statistic is calculated
#' by the formula:
#' \deqn{\frac{(n_0 - n\hat{p}_0)^2}{n\hat{p}_0(1-\hat{p}_0) - n\bar{x}\hat{p}_0^2}} .
#' This test statistic has a \eqn{\chi^2_1} distribution.
#'
#' @param x A vector of non-negative counts, or a one-way frequency table of
#' such counts.
#'
#' @return Returns invisibly a list of three elements:
#' \item{`statistic`}{Value of the test statistic}
#' \item{`df`}{Degrees of freedom}
#' \item{`pvalue`}{Upper tail p-value}
#'
#' @author Michael Friendly
#'
#' @references The original R code came from a Stackexchange question,
#' <https://stats.stackexchange.com/questions/118322/how-to-test-for-zero-inflation-in-a-dataset>
#'
#' Van den Broek, J. (1995).  A Score Test for Zero Inflation in a Poisson
#' Distribution.  *Biometrics*, **51**(2), 738-743.
#' https://www.jstor.org/stable/2532959
#'
#' Yang, Zhao, James W. Hardin, and Cheryl L. Addy (2010).  Score Tests for
#' Zero-Inflation in Overdispersed Count Data.  *Communications in
#' Statistics - Theory and Methods* **39** (11) 2008-2030.
#' DOI: 10.1080/03610920902948228
#'
#' @family association tests
#'
#' @keywords htest
#' @export
#' @examples
#'
#' # synthetic tests
#' zero.test(rpois(100, 1))
#' zero.test(rpois(100, 5))
#' # add some extra zeros
#' zero.test(c(rep(0, 20), rpois(100, 5)))
#'
#' # Articles by Phd candidates
#' data(PhdPubs, package="vcdExtra")
#' zero.test(PhdPubs$articles)
#'
#' phd.tab <- table(PhdPubs$articles)
#' zero.test(phd.tab)
#'
#'
zero.test <- function(x) {

    if(is.table(x)) { # expand to vector of values
        if(length(dim(x)) > 1) stop ("x must be a 1-way table")
        x <- rep(as.numeric(names(x)), unname(c(x)))
    }
		lambda <- mean(x)
		p0_tilde <- exp(-lambda)
		n0 <- sum(1*(!(x >0)))
		n <- length(x)
		numerator <- (n0 - n*p0_tilde)^2
		denominator <- n*p0_tilde*(1-p0_tilde) - n*lambda*(p0_tilde^2)
		stat <- numerator/denominator
		pvalue <- pchisq(stat,df=1, ncp=0, lower.tail=FALSE)
		result <- list(statistic=stat, df=1, prob=pvalue)
		cat(paste("Score test for zero inflation\n\n",
		          "\tChi-square =", round(stat,5), "\n",
		          "\tdf = 1\n",
		          "\tpvalue:", format.pval(pvalue), "\n"))
		invisible(result)
}
