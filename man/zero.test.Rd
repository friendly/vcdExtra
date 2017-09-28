\name{zero.test}
\alias{zero.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Score test for zero inflation in Poisson data
}
\description{
Carries out a simple score test (van den Broek, 1995) for excess zeros in 
an otherwise Poisson distribution of counts.  It gives a \eqn{\chi^2_1}
statistic on one degree of freedom.
}
\usage{
zero.test(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A vector of non-negative counts, or a one-way frequency table
of such counts.
}
}
\details{
The test first calculates the rate estimate from the mean,
\eqn{\hat{\lambda} = \bar{x}}.
The number of observed zeros, \eqn{n_0} is then compared with the expected
number, \eqn{n \hat{p_0}}, where \eqn{\hat{p}_0=\exp[-\hat{\lambda}]}.
Then the test statistic is calculated by the formula: 
\deqn{\frac{(n_0 - n\hat{p}_0)^2}{n\hat{p}_0(1-\hat{p}_0) - n\bar{x}\hat{p}_0^2}} 
This test statistic
has a \eqn{\chi^2_1} distribution.

}
\value{
Returns invisibly a list of three elements:
	\item{\code{statistic}}{Description of 'comp1'}
	\item{\code{df}}{Description of 'comp2'}
	\item{\code{pvalue}}{Upper tail p-value}
}
\references{
The original R code came from a Stackexchange question,
\url{https://stats.stackexchange.com/questions/118322/how-to-test-for-zero-inflation-in-a-dataset}

Van den Broek, J. (1995). 
A Score Test for Zero Inflation in a Poisson Distribution. 
\emph{Biometrics}, \bold{51}(2), 738-743. \code{doi:10.2307/2532959}

Yang, Zhao, James W. Hardin, and Cheryl L. Addy (2010). 
Score Tests for Zero-Inflation in Overdispersed Count Data. 
\emph{Communications in Statistics - Theory and Methods} \bold{39} (11) 2008-2030. 
\code{doi:10.1080/03610920902948228}

}
\author{
Michael Friendly
}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
# synthetic tests
zero.test(rpois(100, 1))
zero.test(rpois(100, 5))
# add some extra zeros
zero.test(c(rep(0, 20), rpois(100, 5)))

# Articles by Phd candidates
data(PhdPubs, package="vcdExtra")
zero.test(PhdPubs$articles)

phd.tab <- table(PhdPubs$articles)
zero.test(phd.tab)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
