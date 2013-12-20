\name{logseries}
\alias{Logseries}
\alias{dlogseries}
\alias{plogseries}
\alias{qlogseries}
\alias{rlogseries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	The Logarithmic Series Distribution
}
\description{
The logarithmic series distribution is a long-tailed distribution
introduced by
Fisher etal. (1943)
in connection with data on the abundance of individuals
classified by species. 

These functions provide the density, distribution function, quantile function and 
random generation for the 
logarithmic series distribution with parameter \code{prob}. 
}
\usage{

dlogseries(x, prob = 0.5, log = FALSE)

plogseries(q, prob = 0.5, lower.tail = TRUE, log.p = FALSE)

qlogseries(p, prob = 0.5, lower.tail = TRUE, log.p = FALSE, max.value = 10000)

rlogseries(n, prob = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x, q}{
vector of quantiles representing the number of events.
}
  \item{prob}{
parameter for the distribution, \code{0 < prob < 1}
}
  \item{log, log.p}{
ogical; if TRUE, probabilities \code{p} are given as \code{log(p)}
}

  \item{lower.tail}{
logical; if TRUE (default), probabilities are \eqn{P[X \le x]}{P[X <= x]}, 
otherwise, \eqn{P[X > x]}{P[X > x]}.
}

  \item{p}{
vector of probabilities
}
  \item{max.value}{
maximum value returned by \code{qlogseries}
}

  \item{n}{
number of observations for \code{rlogseries}
}
}
\details{
The logarithmic series distribution with \code{prob} = \eqn{p} has density
\deqn{
	p ( x )  = \alpha p^x / x
}
for \eqn{x = 1, 2, \dots}, where \eqn{\alpha= -1 / \log(1 - p)}
and \eqn{0 < p <1}.  Note that counts \code{x==2} cannot occur.
}
\value{
\code{dlogseries} gives the density, 
\code{plogseries} gives the distribution function, 
\code{qlogseries} gives the quantile function, and 
\code{rlogseries} generates random deviates.


%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
\url{http://en.wikipedia.org/wiki/Logarithmic_distribution}

Fisher, R. A. and Corbet, A. S. and Williams, C. B. (1943).
The relation between the number of species and the number of individuals 
\emph{Journal of Animal Ecology}, 12, 42-58.
}
\author{
Michael Friendly, using
original code modified from the \code{gmlss.dist} package
by Mikis Stasinopoulos.
}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[stats]{Distributions}}, ~~~
}
\examples{
XL <-expand.grid(x=1:5, p=c(0.33, 0.66, 0.99))
lgs.df <- data.frame(XL, prob=dlogseries(XL[,"x"], XL[,"p"]))
lgs.df$p = factor(lgs.df$p)
str(lgs.df)

require(lattice)
mycol <- palette()[2:4]
xyplot( prob ~ x, data=lgs.df, groups=p,
	xlab=list('Number of events (k)', cex=1.25),
	ylab=list('Probability',  cex=1.25),
	type='b', pch=15:17, lwd=2, cex=1.25, col=mycol,
	key = list(
					title = 'p',
					points = list(pch=15:17, col=mycol, cex=1.25),
					lines = list(lwd=2, col=mycol),
					text = list(levels(lgs.df$p)),
					x=0.9, y=0.98, corner=c(x=1, y=1)
					)
	)


# random numbers
hist(rlogseries(200, prob=.4), xlab='x')
hist(rlogseries(200, prob=.8), xlab='x')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
