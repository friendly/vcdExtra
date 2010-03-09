\name{modFit}
\Rdversion{1.1}
\alias{modFit}
\alias{modFit.loglm}
\alias{modFit.glm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Brief summary of Model Fit for a glm or loglm object
}
\description{
Formats a brief summary of model fit for a \code{glm} or \code{loglm} object, showing
the likelihood ratio Chisq (df) value and or AIC.  Useful for inclusion in a plot
title or annotation. 
}
\usage{
modFit(x, ...)
\method{modFit}{glm}(x, stats="chisq", digits=2, ...)
\method{modFit}{loglm}(x, stats="chisq", digits=2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A \code{glm} or \code{loglm} object
}
  \item{\dots}{
Arguments passed down
}
  \item{stats}{
One or more of \code{chisq} or \code{aic}, determining the statistics displayed.
}
  \item{digits}{
Number of digits after the decimal point in displayed statistics.
}

}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
	A character string containing the formatted values of the chosen statistics.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{
Michael Friendly
}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
data(Mental)
(Mental.tab <- xtabs(Freq ~ ses+mental, data=Mental))
(Mental.mod <- loglm(~ses+mental, Mental.tab))
Mental.mod
modFit(Mental.mod)

# use to label mosaic()
mosaic(Mental.mod, main=paste("Independence model,", modFit(Mental.mod)))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\keyword{models}% __ONLY ONE__ keyword per line
