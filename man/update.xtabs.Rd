\name{update.xtabs}
\alias{update.xtabs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update method for a \code{xtabs} object
}
\description{
Provides an \code{update} method for \code{"xtabs"} objects,
typically by removing terms from the formula to collapse over them.
}
\usage{
\method{update}{xtabs}(object, formula., ..., evaluate = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An existing \code{"xtabs"} object}
  \item{formula.}{Changes to the formula ? see \code{\link[stats]{update.formula}} for details}

  \item{\dots}{Additional arguments to the call, or arguments with changed values.
  }
  \item{evaluate}{If \code{TRUE}, evaluate the new call else return the call}

}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
If \code{evaluate == TRUE}, the new \code{"xtabs"} object, otherwise the updated call
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

\seealso{
\code{\link[stats]{update.formula}} for details on updates to model formulae

\code{\link[base]{margin.table}} does something similar,
\code{\link{collapse.table}} collapses category levels

}
\examples{
vietnam.tab <- xtabs(Freq ~ sex + year + response, data=Vietnam)

update(vietnam.tab, formula = ~ . -year)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
