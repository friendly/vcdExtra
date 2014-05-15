\name{blogits}
\alias{blogits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bivariate Logits and Log Odds Ratio
}
\description{
This function calculates the log odds and log odds ratio for
two binary responses classified by one or more stratifying
variables.

It is useful for plotting the results of bivariate logistic regression
models, such as those fit using \code{\link[VGAM]{vglm}} in the
\pkg{VGAM}.
}
\usage{
blogits(Y, add, colnames, row.vars, rev=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
A four-column matrix or data frame whose columns correspond to the 2 x 2
combinations of two binary responses. 
}
  \item{add}{
Constant added to all cells to allow for zero frequencies.  The default
is 0.5 if \code{any(Y)==0} and 0 otherwise.
}
  \item{colnames}{
Names for the columns of the results. The default is 
\code{c("logit1", "logit2", "logOR")}.  If less than three
names are supplied, the remaining ones are filled in from the default.
}
  \item{row.vars}{
A data frame or matrix giving the factor levels of one or more factors
corresponding to the rows of \code{Y} 
}
  \item{rev}{A logical, indicating whether the order of the columns in \code{Y}
  should be reversed.}
}

\details{
For two binary variables with levels 0,1 the logits are calculated 
assuming the columns in \code{Y} are given in the order
11, 10, 01, 00, so the logits give the log odds of the 1 response
compared to 0.  
If this is not the case, either use \code{rev=TRUE} or supply
\code{Y[,4:1]} as the first argument.
}

\value{
A data frame with \code{nrow(Y)} rows and \code{3 + ncol(row.vars)} columns
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
\code{\link[VGAM]{vglm}}
}
\examples{
data(Toxaemia)
tox.tab <- xtabs(Freq~class + smoke + hyper + urea, Toxaemia)

# reshape to 4-column matrix
toxaemia <- t(matrix(aperm(tox.tab), 4, 15))
colnames(toxaemia) <- c("hu", "hU", "Hu", "HU")
rowlabs <- expand.grid(smoke=c("0", "1-19", "20+"), class=factor(1:5))
toxaemia <- cbind(toxaemia, rowlabs)

# logits for H and U
logitsTox <- blogits(toxaemia[,4:1], add=0.5, colnames=c("logitH", "logitW"), row.vars=rowlabs)
logitsTox

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
