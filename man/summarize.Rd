\name{summarize}
\Rdversion{1.1}
\alias{summarize}
\alias{summarize.glm}
\alias{summarize.glmlist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Brief Summary of Model Fit for glm Models
}
\description{
For \code{glm} objects, the \code{print} and \code{summary} methods give
too much information if all one wants to see is a brief summary of model
goodness of fit, and there is no easy way to display a compact comparison of
model goodness of fit for a collection of models fit to the same data.

\code{summarize} provides a brief summary for one or more \code{glm} models.
This implementation is experimental, and is subject to change.
}
\usage{

summarize(object, ...)

\method{summarize}{glm}(object, ..., dispersion = NULL, test = NULL)

\method{summarize}{glmlist}(object, ..., dispersion = NULL, test = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{object, \dots}{objects of class \code{glm}, 
  	typically the result of a call to \code{glm}, or a list of objects for the "glmlist" method.
}
%  \item{\dots}{
%%%     ~~Describe \code{\dots} here~~
%}

  \item{dispersion}{
the dispersion parameter for the fitting family. 
Not used in the current implementation.  
}
  \item{test}{
a character string, (partially) matching one of "Chisq", "F" or "Cp".
Not used in the current implementation.
}

}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A data frame with columns \code{c("LR Chisq", "Df", "Pr(>Chisq)", "AIC")}.
Row names are taken from the names of the model object(s).
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

\seealso{
\code{\link{glmlist}}
}
\examples{
data(Mental)
indep <- glm(Freq ~ mental+ses,
                family = poisson, data = Mental)
summarize(indep)
Cscore <- as.numeric(Mental$ses)
Rscore <- as.numeric(Mental$mental)

coleff <- glm(Freq ~ mental + ses + Rscore:ses,
                family = poisson, data = Mental)
roweff <- glm(Freq ~ mental + ses + mental:Cscore,
                family = poisson, data = Mental)
linlin <- glm(Freq ~ mental + ses + Rscore:Cscore,
                family = poisson, data = Mental)
                
# make a glmlist
mods <- glmlist(indep, coleff, roweff, linlin)
summarize(mods)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
