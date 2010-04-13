\name{glmlist}
\Rdversion{1.1}
\alias{glmlist}
\alias{loglmlist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a Model List Object
}
\description{
\code{glmlist} creates a \code{glmlist} object containing a list of fitted \code{glm} objects with their names.
\code{loglmlist} does the same for \code{loglm} objects.

The intention is to provide object classes to facilitate model comparison,
extraction, summary and plotting of model components, etc., perhaps using
\code{\link[base]{lapply}} or similar.
}
\usage{
glmlist(...)
loglmlist(...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
One or more model objects, as appropriate to the function,
optionally assigned names as in \code{list}.
}
}
\details{
The arguments to \code{glmlist}  or \code{loglmlist} are of the form \code{value} or \code{name=value}.

Any objects which do not inherit the appropriate class \code{glm} or \code{loglm} are excluded, with a warning.
}
\value{
An object of class \code{glmlist} \code{loglmlist}, just like a \code{list},
except that each is given a \code{name} attribute.
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
The function \code{\link[Hmisc]{llist}} in package \code{Hmisc} is similar, but perplexingly
more general.

The function \code{\link[stats]{anova.glm}} also handles \code{glmlist objects}


}
\examples{
data(Mental)
indep <- glm(Freq ~ mental+ses,
                family = poisson, data = Mental)
Cscore <- as.numeric(Mental$ses)
Rscore <- as.numeric(Mental$mental)

coleff <- glm(Freq ~ mental + ses + Rscore:ses,
                family = poisson, data = Mental)
roweff <- glm(Freq ~ mental + ses + mental:Cscore,
                family = poisson, data = Mental)
linlin <- glm(Freq ~ mental + ses + Rscore:Cscore,
                family = poisson, data = Mental)
                
# use object names
mods <- glmlist(indep, coleff, roweff, linlin)
names(mods)

# assign new names
mods <- glmlist(Indep=indep, Col=coleff, Row=roweff, LinxLin=linlin)
names(mods)

summarize(mods)

#extract model components
unlist(lapply(mods, deviance))

res <- lapply(mods, residuals)
boxplot(as.data.frame(res), main="Residuals from various models")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\keyword{models}% __ONLY ONE__ keyword per line
