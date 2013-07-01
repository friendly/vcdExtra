\name{mosaic.glmlist}
\alias{mosaic.glmlist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mosaic Displays for a glmlist Object
}
\description{
This function provides a convenient interface for viewing mosaic displays
associated with a collection of glm models for freqency tables that have
been stored in a glmlist object.


}
\usage{
\method{mosaic}{glmlist}(x, selection, 
  type = c("observed", "expected"), 
  legend = FALSE, main = NULL, 
  ask = TRUE, graphics = TRUE, rows, cols, newpage = TRUE,
  ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a \code{glmlist} object
}
  \item{selection}{
the index or name of one glm in \code{x}
}
  \item{type}{
a character string indicating whether the \code{"observed"} or the \code{"expected"} values of the table should be visualized
}
  \item{legend}{
show a legend in the mosaic displays?
}
  \item{main}{
either a logical, or a vector of character strings used for plotting the main title. 
If main is a logical and \code{TRUE}, the name of the selected glm object  is used

}
  \item{ask}{
logical: should the function display a menu of models, when one is not specified in \code{selection}?

}
  \item{graphics}{
logical: use a graphic menu when \code{ask=TRUE}?
}
  \item{rows,cols}{
when \code{ask=FALSE}, the number of rows and columns in which to plot the mosaics.
}
  \item{newpage}{
start a new page? (only applies to \code{ask=FALSE})
}
  \item{\dots}{
other arguments passed to \code{\link{mosaic.glm}}. 
}
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns the result of \code{\link{mosaic.glm}}.
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

\seealso{
\code{\link{glmlist}},
\code{\link{Kway}},

}
\examples{
data(JobSatisfaction, package="vcd")

# view all pairwise mosaics
pairs(xtabs(Freq~management+supervisor+own, data=JobSatisfaction), 
    shade=TRUE, diag_panel=pairs_diagonal_mosaic)

modSat <- Kway(Freq ~ management+supervisor+own, data=JobSatisfaction, 
               family=poisson, prefix="JobSat")
\dontrun{
mosaic(modSat)              # uses menu, if interactive()
}
mosaic(modSat, "JobSat.1")  # model label
mosaic(modSat, 2)           # model index

mosaic(modSat, ask=FALSE)   # uses viewports 

# use a different panel function
mosaic(modSat, 1, main=TRUE, panel=sieve)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
