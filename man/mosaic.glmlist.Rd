\name{mosaic.glmlist}
\alias{mosaic.glmlist}
\alias{mosaic.loglmlist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mosaic Displays for \code{glmlist} and \code{logllmlist} Objects
}
\description{
This function provides a convenient interface for viewing mosaic displays
associated with a collection of glm models for freqency tables that have
been stored in a \code{glmlist} or \code{loglmlist} object.  You can plot either selected models
individually, or mosaics for all models in an array of viewports.


}
\usage{
\method{mosaic}{glmlist}(x, selection, 
  panel=mosaic,
  type=c("observed", "expected"), 
  legend=ask | !missing(selection),
  main=NULL,
  ask=TRUE, graphics=TRUE, rows, cols, newpage=TRUE,
  ...)

\method{mosaic}{loglmlist}(x, selection, 
  panel=mosaic,
  type=c("observed", "expected"), 
  legend=ask | !missing(selection),
  main=NULL,
  ask=TRUE, graphics=TRUE, rows, cols, newpage=TRUE,
  ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a \code{glmlist} or \code{loglmlist} object
}
  \item{selection}{
the index or name of one \code{glm} or \code{loglm} object in \code{x}.  If no selection is specified,
a menu of models is presented or all models are plotted.
}

  \item{panel}{
a \code{\link[vcd]{strucplot}} panel function, typicially \code{\link[vcd]{mosaic}} or \code{\link[vcd]{sieve}}
}
  \item{type}{
a character string indicating whether the \code{"observed"} or the \code{"expected"} values of the table should be visualized
}
  \item{legend}{
logical: show a legend for residuals in the mosaic display(s)?  The default behavior is to include a legend
when only a single plot is shown, i.e., if \code{ask} is \code{TRUE} or a \code{selection} has been
specified.
}
  \item{main}{
either a logical, or a vector of character strings used for plotting the main title. 
If main is a logical and \code{TRUE}, the name of the selected glm object  is used.

}
  \item{ask}{
logical: should the function display a menu of models, when one is not specified in \code{selection}?
If \code{selection} is not supplied and \code{ask} is \code{TRUE} (the default), 
a menu of model names is presented; 
if \code{ask} is \code{FALSE}, mosaics for all models are plotted in an array.

}
  \item{graphics}{
logical: use a graphic dialog box when \code{ask=TRUE}?
}
  \item{rows,cols}{
when \code{ask=FALSE}, the number of rows and columns in which to plot the mosaics.
}
  \item{newpage}{
start a new page? (only applies to \code{ask=FALSE})
}
  \item{\dots}{
other arguments passed to \code{\link{mosaic.glm}} and ultimately to \code{\link[vcd]{mosaic}}. 
}
}
\details{
Most details of the plots produced can be controlled via \dots arguments
as shown in some of the examples below.  In particular, with \code{panel=sieve}
you need to also pass \code{gp=shading_Friendly} to get a color version.
}
\value{
Returns the result of \code{\link{mosaic.glm}}.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
 David Meyer, Achim Zeileis, and Kurt Hornik (2006). The Strucplot Framework: Visualizing Multi-Way
  Contingency Tables with vcd. \emph{Journal of Statistical Software}, 17(3), 1-48. 
  \url{http://www.jstatsoft.org/v17/i03/}, available as \code{vignette("strucplot", package="vcd")}.

}
\author{
Michael Friendly
}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{glmlist}}, \code{\link{loglmlist}},
\code{\link{Kway}}

\code{\link{mosaic.glm}}, \code{\link[vcd]{mosaic}},
\code{\link[vcd]{strucplot}}, for the many parameters that control the details of mosaic plots.


}
\examples{
data(JobSatisfaction, package="vcd")

# view all pairwise mosaics
pairs(xtabs(Freq~management+supervisor+own, data=JobSatisfaction), 
    shade=TRUE, diag_panel=pairs_diagonal_mosaic)

modSat <- Kway(Freq ~ management+supervisor+own, data=JobSatisfaction, 
               family=poisson, prefix="JobSat")
names(modSat)

\dontrun{
mosaic(modSat)              # uses menu, if interactive()
}
mosaic(modSat, "JobSat.1")  # model label
mosaic(modSat, 2)           # model index

# supply a formula to determine the order of variables in the mosaic
mosaic(modSat, 2, formula=~own+supervisor+management)

mosaic(modSat, ask=FALSE)   # uses viewports 

# use a different panel function, label the observed valued in the cells
mosaic(modSat, 1, main=TRUE, panel=sieve, gp=shading_Friendly, labeling=labeling_values)

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

# assign names for the plot labels
modMental <- glmlist(Indep=indep, ColEff=coleff, RowEff=roweff, `Lin x Lin`=linlin)
mosaic(modMental, ask=FALSE, margins=c(3,1,1,2), labeling_args=list(abbreviate_labs=5))


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
