\name{loddsratio}
\alias{coef.loddsratio}
\alias{confint.loddsratio}
\alias{dim.loddsratio}
\alias{dimnames.loddsratio}
\alias{loddsratio}
\alias{loddsratio.default}
\alias{print.loddsratio}
\alias{vcov.loddsratio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Log Odds Ratios for Frequency Tables
}
\description{
Computes (log) odds ratios and their asymptotic variance covariance matrix for (possibly) stratified data.
Odds ratios are calculated for the first two array dimensions, separately for each level of
all stratifying dimensions.
This generalizes the \code{\link[vcd]{oddsratio}} methods in the vcd package from
2 x 2 ( x strata) tables to R x C (x strata) tables.
}
\usage{

loddsratio(x, ...)

\method{loddsratio}{default}(x, strata = NULL, ref = NULL, correct = any(x == 0), ...)

\method{coef}{loddsratio}(object, log=TRUE, ...)
\method{confint}{loddsratio}(object, parm, level=0.95, log=TRUE, ...)


\method{print}{loddsratio}(x, log=TRUE, ...)
\method{vcov}{loddsratio}(object, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x}{
A two-way or n-way matrix or array of frequencies, with the first two dimensions
considered the primary ones.
}

  \item{strata}{
For an n-way array, a vector table dimensions considered as strata.
If not specified, dimensions \code{3:length(dim(x))} are taken as strata.
}
  \item{ref}{
Reference categories for the first two table dimensions, a vector or list
of length 1 or 2.
The default, \code{ref=NULL}, gives odds ratios for profile contrasts
for adjacent categories.
See Details below.
}
  \item{correct}{
A logical, indicating whether 0.5 will be added to the table entries,
often recommended to reduce bias when some frequencies are small
(Fleiss, 1981).
The default is \code{TRUE} if \code{any(x == 0)}.
}

  \item{object}{
An object of class \code{loddsratio}
}
  \item{parm}{
A specification of which parameters are to be given confidence intervals, either a vector of numbers or a vector of names. If missing, all parameters are considered.
}

  \item{level}{
the confidence level required for the \code{confint} method.
}

  \item{log}{
If TRUE, return or print log odds ratios, otherwise apply \code{exp()} to
give odds ratios.
}
  \item{\dots}{
Other arguments passed down
}


}
\details{
For an R x C table, (log) odds ratios are formed for the set of (R-1) x (C-1)
2 x 2 tables, corresponding to some set of contrasts among the row and column
variables.  The \code{ref} argument allows these to be specified in a general
way.

\code{ref = NULL} (default) corresponds to "profile contrasts" comparing 
adjacent categories for ordered
variables, i.e., R1-R2, R2-R3, R3-R4, etc., and similarly for the column
categories.

\code{ref=1} gives contrasts with the first category; \code{ref=dim(x)}
gives contrasts with the last category; \code{ref = c(2, 4)} or \code{ref = list(2, 4)}
corresponds to the reference being the second category in rows and
the fourth in columns.

Combinations like \code{ref = list(NULL, 3)} are also possible, as are character
vectors, e.g., \code{ref = c("foo", "bar")} also works ("foo" pertaining again to the
    row reference and "bar" to column reference).

Note that all such parameterizations are equivalent, in that one can derive all
other possible odds ratios from any non-redundant set. 
}
\value{
An object of class \code{loddsratio}, with the following components: 
%%  If it is a LIST, use
\item{coefficients}{A named vector, of length (R-1) x (C-1) x \code{prod(dim(x)[strata])}
	containing the log odds ratios. Use the \code{coef} method to 
	extract these from the object, and the \code{confint} method for confidence intervals.
	For a two-way table, the names for the log odds ratios are constructed in the form
	Ri:Rj/Ci:Cj using the table names for rows and columns. For a stratified table,
	the names are constructed in the form Ri:Rj/Ci:Cj|Lk.
	}
\item{dnames}{Dimension names for the log odds ratios, considered as a table of
	size \code{c(R-1, C-1, dim(x)[strata]}. Use the \code{dim} and \code{dimnames} methods
	to extract these and manipulate the log odds ratios in relation to the original table.}

\item{vcov}{Variance covariance matrix of the log odds ratios.}

\item{contrasts}{A matrix C, such that \code{C \%*\% as.vector(log(x))} gives the log odds
	ratios.  Each row corresponds to one log odds ratio, and is all zero, except for 4 elements
of \code{c(1, -1, -1, 1)} for a given 2 x 2 subtable.}

%% ...
}

\references{
A. Agresti (1990), \emph{Categorical Data Analysis}. New York: Wiley.

Fleiss, J. L. (1981). \emph{Statistical Methods for Rates and Proportions}.
2nd Edition. New York: Wiley.

M. Friendly (2000), \emph{Visualizing Categorical Data}. SAS Institute, Cary, NC.
}
\author{
Achim Zeileis and Michael Friendly
}
\note{
The method of calculation is an example of the use of the delta method
described by Agresti (1990), Section 12.1.7, giving estimates of
log odds ratios and their asymptotic covariance matrix.

The code{coef} method returns the \code{coefficients} component as a vector
of length (R-1) x (C-1) x \code{prod(dim(x)[strata])}.
The code{dim} and \code{dimnames} methods provide the proper attributes for
treating the \code{coefficients} vector as an (R-1) x (C-1) x strata array.

The \code{confint} method computes confidence intervals for the log odds ratios
(or for odds ratios, with \code{log=FALSE}).
The \code{coeftest} method prints the asympotic standard errors,
 z tests (standardized log odds ratios), and the corresponding p values.   
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[vcd]{oddsratio}}, \code{\link[stats]{confint}},

\code{\link[lmtest]{coeftest}}


}
\examples{
## artificial example
set.seed(1)
x <- matrix(rpois(5 * 3, 7), ncol = 5, nrow = 3)
dimnames(x) <- list(Row=head(letters, 3), Col=tail(letters, 5))

x_lor <- loddsratio(x)
coef(x_lor)
x_lor
confint(x_lor)
if(require("lmtest")) coeftest(x_lor)


## 2 x 2 x k cases
#data(CoalMiners, package="vcd")
lor.CM <- loddsratio(CoalMiners)
lor.CM
coef(lor.CM)
confint(lor.CM)
confint(lor.CM, log=FALSE)


## 2 x k x 2
lor.Emp <-loddsratio(Employment)
lor.Emp
confint(lor.Emp)

## R x C case
#data(Hauser79, package="vcdExtra")
hauser.tab <- xtabs(Freq ~ Father+Son, data=Hauser79)
(lor.hauser <- loddsratio(hauser.tab))
confint(lor.hauser)

## 4 way tables 
#data(Punishment, package="vcd")
punish.tab <- xtabs(Freq ~ memory + attitude + age + education, data = Punishment)
lor.pun <- loddsratio(punish.tab)
lor.pun
confint(lor.pun)
if(require("lmtest")) coeftest(lor.pun)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{category}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
