\name{CMHtest}
\alias{CMHtest}
\alias{Cochran Mantel Haenszel test}
\alias{print.CMHtest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generalized Cochran-Mantel-Haenszel Tests
}
\description{
Provides generalized Cochran-Mantel-Haenszel tests of association of two possibly ordered factors, 
optionally stratified other factor(s).  With strata, \code{CMHtest} calculates these tests for
each level of the statifying variables and also provides overall tests controlling for the
strata.

For ordinal factors, more powerful tests than the test for general association
(independence) are obtained by assigning scores to the row and columm categories. 
}
\usage{

CMHtest(x, strata = NULL, 
   rscores = 1:R, cscores = 1:C, 
   types = c("cor", "cmeans", "rmeans", "general"), 
   overall=FALSE, details=overall, ...)

\method{print}{CMHtest}(x, digits = max(getOption("digits") - 2, 3), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x}{
A 2+ way contingency table as a class \code{"table"} object.
}
  \item{strata}{
For a 3- or higher-way table, the names or numbers of the factors
to be treated as strata.  By default, the first 2 factors are treated
as the main table variables, and all others considered stratifying factors.
}
  \item{rscores}{
Row scores.  Either a set of numbers (typically integers, \code{1:R})
or the string \code{"midrank"} for standardized midrank scores,
or \code{NULL} to exclude tests that depend on row scores.
}
  \item{cscores}{
Column scores. Same as for row scores.
}
  \item{types}{
Types of CMH tests to compute: Any one or more of 
\code{c("cor", "cmeans", "rmeans", "general")},
or \code{"ALL"} for all of these.
}
  \item{overall}{
logical. Whether to calculate overall tests, controlling for
the stratifying factors. 
}
  \item{details}{
logical.  Whether to include computational details in the result
}
  \item{\dots}{
Other arguments
}

  \item{digits}{
Digits to print.
}
}
\details{
The standard \eqn{\chi^2} tests for association in a two-way table
treat both table factors as nominal (unordered) categories.
When one or both factors of a two-way table are
quantitative or ordinal, more powerful tests of association
may be obtained by taking ordinality into account using
row and or column scores to test for linear trends or differences
in row or column means.

The CMH analysis for a two-way table produces generalized Cochran-Mantel-Haenszel statistics
(Landis etal., 1978).

These include the CMH \bold{correlation} statistic (\code{"cor"}),
treating both factors as ordered.
For a given statum, with equally spaced row and column scores,
this CMH statistic reduces to \eqn{(n-1) r^2},
where \eqn{r} is the Pearson correlation between X and Y.
With \code{"midrank"} scores, this CMH statistic is analogous
to \eqn{(n-1) r_S^2}, using the Spearman rank correlation.
 
The \bold{ANOVA} (row mean scores and column mean scores) statistics,
treat the columns and rows respectively as ordinal,
and are sensitive to mean shifts over columns or rows.
These are transforms of the \eqn{F} statistics from one-way ANOVAs
with equally spaced scores and to Kruskal-Wallis tests with
\code{"midrank"} scores. 

The CMH \bold{general} association statistic treat both factors as unordered,
and give a test closely related to the Pearson \eqn{\chi^2} test.
When there is more than one stratum, the overall general CMH statistic
gives a stratum-adjusted Pearson \eqn{\chi^2},
equivalent to what is calculated by \code{\link[stats]{mantelhaen.test}}.

For a 3+ way table, one table of CMH tests is produced for each 
combination of the factors identified as \code{strata}.
If \code{overall=TRUE}, an additional table is calculated for
the same two primary variables, controlling for (pooling over)
the \code{strata} variables.
These tests implicitly assume no interactions  between the 
primary variables and the strata and will have low power in the
presence of interactions.
}

\value{
An object of class \code{"CMHtest"} , a list with the following 4 components:
  \item{table}{A matrix containing the test statistics, with columns
  	\code{Chisq}, \code{Df} and \code{Prob} }
  \item{names}{The names of the table row and column variables}
  \item{rscore}{Row scores}
  \item{cscore}{Column scores}

If \code{details==TRUE}, additional components are included.

If there are strata, the result is a list of \code{"CMHtest"} objects.
If \code{overall=TRUE} another component, labeled \code{ALL}
is appended to the list.
}


\references{
Stokes, M. E. & Davis, C. S.  & Koch, G., (2000). 
 \emph{Categorical Data Analysis using the SAS System}, 2nd Ed.,
 Cary, NC: SAS Institute,
 pp 74-75, 92-101, 124-129.
 Details of the computation are given at:
 \url{http://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_freq_a0000000648.htm
}
 
Landis, R. J., Heyman, E. R., and Koch, G. G. (1978). 
 Average Partial Association in Three-way Contingency Tables: 
 A Review and Discussion of Alternative Tests, 
 \emph{International Statistical Review}, \bold{46}, 237-254.
}
\author{
Michael Friendly
}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[coin]{cmh_test}} provides the CMH test of general association;
\code{\link[coin]{lbl_test}} provides the CMH correlation test of linear by linear association.

\code{\link[stats]{mantelhaen.test}} provides the overall general
Cochran-Mantel-Haenszel chi-squared test of the null that two nominal variables are conditionally independent 
in each stratum, assuming that there is no three-way interaction

}
\examples{
data(JobSat, package="vcdExtra")
CMHtest(JobSat)
CMHtest(JobSat, rscores="midrank", cscores="midrank")

# A 3-way table (both factors ordinal)
data(MSPatients, package="vcd")
CMHtest(MSPatients)

# also calculate overall tests, controlling for Patient
CMHtest(MSPatients, overall=TRUE)
# compare with mantelhaen.test
mantelhaen.test(MSPatients)

# using a frequency data.frame
CMHtest(xtabs(Freq~ses+mental, data=Mental))

# Stokes etal, Table 5.1, p 92: two unordered factors
parties <- matrix(
	c(221, 160, 360, 140,
	  200, 291, 160, 311,
	  208, 106, 316, 97), nrow=3, ncol=4, byrow=TRUE)
dimnames(parties) <- list(party=c("Dem", "Indep", "Rep"),
						neighborhood=c("Bayside", "Highland", "Longview", "Sheffield"))
CMHtest(parties, rscores=NULL, cscores=NULL)
# compare with Pearson chisquare
chisq.test(parties)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
