\name{Abortion}
\Rdversion{1.1}
\alias{Abortion}
\docType{data}
\title{Abortion Opinion Data}
\description{Opinions about abortion classified by gender and SES}

\usage{
data(Abortion)
}
\format{
  A 3-dimensional array resulting from cross-tabulating 3 variables for  1100 observations. The variable names and their levels are:

  \tabular{rll}{
    No \tab Name                    \tab Levels \cr
    1  \tab \code{Sex}              \tab \code{"Female", "Male"}\cr
    2  \tab \code{Status}           \tab \code{"Lo", "Hi"}\cr
    3  \tab \code{Support_Abortion} \tab \code{"Yes", "No"}\cr
  }
}

\details{
\code{Support_Abortion} is a natural response variable.

The combinations of \code{Sex} and \code{Status} represent four independent samples, having
fixed \code{Sex}-\code{Status} marginal totals.
There were 500 females and 600 males. Within the female group, 250 of low status and 250
of high status were sampled. Similarly for the males, with 300 in each of the low and high
status sub-groups.

This is an example of a product-multinomial sampling scheme.
the \code{Sex:Status} association must be included in any loglinear model
where the goal is to determine how attitude toward abortion depends on the others.

Alternatively, a logit model for abortion support may provide a simpler analysis.

}
\source{

%  \cite{Christensen:90 [p. 92]}
Christensen, R. (1990).
 \emph{Log-Linear Models},
 New York, NY: Springer-Verlag, p. 92, Example 3.5.2.

Christensen, R. (1997).
 \emph{Log-Linear Models and Logistic Regression},
 New York, NY: Springer, p. 100, Example 3.5.2.

}

%\references{
%}

%\seealso { }
\examples{
data(Abortion)


ftable(Abortion)
mosaic(Abortion, shade=TRUE)

# stratified by Sex
fourfold(aperm(Abortion, 3:1))
# stratified by Status
fourfold(aperm(Abortion, c(3,1,2)))

}

\keyword{datasets}
\concept{loglinear models}
\concept{logit models}
\concept{2 by 2 tables}
