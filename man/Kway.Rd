\name{Kway}
\Rdversion{1.1}
\alias{Kway}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit All K-way Models in a GLM
}
\description{
Generate and fit all 0-way, 1-way, 2-way, ... k-way terms in a glm.

This function is designed mainly for hierarchical
loglinear models (or \code{glm}s
in the poission family), where it is desired to find the 
highest-order terms necessary to achieve a satisfactory fit.

Using \code{\link[stats]{anova}} on the resulting \code{\link{glmlist}}
object will then give sequential tests of the pooled contributions of
all terms of degree \eqn{k+1} over and above those of degree \eqn{k}. 

This function is also intended as an example of a generating function
for \code{\link{glmlist}} objects, to facilitate model comparison, extraction, 
summary and plotting of model components, etc., perhaps using \code{lapply} or similar. 

}
\usage{
Kway(formula, family=poisson, data, ..., order = nt, prefix = "kway")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
a two-sided formula for the 1-way effects in the model.
The LHS should be the response, and the RHS should be the first-order terms
connected by \code{+} signs.
}
  \item{family}{
a description  of the  error distribution  and link  function to  be used in the
model.  This  can be  a  character string  naming  a family  function,  a family
function or the result of a call to a family function. 
(See \code{\link[stats]{family}} for details of family functions.)
}
  \item{data}{
an  optional  data   frame,  list  or   environment  (or  object   coercible  by
\code{\link[base]{as.data.frame}} to a  data frame) containing  the variables in  the model. If  not
found in data, the variables are taken from \code{environment(formula)}, typically  the
environment from which \code{glm} is called.
}
  \item{\dots}{
Other arguments passed to \code{glm}
}
  \item{order}{
Highest order interaction of the models generated. Defaults to the number
of terms in the model formula.
}
  \item{prefix}{
Prefix used to label the models fit in the \code{glmlist} object.
}
}
\details{
With \code{y} as the response in the \code{formula}, the 0-way (null) model
is \code{y ~ 1}.  
The 1-way ("main effects") model is that specified in the
\code{formula} argument.  The k-way model is generated using the formula
\code{. ~ .^k}.
With the default \code{order = nt}, the final model is the saturated model.

As presently written, the function requires a two-sided formula with an explicit
response on the LHS. For frequency data in table form (e.g., produced by \code{xtabs})
you the \code{data} argument is coerced to a data.frame, so you
should  supply the \code{formula} in the form \code{Freq ~ } \dots.
}
\value{
An object of class \code{glmlist}, of length \code{order+1}
containing the 0-way, 1-way, ... 
models up to degree \code{order}.
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{
Michael Friendly and Heather Turner
}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{glmlist}}, 
\code{\link{summarise}}
}
\examples{
## artificial data
factors <- expand.grid(A=factor(1:3), B=factor(1:2), C=factor(1:3), D=factor(1:2))
Freq <- rpois(nrow(factors), lambda=40)
df <- cbind(factors, Freq)

mods3 <- Kway(Freq ~ A + B + C, data=df, family=poisson)
summarise(mods3)
mods4 <- Kway(Freq ~ A + B + C + D, data=df, family=poisson)
summarise(mods4)

# JobSatisfaction data
data(JobSatisfaction, package="vcd")
modSat <- Kway(Freq ~ management+supervisor+own, data=JobSatisfaction, 
               family=poisson, prefix="JobSat")
summarise(modSat)
anova(modSat, test="Chisq")

# Rochdale data: very sparse, in table form
data(Rochdale, package="vcd")
\dontrun{
modRoch <- Kway(Freq~EconActive + Age + HusbandEmployed + Child + 
                     Education + HusbandEducation + Asian + HouseholdWorking,
                data=Rochdale, family=poisson)
summarise(modRoch)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
