\name{mosaic.glm}
\alias{mosaic.glm}
\alias{sieve.glm}
\alias{assoc.glm}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mosaic plots for fitted generalized linear and generalized nonlinear models }
\description{
Procduces mosaic plots (and other plots in the \code{\link[vcd]{strucplot}} framework) 
for a log-linear model fitted with \code{\link[stats]{glm}}
or for a generalized nonlinear model fitted with \code{\link[gnm]{gnm}}.

These methods extend the range of strucplot
visualizations well beyond the models that can
be fit with \code{\link[MASS]{loglm}}.
They are intended for models for counts using the Poisson family (or quasi-poisson),
but should be sensible as long as (a) the response variable is non-negative and (b) the
predictors visualized in the \code{strucplot} are discrete factors.
}
\usage{
\method{mosaic}{glm}(x, formula = NULL, panel = mosaic, 
     type = c("observed", "expected"), 
     residuals = NULL, 
     residuals_type = c("pearson", "deviance", "rstandard"), 
     gp = shading_hcl, gp_args = list(), ...)
\method{sieve}{glm}(x,  ...)
\method{assoc}{glm}(x,  ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A \code{glm} or \code{gnm} object. The response variable, typically a 
    cell frequency, should be non-negative. }
  \item{formula}{ A one-sided formula with the indexing factors of the plot
    separated by '+', determining the order in which the variables are used in the mosaic. 
    A formula must be provided unless \code{x$data}
    inherits from class \code{"table"} -- in which case the indexing
    factors of this table are used, or the factors in \code{x$data}
    (or model.frame(x) if \code{x$data} is an environment) exactly
    cross-classify the data -- in which case this set of
    cross-classifying factors are used. } 
   \item{panel}{Panel function used to draw the plot for visualizing the observed values, residuals 
   and expected values. Currently, one of \code{"mosaic"}, \code{"assoc"}, or \code{"sieve"}
   in \code{vcd}.}
  \item{type}{A character string indicating whether the 
  \code{"observed"} or the \code{"expected"} values of the table should be visualized
  by the area of the tiles or bars.}
  \item{residuals}{ An optional array or vector of residuals corresponding to the cells in the
  data, for example, as calculated by \code{residuals.glm(x)}, \code{residuals.gnm(x)}.}
  \item{residuals_type}{If the \code{residuals} argument is \code{NULL}, residuals are calculated internally
  and used in the display.  In this case, \code{residual_type} can be \code{"pearson"}, 
  \code{"deviance"} or \code{"rstandard"}.  Otherwise (when \code{residuals} is supplied),
  \code{residuals_type} is used as a label for the legend in the 
  plot.  
   }
  \item{gp}{Object of class \code{"gpar"}, shading function or a corresponding generating function 
  (see \code{\link[vcd]{strucplot}} Details and \code{\link[vcd]{shadings}}). 
  Ignored if shade = FALSE.}
  \item{gp_args}{A list of arguments for the shading-generating function, if specified.}
  \item{\dots}{ Other arguments passed to the \code{panel} function e.g., \code{\link[vcd]{mosaic}} }
}
\details{
For both poisson family generalized linear models and loglinear models, standardized residuals
provided by \code{rstandard}
(sometimes called adjusted residuals) are often preferred because they have 
constant unit asymptotic variance.

The \code{sieve} and \code{assoc} methods are simple convenience interfaces to this plot method, setting the panel argument accordingly. 

}
%\note{
%In the current version, the \code{glm} or \code{gnm} object \emph{must} have been fit using
%the \code{data} argument to supply a data.frame or table, rather than with variables
%in the global environment.
%}

\value{
The \code{structable} visualized by \code{\link[vcd]{strucplot}} is returned invisibly.

}
%\references{ ~put references to the literature/web site here ~ }
\author{Heather Turner, Michael Friendly, with help from Achim Zeileis}
%\note{
%}
\seealso{
\code{\link[stats]{glm}}, \code{\link[gnm]{gnm}}, \code{\link[vcd]{plot.loglm}}, \code{\link[vcd]{mosaic}}
}
\examples{
GSStab <- xtabs(count ~ sex + party, data=GSS)
# using the data in table form
mod.glm1 <- glm(Freq ~ sex + party, family = poisson, data = GSStab)
res <- residuals(mod.glm1)    
std <- rstandard(mod.glm1)

# For mosaic.default(), need to re-shape residuals to conform to data
stdtab <- array(std, dim=dim(GSStab), dimnames=dimnames(GSStab))
mosaic(GSStab, gp=shading_Friendly, residuals=stdtab, residuals_type="Std\nresiduals", 
       labeling = labeling_residuals)


# Using externally calculated residuals with the glm() object
mosaic.glm(mod.glm1, residuals=std, labeling = labeling_residuals, shade=TRUE)

# Using residuals_type
mosaic.glm(mod.glm1, residuals_type="rstandard", labeling = labeling_residuals, shade=TRUE)

## Ordinal factors and structured associations
data(Mental)
xtabs(Freq ~ mental+ses, data=Mental)
long.labels <- list(set_varnames = c(mental="Mental Health Status", ses="Parent SES"))

# fit independence model
# Residual deviance: 47.418 on 15 degrees of freedom
indep <- glm(Freq ~ mental+ses,
                family = poisson, data = Mental)

long.labels <- list(set_varnames = c(mental="Mental Health Status", 
                                     ses="Parent SES"))
mosaic(indep,residuals_type="rstandard", labeling_args = long.labels, labeling=labeling_residuals)
# or, show as a sieve diagram
mosaic(indep, labeling_args = long.labels, panel=sieve, gp=shading_Friendly)

# fit linear x linear (uniform) association.  Use integer scores for rows/cols 
Cscore <- as.numeric(Mental$ses)
Rscore <- as.numeric(Mental$mental)

linlin <- glm(Freq ~ mental + ses + Rscore:Cscore,
                family = poisson, data = Mental)
mosaic(linlin,residuals_type="rstandard", 
 labeling_args = long.labels, labeling=labeling_residuals, suppress=1, gp=shading_Friendly,
 main="Lin x Lin model")

##  Goodman Row-Column association model fits even better (deviance 3.57, df 8)
if (require(gnm)) {
Mental$mental <- C(Mental$mental, treatment)
Mental$ses <- C(Mental$ses, treatment)
RC1model <- gnm(Freq ~ ses + mental + Mult(ses, mental),
                family = poisson, data = Mental)

mosaic(RC1model,residuals_type="rstandard", 
 labeling_args = long.labels, labeling=labeling_residuals, suppress=1, gp=shading_Friendly,
 main="RC1 model")
 }
 
 ############# UCB Admissions data, fit using glm()
 
 structable(Dept ~ Admit+Gender,UCBAdmissions)
 
berkeley <- as.data.frame(UCBAdmissions)
berk.glm1 <- glm(Freq ~ Dept * (Gender+Admit), data=berkeley, family="poisson")
summary(berk.glm1)
mosaic(berk.glm1, gp=shading_Friendly, labeling=labeling_residuals, formula=~Admit+Dept+Gender)
# the same, displaying studentized residuals; note use of formula to reorder factors in the mosaic
mosaic(berk.glm1, residuals_type="rstandard", labeling=labeling_residuals, shade=TRUE, 
	formula=~Admit+Dept+Gender, main="Model: [DeptGender][DeptAdmit]")

## all two-way model
berk.glm2 <- glm(Freq ~ (Dept + Gender + Admit)^2, data=berkeley, family="poisson")
summary(berk.glm2)
mosaic.glm(berk.glm2, residuals_type="rstandard", labeling = labeling_residuals, shade=TRUE,
	formula=~Admit+Dept+Gender, main="Model: [DeptGender][DeptAdmit][AdmitGender]")
anova(berk.glm1, berk.glm2, test="Chisq")

# Add 1 df term for association of [GenderAdmit] only in Dept A
berkeley <- within(berkeley, dept1AG <- (Dept=='A')*(Gender=='Female')*(Admit=='Admitted'))
berkeley[1:6,]
berk.glm3 <- glm(Freq ~ Dept * (Gender+Admit) + dept1AG, data=berkeley, family="poisson")
summary(berk.glm3)
mosaic.glm(berk.glm3, residuals_type="rstandard", labeling = labeling_residuals, shade=TRUE,
	formula=~Admit+Dept+Gender, main="Model: [DeptGender][DeptAdmit] + DeptA*[GA]")
anova(berk.glm1, berk.glm3, test="Chisq")
 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{models}
\keyword{multivariate}
% __ONLY ONE__ keyword per line
