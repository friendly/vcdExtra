\name{loddsratio}
\alias{loddsratio}
\alias{loddsratio.default}

\alias{coef.loddsratio}
%\alias{confint.loddsratio}
\alias{dim.loddsratio}
\alias{dimnames.loddsratio}
\alias{print.loddsratio}
\alias{vcov.loddsratio}
\alias{as.array}
\alias{as.matrix.loddsratio}
\alias{as.array.loddsratio}
\alias{as.data.frame.loddsratio}

\title{
Calculate Log Odds Ratios for Frequency Tables
}
\description{
Computes (log) odds ratios and their asymptotic variance covariance matrix for (possibly) stratified data.
Odds ratios are calculated for two array dimensions, separately for each level of
all stratifying dimensions.
This generalizes the \code{\link[vcd]{oddsratio}} methods in the vcd package from
2 x 2 ( x strata) tables to R x C (x strata) tables.  In future versions, these
functions may be renamed and or moved to the vcd package.
}
\usage{

loddsratio(x, \dots)
\method{loddsratio}{default}(x, strata = NULL, log = TRUE, 
  ref = NULL, correct = any(x == 0), \dots)

\method{coef}{loddsratio}(object, log = object$log, \dots)
\method{vcov}{loddsratio}(object, log = object$log, \dots)
\method{print}{loddsratio}(x, log = x$log, \dots)

as.array(x, \dots)
\method{as.array}{loddsratio}(x, log=x$log, \dots)
}

%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x}{An object. For the default method a k-way matrix/table/array
    of frequencies. The number of margins has to be at least 2.}
  \item{strata}{Numeric or character indicating the margins of a
    k-way table \code{x} (with k greater than 2) that should be employed
    as strata. By default dimensions \code{3:length(dim(x))} are used.}
  \item{ref}{Numeric or character. Reference categories for the (non-stratum)
    row and column dimensions that should be employed for computing the
    odds ratios. By default, odds ratios for profile contrasts (or sequential
    contrasts, i.e., successive differences of adjacent categories) are
    used. See details below.}
  \item{log}{Logical. Should the results be displayed on a log scale
    or not? All internal computations are always on the log-scale but the
    results are transformed by default if \code{log = TRUE}.}
  \item{correct}{Logical. Should a continuity correction be applied (by adding
    0.5 to all table entries) before computing odds ratios? By default, this
    not employed unless there are any zero cells in the table,
    but this correction is often recommended to reduce bias when some frequencies 
    are small (Fleiss, 1981).}
  \item{object}{An object of class \code{loddsratio} as computed by
    \code{loddsratio}.}
  \item{\dots}{Arguments passed to methods.}

%  \item{parm}{A specification of which parameters are to be given confidence intervals, 
%  	either a vector of numbers or a vector of names. If missing, all parameters are considered.}

%  \item{level}{the confidence level required for the \code{confint} method.}

}
\details{
For an R x C table, (log) odds ratios are formed for the set of (R-1) x (C-1)
2 x 2 tables, corresponding to some set of contrasts among the row and column
variables.  The \code{ref} argument allows these to be specified in a general
way.

\code{ref = NULL} (default) corresponds to \dQuote{profile contrasts} 
(or sequential contrasts or successive differences) for ordered categories,
i.e., R1-R2, R2-R3, R3-R4, etc., and similarly for the column categories.
These are sometimes called \dQuote{local odds ratios}.

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
\item{coefficients}{A named vector, of length (R-1) x (C-1) x \code{prod(dim(x)[strata])}
	containing the log odds ratios. Use the \code{coef} method to 
	extract these from the object, and the \code{confint} method for confidence intervals.
	For a two-way table, the names for the log odds ratios are constructed in the form
	Ri:Rj/Ci:Cj using the table names for rows and columns. For a stratified table,
	the names are constructed in the form Ri:Rj/Ci:Cj|Lk.
	}
\item{vcov}{Variance covariance matrix of the log odds ratios.}
\item{dimnames}{Dimension names for the log odds ratios, considered as a table of
  size \code{c(R-1, C-1, dim(x)[strata]}. Use the \code{dim} and \code{dimnames} methods
  to extract these and manipulate the log odds ratios in relation to the original table.}
\item{dim}{Corresponding dimension vector.}
\item{contrasts}{A matrix C, such that \code{C \%*\% as.vector(log(x))} gives the log odds
  ratios. Each row corresponds to one log odds ratio, and is all zero, except for 4 elements
  of \code{c(1, -1, -1, 1)} for a given 2 x 2 subtable.}
\item{log}{A logical, indicating whether the value of \code{log} in the original call.}
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
The \code{dim} and \code{dimnames} methods provide the proper attributes for
treating the \code{coefficients} vector as an (R-1) x (C-1) x strata array.
\code{as.matrix} and \code{as.array} methods are also provided for this purpose.

The \code{confint} method computes confidence intervals for the log odds ratios
(or for odds ratios, with \code{log=FALSE}).
The \code{\link[lmtest]{coeftest}} method prints the asympotic standard errors,
 z tests (standardized log odds ratios), and the corresponding p values.   
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[vcd]{oddsratio}}, \code{\link[stats]{confint}},

\code{\link[lmtest]{coeftest}} for z-tests of significance


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

# odds ratio plot
lor.CM.df <- as.data.frame(lor.CM)
lor.CM.df <- within(lor.CM.df, 
	{lower<-LOR-ASE
	 upper<-LOR+ASE}
	)
range <- c(min(lor.CM.df$lower), max(lor.CM.df$upper))
with(lor.CM.df, {
	plot(LOR ~ as.numeric(Age), type='b', pch=16, xaxt='n',
	  ylim=range, 
		xlab="Age", ylab="Log odds ratio: Wheeze x Breathlessness",
		main="CoalMiners data: Log odds ratio plot")
	axis(side=1, at=as.numeric(Age), labels=Age)
	segments(as.numeric(Age), lower, as.numeric(Age), upper)
	}
	)
# fit linear models using WLS
abline(lm(LOR ~ as.numeric(Age), weights=1/ASE^2, data=lor.CM.df), col="blue")
age <- seq(25, 60, by = 5)
qmod <- lm(LOR ~ poly(age,2), weights=1/ASE^2, data=lor.CM.df)
lines(fitted(qmod), col = "red", lwd=2)


## 2 x k x 2
lor.Emp <-loddsratio(Employment)
lor.Emp
confint(lor.Emp)

# visualize the log odds ratios
lor.Emp.a <- as.array(lor.Emp)
matplot(lor.Emp.a, type='b', xaxt='n', pch=15:16, cex=1.5,
	ylab='log odds ratio: Employment Status x Length',
	xlab='Employment Length', 
	xlim=c(0.2, 5), 
	main="Employment status data")
abline(h=0, col='gray')
axis(side=1, at=1:5, labels=rownames(lor.Emp.a))
text(0.3, lor.Emp.a[1,], colnames(lor.Emp.a), pos=4, col=1:2)
text(0.5, max(lor.Emp.a[1,])+.1, "Layoff cause")


## R x C case
#data(Hauser79, package="vcdExtra")
hauser.tab <- xtabs(Freq ~ Father+Son, data=Hauser79)
(lor.hauser <- loddsratio(hauser.tab))
confint(lor.hauser)

# odds ratio plot
op <- par(xpd=TRUE)
matplot(as.matrix(lor.hauser), type='b', ylab='log odds ratio', 
	xlab="Son's status comparisons", xaxt='n', xlim=c(1,4.5), ylim=c(-.5,3),
	main="Hauser79 data")
abline(h=0, col='gray')
axis(side=1, at=1:4, labels=colnames(lor.hauser))
text(4, as.matrix(lor.hauser)[4,], rownames(lor.hauser), pos=4, col=1:4)
text(4, 3, "Father's status")
par(op)


## 4 way tables 
data(Punishment, package="vcd")
punish <- xtabs(Freq ~ memory + attitude + age + education, data = Punishment)
mosaic(~ age + education + memory + attitude, data = punish, keep = FALSE,
       gp = gpar(fill = grey.colors(2)), spacing = spacing_highlighting,
       rep = c(attitude = FALSE))

lor.pun <- loddsratio(punish)
lor.pun
confint(lor.pun)
if(require("lmtest")) coeftest(lor.pun)

# visualize the log odds ratios, by education
lor.pun.a <- as.array(lor.pun)
matplot(lor.pun.a, type='b', xaxt='n', pch=15:17, cex=1.5,
	ylab='log odds ratio: Attitude x Memory',
	xlab='Age', xlim=c(0.5, 3), ylim=c(-2, 1), 
	main="Attitudes toward corporal punishment")
abline(h=0, col='gray')
axis(side=1, at=1:3, labels=rownames(lor.pun.a))
text(0.5, lor.pun.a[1,], colnames(lor.pun.a), pos=4, col=1:3)
text(0.6, max(lor.pun.a[1,])+.2, "Education")

# visualize the log odds ratios, by age
matplot(t(lor.pun.a), type='b', xaxt='n', pch=15:17, cex=1.5,
	ylab='log odds ratio: Attitude x Memory',
	xlab='Education', xlim=c(0.5, 3), ylim=c(-2, 1), 
	main="Attitudes toward corporal punishment")
abline(h=0, col='gray')
axis(side=1, at=1:3, labels=colnames(lor.pun.a))
text(0.5, lor.pun.a[,1], rownames(lor.pun.a), pos=4, col=1:3)
text(0.6, max(lor.pun.a[,1])+.2, "Age")

# fit linear model using WLS
lor.pun.df <- as.data.frame(lor.pun)
pun.mod1 <- lm(LOR ~ as.numeric(age) * as.numeric(education), data=lor.pun.df, weights=1/ASE^2)
anova(pun.mod1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{category}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
