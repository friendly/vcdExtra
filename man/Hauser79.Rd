\name{Hauser79}
\alias{Hauser79}
\docType{data}
\title{
Hauser (1979) Data on Social Mobility
}
\description{
Hauser (1979) presented this two-way frequency table,
cross-classifying occupational categories of sons and fathers
in the United States.
}
\usage{data(Hauser79)}
\format{
  A frequency data frame with 25 observations on the following 3 variables, representing the
  cross-classification of 19912 individuals by father's occupation and son's first occupation. 
  \describe{
    \item{\code{Son}}{a factor with levels \code{UpNM} \code{LoNM} \code{UpM} \code{LoM} \code{Farm}}
    \item{\code{Father}}{a factor with levels \code{UpNM} \code{LoNM} \code{UpM} \code{LoM} \code{Farm}}
    \item{\code{Freq}}{a numeric vector}
  }
}
%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
R.M. Hauser (1979), Some exploratory methods for modeling mobility tables and other cross-classified data. 
In: K.F. Schuessler (Ed.), \emph{Sociological Methodology}, 1980, Jossey-Bass, San Francisco, pp. 413-458.
}
\references{
Powers, D.A. and Xie, Y. (2008). \emph{Statistical Methods for Categorical Data Analysis}, 
Bingley, UK: Emerald.
}
\examples{
data(Hauser79)
str(Hauser79)

# display table
structable(~Father+Son, data=Hauser79)

#Examples from Powers & Xie, Table 4.15
# independence model
mosaic(Freq ~ Father + Son, data=Hauser79, shade=TRUE)

hauser.indep <- gnm(Freq ~ Father + Son, data=Hauser79, family=poisson)
mosaic(hauser.indep, ~Father+Son, main="Independence model", gp=shading_Friendly)

hauser.quasi <-  update(hauser.indep, ~ . + Diag(Father,Son))
mosaic(hauser.quasi, ~Father+Son, main="Quasi-independence model", gp=shading_Friendly)

hauser.qsymm <-  update(hauser.indep, ~ . + Diag(Father,Son) + Symm(Father,Son))
mosaic(hauser.qsymm, ~Father+Son, main="Quasi-symmetry model", gp=shading_Friendly)
#mosaic(hauser.qsymm, ~Father+Son, main="Quasi-symmetry model")


# numeric scores for row/column effects
Sscore <- as.numeric(Hauser79$Son)
Fscore <- as.numeric(Hauser79$Father)

# row effects model
hauser.roweff <- update(hauser.indep, ~ . + Father*Sscore)
summarise(hauser.roweff)

# uniform association
hauser.UA <- update(hauser.indep, ~ . + Fscore*Sscore)
summarise(hauser.UA)

# uniform association, omitting diagonals
hauser.UAdiag <- update(hauser.indep, ~ . + Fscore*Sscore + Diag(Father,Son))
summarise(hauser.UAdiag)

# Levels for Hauser 5-level model
levels <- matrix(c(
  2,  4,  5,  5,  5,
  3,  4,  5,  5,  5,
  5,  5,  5,  5,  5,
  5,  5,  5,  4,  4,
  5,  5,  5,  4,  1
  ), 5, 5, byrow=TRUE)

hauser.topo <- update(hauser.indep, ~ . + Topo(Father, Son, spec=levels))
mosaic(hauser.topo, ~Father+Son, main="Topological model", gp=shading_Friendly)

hauser.RC <- update(hauser.indep, ~ . + Mult(Father, Son), verbose=FALSE)
mosaic(hauser.RC, ~Father+Son, main="RC model", gp=shading_Friendly)
summarise(hauser.RC)

# crossings models 
hauser.CR <- update(hauser.indep, ~ . + Crossings(Father,Son))
mosaic(hauser.topo, ~Father+Son, main="Crossings model", gp=shading_Friendly)
summarise(hauser.CR)

hauser.CRdiag <- update(hauser.indep, ~ . + Crossings(Father,Son) + Diag(Father,Son))
summarise(hauser.CRdiag)


# compare model fit statistics
modlist <- glmlist(hauser.indep, hauser.roweff, hauser.UA, hauser.UAdiag, 
                   hauser.quasi, hauser.qsymm,  hauser.topo, 
                   hauser.RC, hauser.CR, hauser.CRdiag)
sumry <- summarise(modlist)
sumry[order(sumry$AIC, decreasing=TRUE),]
# or, more simply
summarise(modlist, sortby="AIC")

mods <- substring(rownames(sumry),8)
with(sumry,
	{plot(Df, AIC, cex=1.3, pch=19, xlab='Degrees of freedom', ylab='AIC')
	text(Df, AIC, mods, adj=c(0.5,-.5), col='red', xpd=TRUE)
	})


}
\keyword{datasets}
