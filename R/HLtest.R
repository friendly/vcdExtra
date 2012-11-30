# Functions for Hosmer Lemeshow test
# original function downloaded from 
# http://sas-and-r.blogspot.com/2010/09/example-87-hosmer-and-lemeshow-goodness.html
#
# see also: MKmisc::gof.test for more general versions



HLtest <- HosmerLemeshow <- function(model, g=10) {
	if (!inherits(model, "glm")) stop("requires a binomial family glm")
	if (!family(model)$family == 'binomial') stop("requires a binomial family glm")
	y <- model$y
	yhat <- model$fitted.values
  cutyhat = cut(yhat,
     breaks = quantile(yhat, probs=seq(0, 1, 1/g)), 
     include.lowest=TRUE)
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)
  exp = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)

  chi = (obs - exp)/sqrt(exp)
	# browser()
  table <- data.frame(cut=dimnames(obs)$cutyhat,
                      total= as.numeric(apply(obs, 1, sum)),
                      obs=as.numeric(as.character(obs[,1])),
                      exp=as.numeric(as.character(exp[,1])), 
                      chi=as.numeric(as.character(chi[,1]))
                         )

  rownames(table) <- 1:g
  chisq = sum(chi^2)
  p = 1 - pchisq(chisq, g - 2)
  result <- list(table=table, chisq=chisq, df=g-2, p.value=p, groups=g, call=model$call)
  class(result) <- "HLtest"
  return(result)
}

print.HLtest <- function(x, ...) {
	heading <- "Hosmer and Lemeshow Goodness-of-Fit Test"
	df <- data.frame("ChiSquare"=x$chisq, df=x$df, "P_value"= x$p.value)
	cat(heading,"\n\n")
  cat("Call:\n")
  print(x$call)
	print(df, row.names=FALSE)
	invisible(x)
}

# Q: how to print **s next to larg chisq components?
summary.HLtest <- function(object, ...) {
	heading <- "Partition for Hosmer and Lemeshow Goodness-of-Fit Test"
	cat(heading,"\n\n")
	print(object$table)
	print(object)
}

## Q: how to display any large chi residuals on the bars??
rootogram.HLtest <- function(x, ...) {
	vcd:::rootogram.default(as.numeric(x$table$obs), as.numeric(x$table$exp), 
		xlab="Fitted value group", names=1:x$groups, ...) 
}

plot.HLtest <- function(x, ...) {
	rootogram.HLtest(x, ...)
}


TESTME<-TRUE
if(TESTME) {
data(birthwt, package="MASS")

birthwt <- transform(birthwt,
	race = factor(race, labels = c("white", "black", "other")),
	ptd = factor(ptl > 0),
	ftv = factor(ftv)
)
levels(birthwt$ftv)[-(1:2)] = "2+"


# how to do this without attach?
attach(birthwt)
	race = factor(race, labels = c("white", "black", "other"))
	ptd = factor(ptl > 0)
	ftv = factor(ftv)
	levels(ftv)[-(1:2)] = "2+"
bwt <- data.frame(low = factor(low), age, lwt, race,
    smoke = (smoke > 0), ptd, ht = (ht > 0), ui = (ui > 0), ftv)
detach(birthwt)
options(contrasts = c("contr.treatment", "contr.poly"))
BWmod <- glm(low ~ ., family=binomial, data=bwt)

(hlt <- HLtest(BWmod))
str(hlt)
summary(hlt)
plot(hlt)

# basic model
BWmod0 <- glm(low ~ age, family=binomial, data=bwt)
(hlt0 <- HLtest(BWmod0))
str(hlt0)
summary(hlt0)
plot(hlt0)


## test for a plot method, plot.HLtest()
#rootogram(as.numeric(hlt$table$obs), as.numeric(hlt$table$exp), xlab="group", names=1:10)

}
