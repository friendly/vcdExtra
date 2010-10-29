
# Summarize a glm object or glmlist

summarize <- function(object, ...) {
	UseMethod("summarize")
}

stat.summarize <- function(deviance, df, onames, n) {
	p <- pchisq(deviance, df, lower.tail=FALSE)
	aic <- deviance - 2*df
	if (missing(n)) {
		result <- data.frame(deviance, df, p, aic)
		names(result) <- c("LR Chisq", "Df", "Pr(>Chisq)", "AIC")
	}
	else {
		bic <- deviance - log(n)*df
		result <- data.frame(deviance, df, p, aic, bic)
		names(result) <- c("LR Chisq", "Df", "Pr(>Chisq)", "AIC", "BIC")
	}
	
	rownames(result) <- onames
	attr(result, "heading") <- "Model Summary:"
	class(result) <- c("anova", "data.frame")
	result
}


summarize.glm <-function(object, ..., test=NULL){
	dotargs <- list(...)
	is.glm <- unlist(lapply(dotargs, function(x) inherits(x, "glm")))
	dotargs <- dotargs[is.glm]
	if (length(dotargs)) 
		return(summarize.glmlist(c(list(object), dotargs), test = test))
	
	oname <- as.character(sys.call())[2]
	result <- stat.summarize(object$deviance, object$df.residual, oname, sum(fitted(object)))
	result
}

summarize.glmlist <-function(object, ..., test=NULL){
	nmodels <- length(object)
	if (nmodels == 1) 
		return(summarize.glm(object[[1]], test = test))
	if (is.null(names(object))) {
		oname <- as.character(sys.call())[-1]
		oname <- oname[1:length(object)]
	}
	else oname <- names(object)
	
	resdf <- as.numeric(lapply(object, function(x) x$df.residual))
	resdev <- as.numeric(lapply(object, function(x) x$deviance))
	n <- as.numeric(lapply(object, function(x) sum(fitted(x))))
	result <- stat.summarize(resdev, resdf, oname, n)
	result
}


summarize.loglm <-function(object, ...){
	dotargs <- list(...)
	is.loglm <- unlist(lapply(dotargs, function(x) inherits(x, "loglm")))
	dotargs <- dotargs[is.loglm]
	if (length(dotargs)) 
		return(summarize.loglmlist(c(list(object), dotargs)))
	
	oname <- as.character(sys.call())[2]
	result <- stat.summarize(object$deviance, object$df, oname, sum(fitted(object)))
	result
}

summarize.loglmlist <-function(object, ...){
	nmodels <- length(object)
	if (nmodels == 1) 
		return(summarize.loglm(object[[1]]))
	if (is.null(names(object))) {
		oname <- as.character(sys.call())[-1]
		oname <- oname[1:length(object)]
	}
	else oname <- names(object)
	
	resdf <- as.numeric(lapply(object, function(x) x$df))
	resdev <- as.numeric(lapply(object, function(x) x$deviance))
	n <- as.numeric(lapply(object, function(x) sum(fitted(x))))
	result <- stat.summarize(resdev, resdf, oname, n)
	result
}

