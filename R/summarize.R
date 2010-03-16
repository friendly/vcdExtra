
# Summarize a glm object or glmlist

summarize <- function(object, ...) {
	UseMethod("summarize")
}


summarize.glm <-function(object, ..., dispersion=NULL, test=NULL){
    dotargs <- list(...)
    is.glm <- unlist(lapply(dotargs, function(x) inherits(x, 
        "glm")))
    dotargs <- dotargs[is.glm]
    if (length(dotargs)) 
        return(summarize.glmlist(c(list(object), dotargs), dispersion = dispersion, 
            test = test))
    # get object name if there is one
	oname <- if (typeof(substitute(object))=="symbol") as.character(sys.call())[2] else NULL
	
    resdev <- object$deviance
    resdf <- object$df.residual
    p <- pchisq(resdev, resdf, lower.tail=FALSE)
    aic <- resdev - 2*resdf
    result <- data.frame(resdev, resdf, p, aic)
    names(result) <- c("LR Chisq", "Df", "Pr(>Chisq)", "AIC")
    rownames(result) <- oname
    attr(result, "heading") <- c("Model Summary:")
    class(result) <- c("anova", "data.frame")
    result
}

summarize.glmlist <-function(object, ..., dispersion=NULL, test=NULL){
    nmodels <- length(object)
    if (nmodels == 1) 
        return(summarize.glm(object[[1]], dispersion = dispersion, 
            test = test))
	if (is.null(names(object))) {
	    oname <- as.character(sys.call())[-1]
	    oname <- oname[1:length(object)]
		}
		else oname <- names(object)

    resdf <- as.numeric(lapply(object, function(x) x$df.residual))
    resdev <- as.numeric(lapply(object, function(x) x$deviance))
    p <- pchisq(resdev, resdf, lower.tail=FALSE)
    aic <- resdev - 2*resdf
    result <- data.frame(resdev, resdf, p, aic)
    names(result) <- c("LR Chisq", "Df", "Pr(>Chisq)", "AIC")
    rownames(result) <- oname
    attr(result, "heading") <- c("Model Summary:")
    class(result) <- c("anova", "data.frame")
    result
}

