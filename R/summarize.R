
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
    
    oname <- as.character(sys.call())[2]
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


#	result <- data.frame(LR, df, p)
#	row.names(result) <- names
#	names(result) <- c("LR Chisq", "Df", "Pr(>Chisq)")
#	class(result) <- c("anova", "data.frame")
#	attr(result, "heading") <- c("Analysis of Deviance Table (Type II tests)\n",
#			paste("Response:", responseName(mod)))
#	result

if(FALSE) {
## Hmisc::llist
llist <-
function (..., labels = TRUE) 
{
    dotlist <- list(...)
    lname <- names(dotlist)
    name <- vname <- as.character(sys.call())[-1]
    for (i in 1:length(dotlist)) {
        vname[i] <- if (length(lname) && lname[i] != "") 
            lname[i]
        else name[i]
        lab <- vname[i]
        if (labels) {
            lab <- attr(dotlist[[i]], "label")
            if (length(lab) == 0) 
                lab <- vname[i]
        }
        label(dotlist[[i]]) <- lab
    }
    names(dotlist) <- vname[1:length(dotlist)]
    dotlist
}


# from Miguel Porto <mpbertolo@gmail.com>, R-Help, 3/12/2010
nlist=function(...) {
    a=list(...);
    names(a)=as.character(match.call()[2:(length(a)+1)])
    return(a);
}

}