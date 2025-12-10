# glmlist - make a glmlist object containing a list of fitted glm objects with their names

# borrowing code from Hmisc::llist


#' Create a Model List Object
#'
#' `glmlist` creates a `glmlist` object containing a list of fitted
#' `glm` objects with their names. `loglmlist` does the same for
#' `loglm` objects.
#'
#' The intention is to provide object classes to facilitate model comparison,
#' extraction, summary and plotting of model components, etc., perhaps using
#' \code{\link[base]{lapply}} or similar.
#'
#' There exists a \code{\link[stats]{anova.glm}} method for `glmlist`
#' objects.  Here, a `coef` method is also defined, collecting the
#' coefficients from all models in a single object of type determined by
#' `result`.
#'
#' The arguments to `glmlist` or `loglmlist` are of the form
#' `value` or `name=value`.
#'
#' Any objects which do not inherit the appropriate class `glm` or
#' `loglm` are excluded, with a warning.
#'
#' In the `coef` method, coefficients from the different models are
#' matched by name in the list of unique names across all models.
#'
#' @aliases glmlist loglmlist coef.glmlist
#' @param \dots One or more model objects, as appropriate to the function, optionally assigned names as in \code{\link{list}}.
#' @param object a `"glmlist"` object
#' @param result type of the result to be returned
#'
#' @return An object of class `glmlist` `loglmlist`, just like a `list`, except that each model is given a `name` attribute.
#'
#' @author Michael Friendly; `coef` method by John Fox
#'
#' @seealso The function \code{\link[Hmisc]{llist}} in package `Hmisc` is
#' similar, but perplexingly more general.
#'
#' The function \code{\link[stats]{anova.glm}} also handles `glmlist objects`
#'
#' \code{\link{LRstats}} gives LR statistics and tests for a `glmlist`
#' object.
#' @family glmlist functions
#' @family loglinear models
#'
#' @keywords utilities models
#' @examples
#'
#' data(Mental)
#' indep <- glm(Freq ~ mental+ses,
#'                 family = poisson, data = Mental)
#' Cscore <- as.numeric(Mental$ses)
#' Rscore <- as.numeric(Mental$mental)
#'
#' coleff <- glm(Freq ~ mental + ses + Rscore:ses,
#'                 family = poisson, data = Mental)
#' roweff <- glm(Freq ~ mental + ses + mental:Cscore,
#'                 family = poisson, data = Mental)
#' linlin <- glm(Freq ~ mental + ses + Rscore:Cscore,
#'                 family = poisson, data = Mental)
#'
#' # use object names
#' mods <- glmlist(indep, coleff, roweff, linlin)
#' names(mods)
#'
#' # assign new names
#' mods <- glmlist(Indep=indep, Col=coleff, Row=roweff, LinxLin=linlin)
#' names(mods)
#'
#' LRstats(mods)
#'
#' coef(mods, result='data.frame')
#'
#' #extract model components
#' unlist(lapply(mods, deviance))
#'
#' res <- lapply(mods, residuals)
#' boxplot(as.data.frame(res), main="Residuals from various models")
#'
#' @export
glmlist <- function(...) {
    args  <- list(...);
    lname <- names(args)
    name <- vname <- as.character(sys.call())[-1]
    for (i in 1:length(args)) {
        vname[i] <- if (length(lname) && lname[i] != "")
            lname[i]
        else name[i]
    }
    names(args) <- vname[1:length(args)]
    is.glm <- unlist(lapply(args, function(x) inherits(x, "glm")))
    if (!all(is.glm)) {
    	warning("Objects ", paste(vname[!is.glm], collapse=', '),
              " removed because they are not `glm` objects")
    	args <- args[is.glm]
    }
    class(args) <- "glmlist"
    return(args);
}

# loglmlist - do the same for loglm objects

#' @rdname glmlist
#' @export
loglmlist <- function(...) {
    args  <- list(...);
    lname <- names(args)
    name <- vname <- as.character(sys.call())[-1]
    for (i in 1:length(args)) {
        vname[i] <- if (length(lname) && lname[i] != "")
            lname[i]
        else name[i]
    }
    names(args) <- vname[1:length(args)]
    is.loglm <- unlist(lapply(args, function(x) inherits(x, "loglm")))
    if (!all(is.loglm)) {
    	warning("Objects ", paste(vname[!is.loglm], collapse=', '),
              " removed because they are not `loglm` objects")
    	args <- args[is.loglm]
    }
    class(args) <- "loglmlist"
    return(args);
}

# generic version: named list

nlist <- function(...) {
    args  <- list(...);
    lname <- names(args)
    name <- vname <- as.character(sys.call())[-1]
    for (i in 1:length(args)) {
        vname[i] <- if (length(lname) && lname[i] != "")
            lname[i]
        else name[i]
    }
    names(args) <- vname[1:length(args)]
    return(args);
}

# coeficient method for a glmlist (from John Fox, r-help, 10-28-2014)

#' @rdname glmlist
#' @export
coef.glmlist <- function(object, result=c("list", "matrix", "data.frame"),
		...){
	result <- match.arg(result)
	coefs <- lapply(object, coef)
	if (result == "list") return(coefs)
	coef.names <- unique(unlist(lapply(coefs, names)))
	n.mods <- length(object)
	coef.matrix <- matrix(NA, length(coef.names), n.mods)
	rownames(coef.matrix) <- coef.names
	colnames(coef.matrix) <- names(object)
	for (i in 1:n.mods){
		coef <- coef(object[[i]])
		coef.matrix[names(coef), i] <- coef
	}
	if (result == "matrix") return(coef.matrix)
	as.data.frame(coef.matrix)
}

