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
#' @details
#'
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
#' **Model notation**
#'
#' For `loglmlist` objects created by \code{\link{seq_loglm}}, the bracket
#' notation distinguishes between models fit to marginal sub-tables and
#' models fit to the full table. Parentheses are used for marginal
#' sub-tables, e.g., `"(Class) (Sex)"`, while square brackets are used
#' for the full table, e.g., `"[Class,Sex,Age] [Survived]"`.
#'
#' The \code{\link{get_models}} function extracts these model strings,
#' and the `abbrev` argument can be used to abbreviate factor names
#' for more compact display, e.g., `"[C,S,A] [S]"`.
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


#' Extract Model Formulas from a glmlist or loglmlist
#'
#' `get_models()` extracts the model formulas or bracket notation from each model in a
#' \code{glmlist} or \code{loglmlist} object. This is useful for labeling
#' models in summaries and plots.
#'
#' @param x A \code{glmlist} or \code{loglmlist} object
#' @param type Type of output: \code{"brackets"} for loglinear bracket notation
#'        (e.g., \code{"[AB] [C]"}), or \code{"formula"} for R formula notation.
#'        For \code{glmlist} objects, only \code{"formula"} is meaningful.
#' @param abbrev Logical or integer. If \code{TRUE} or a positive integer,
#'        abbreviate factor names to that many characters (default 1 when \code{TRUE}).
#'        Only applies to bracket notation.
#' @param \dots Additional arguments passed to \code{loglin2string} (for \code{loglmlist})
#'        such as \code{sep} and \code{collapse}.
#'
#' @return A named character vector with the model formulas or bracket notations.
#'
#' @details
#' For \code{loglmlist} objects created by \code{\link{seq_loglm}}, the bracket
#' notation is stored in the \code{model.string} component. For other \code{loglm}
#' objects, it is constructed from the \code{margin} component using
#' \code{\link{loglin2string}}.
#'
#' For \code{glmlist} objects, the formula is extracted using \code{formula()}.
#'
#' @seealso \code{\link{glmlist}}, \code{\link{loglmlist}}, \code{\link{loglin2string}},
#'          \code{\link{LRstats}}
#'
#' @family glmlist functions
#' @rdname glmlist
#' @export
#' @examples
#' data(Titanic)
#' # Sequential models of joint independence
#' tit.joint <- seq_loglm(Titanic, type = "joint")
#' get_models(tit.joint)
#' get_models(tit.joint, type = "formula")
#'
#' # With abbreviated factor names
#' get_models(tit.joint, abbrev = TRUE)
#' get_models(tit.joint, abbrev = 2)
#'
get_models <- function(x, type = c("brackets", "formula"), abbrev = FALSE, ...) {
  type <- match.arg(type)

  if (inherits(x, "loglmlist")) {
    if (type == "brackets") {
      # Try to get model.string first (set by seq_loglm)
      result <- sapply(x, function(mod) {
        if (!is.null(mod$model.string)) {
          mod$model.string
        } else if (!is.null(mod$margin)) {
          loglin2string(mod$margin, ...)
        } else {
          # Fallback to formula
          deparse(formula(mod))
        }
      })

      # Apply abbreviation if requested
      if (!isFALSE(abbrev)) {
        n <- if (isTRUE(abbrev)) 1 else as.integer(abbrev)
        result <- .abbreviate_brackets(result, n)
      }
    } else {
      # Formula type
      result <- sapply(x, function(mod) {
        if (!is.null(mod$call$formula)) {
          deparse(mod$call$formula)
        } else {
          # Try to construct from margin
          if (!is.null(mod$margin)) {
            deparse(loglin2formula(mod$margin))
          } else {
            NA_character_
          }
        }
      })
    }
  } else if (inherits(x, "glmlist")) {
    # For glmlist, extract formulas
    result <- sapply(x, function(mod) {
      f <- formula(mod)
      # Get just the RHS for cleaner display
      if (type == "formula") {
        deparse(f)
      } else {
        # Try to convert to bracket notation (limited support)
        deparse(f[[3]])  # RHS only
      }
    })
  } else {
    stop("x must be a 'glmlist' or 'loglmlist' object")
  }

  names(result) <- names(x)
  result
}


# Helper function to abbreviate factor names in bracket notation
.abbreviate_brackets <- function(strings, n = 1) {
  sapply(strings, function(s) {
    # Find all bracketed terms: [content] or (content)
    m <- gregexpr("([\\[\\(])([^\\]\\)]+)([\\]\\)])", s, perl = TRUE)
    matches <- regmatches(s, m)[[1]]

    if (length(matches) == 0) return(s)

    replacements <- sapply(matches, function(match) {
      bracket_open <- substr(match, 1, 1)
      bracket_close <- substr(match, nchar(match), nchar(match))
      content <- substr(match, 2, nchar(match) - 1)

      # Split by comma (for multi-factor terms like [Class,Sex])
      if (grepl(",", content)) {
        parts <- strsplit(content, ",\\s*")[[1]]
        abbrev_parts <- abbreviate(parts, minlength = n)
        paste0(bracket_open, paste(abbrev_parts, collapse = ","), bracket_close)
      } else {
        paste0(bracket_open, abbreviate(content, minlength = n), bracket_close)
      }
    }, USE.NAMES = FALSE)

    regmatches(s, m) <- list(replacements)
    s
  }, USE.NAMES = FALSE)
}

