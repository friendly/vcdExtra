#' Loglinear Model Utilities
#'
#' @description
#' These functions generate lists of terms to specify a loglinear model in a
#' form compatible with \code{\link[stats]{loglin}} and also provide for
#' conversion to an equivalent \code{\link[MASS]{loglm}} specification or a
#' shorthand character string representation.
#'
#' They allow for a more conceptual way to specify such models by a function
#' for their type, as opposed to just an uninterpreted list of model terms and
#' also allow easy specification of marginal models for a given contingency
#' table.
#' They are intended to be used as tools in higher-level modeling and graphics
#' functions, but can also be used directly.
#'
#' @details
#' The main model specification functions, `conditional`, `joint`,
#' `markov`, \dots{}, `saturated`, return a list of vectors
#' indicating the marginal totals to be fit, via the `margin` argument to
#' \code{\link[stats]{loglin}}.
#' Each element of this list corresponds to a
#' high-order term in a hierarchical loglinear model, where, e.g., a term like
#' `c("A", "B")` is equivalent to the \code{\link[MASS]{loglm}} term
#' `"A:B"` and hence automatically includes all low-order terms.
#'
#' Note that these can be used to supply the `expected` argument for the
#' default \code{\link[vcd]{mosaic}} function, when the data is supplied as a
#' contingency table.
#'
#' The table below shows some typical results in terms of the standard
#' shorthand notation for loglinear models, with factors A, B, C, \dots{},
#' where brackets are used to delimit the high-order terms in the loglinear
#' model.
#' \tabular{llll}{
#'   **function** \tab **3-way** \tab **4-way** \tab **5-way** \cr
#'   `mutual` \tab  \[A\]  \[B\]  \[C\]  \tab  \[A\]  \[B\]  \[C\]  \[D\]  \tab \[A\]  \[B\]  \[C\]  \[D\]  \[E\] \cr
#'   `joint`  \tab  \[AB\]  \[C\]  \tab  \[ABC\]  \[D\]  \tab  \[ABCE\]  \[E\]  \cr
#'   `joint (with=1)` \tab   \[A\]  \[BC\]  \tab   \[A\]  \[BCD\]  \tab  \[A\]  \[BCDE\]  \cr
#'   `conditional`  \tab  \[AC\]  \[BC\]  \tab  \[AD\]  \[BD\]  \[CD\]  \tab   \[AE\]  \[BE\]  \[CE\]  \[DE\] \cr
#'   `condit (with=1)`  \tab  \[AB\]  \[AC\]  \tab   \[AB\]  \[AC\]  \[AD\]  \tab  \[AB\]  \[AC\]  \[AD\]  \[AE\] \cr
#'   `markov (order=1)`  \tab   \[AB\]  \[BC\]  \tab   \[AB\]  \[BC\]  \[CD\]  \tab  \[AB\]  \[BC\]  \[CD\]  \[DE\] \cr   
#'   `markov (order=2)`  \tab   \[A\]  \[B\]  \[C\]  \tab  \[ABC\]  \[BCD\]  \tab   \[ABC\]  \[BCD\]  \[CDE\]  \cr
#'   `saturated`  \tab \[ABC\] \tab \[ABCD\] \tab \[ABCDE\] \cr
#' }
#'
#'
#' `loglin2formula` converts the output of one of these to a model formula
#' suitable as the `formula` for of \code{\link[MASS]{loglm}}.
#'
#' `loglin2string` converts the output of one of these to a string
#' describing the loglinear model in the shorthand bracket notation,
#' e.g., `"[A,B] [A,C]"`.
#'
#' @aliases loglin-utilities conditional joint loglin2formula loglin2string
#'          markov mutual saturated
#'
#'
#' models of joint independence, of some factors wrt one or more other factors
#'
#' @param nf number of factors for which to generate model
#' @param table a contingency table used for factor names, typically the output from \code{\link[base]{table}}
#' @param factors names of factors used in the model when `table` is not specified
#' @param with    indices of the factors against which others are considered jointly independent
#' @family loglinear models
#' @rdname loglin-utilities
#' @export

joint <- function(nf,
                  table=NULL,
                  factors=1:nf,
                  with=nf) {
    if (!is.null(table)) factors <- names(dimnames(table))
    if (nf == 1) return (list(term1=factors[1]))
    if (nf == 2) return (list(term1=factors[1], term2=factors[2]))
    others <- setdiff(1:nf, with)
    result <- list(term1=factors[others], term2=factors[with])
    result
  }


#' models of conditional independence of some factors wrt one or more other factors

#' @param nf number of factors for which to generate model
#' @param table a contingency table used for factor names, typically the output from \code{\link[base]{table}}
#' @param factors names of factors used in the model when `table` is not specified
#' @param with    indices of the factors against which others are considered conditionally independent
#' @rdname loglin-utilities
#' @export

conditional <- function(nf,
                        table=NULL,
                        factors=1:nf,
                        with=nf) {
    if (!is.null(table)) factors <- names(dimnames(table))
    if (nf == 1) return (list(term1=factors[1]))
    if (nf == 2) return (list(term1=factors[1], term2=factors[2]))
    main <- setdiff(1:nf, with)
    others <- matrix(factors[with], length(with), length(main))
    result <- rbind(factors[main], others)
    result <- as.list(as.data.frame(result, stringsAsFactors=FALSE))
    names(result) <- paste('term', 1:length(result), sep='')
    result
  }

#' models of mutual independence of all factors

#' @param nf number of factors for which to generate model
#' @param table a contingency table used for factor names, typically the output from \code{\link[base]{table}}
#' @param factors names of factors used in the model when `table` is not specified
#' @rdname loglin-utilities
#' @export

mutual <- function(nf,
                   table=NULL,
                   factors=1:nf) {
   if (!is.null(table)) factors <- names(dimnames(table))
   result <- sapply(factors[1:nf], list)
   names(result) <- paste('term', 1:length(result), sep='')
   result
  }


#' saturated model: highest-order interaction

#' @param nf number of factors for which to generate model
#' @param table a contingency table used for factor names, typically the output from \code{\link[base]{table}}
#' @param factors names of factors used in the model when `table` is not specified
#' @rdname loglin-utilities
#' @export

saturated <- function(nf,
                      table=NULL,
                      factors=1:nf) {
	if (!is.null(table)) factors <- names(dimnames(table))
	list(term1=factors[1:nf])
}

# models of conditional independence, given one pair of variables
## Not needed: handled by condit, with length(with)>1

#condit2 <- function(nf, factors=1:nf, with=1:2) {
#    if (nf == 1) return (list(term1=factors[1]))
#    if (nf == 2) return (list(term1=factors[1], term2=factors[2]))
#    others <- setdiff(1:nf, with)
#    result <- rbind(factors[with], cbind(factors[others], factors[others]))
#    result <- as.list(as.data.frame(result, stringsAsFactors=FALSE))
#    names(result) <- paste('term', 1:length(result), sep='')
#    result
#}

#' markov models of a given order

#' @param nf number of factors for which to generate model
#' @param table a contingency table used for factor names, typically the output from \code{\link[base]{table}}
#' @param factors names of factors used in the model when `table` is not specified
#' @param order   order of the markov chain
#' @rdname loglin-utilities
#' @export

markov <- function(nf,
                   factors=1:nf,
                   order=1) {
    if (nf == 1) return (list(term1=factors[1]))
    if (nf == 2) return (list(term1=factors[1], term2=factors[2]))
    if (length(factors) < order+2) {
      warning(paste('Not enough factors for order', order, 'Markov chain; using order=1'))
      order <-1
      result <- rbind(factors[1:(nf-1)], factors[2:nf])
    }
    else {
      if (nf <= order+1) result <- factors[1:nf]
      else {
        result <- NULL
        for (i in 1:(order+1))
          result <- rbind(result, factors[i:(nf-order+i-1)])
      }
    }

    result <- as.list(as.data.frame(result, stringsAsFactors=FALSE))
    names(result) <- paste('term', 1:length(result), sep='')
    result
}

#' convert a loglin model to a model formula for loglm

#' @param  x a list of terms in a loglinear model, such as returned by `joint`, `conditional`, \dots
#' @param  env environment in which to evaluate the formula
#' @source Code from Henrique Dallazuanna, <wwwhsd@gmail.com>, R-help 7-4-2013
#' @rdname loglin-utilities
#' @export

loglin2formula <- function(x,
                           env = parent.frame()) {
	terms <- lapply(x, paste, collapse = ":")
	formula(sprintf(" ~ %s", do.call(paste, c(terms, sep = "+"))), env=env)
}

#' convert a loglin model to a string, using bracket notation for the high-order terms

#' @param x a list of terms in a loglinear model, such as returned by `joint`, `conditional`, \dots
#' @param brackets characters to use to surround model terms.  Either a single character string containing two characters
#'        or a character vector of length two.
#' @param sep characters used to separate factor names within a term
#' @param collapse  characters used to separate terms
#' @param abbrev   Unused as yet
#' @rdname loglin-utilities
#' @export

loglin2string <- function(x,
                          brackets = c('[', ']'),
                          sep=',',
                          collapse=' ',
                          abbrev) {
	if (length(brackets)==1 && (nchar(brackets)>1)) brackets <- unlist(strsplit(brackets, ""))
	terms <- lapply(x, paste, collapse=sep)
	terms <- paste(brackets[1], terms, brackets[2], sep='')
	paste(terms, collapse= ' ')
}


