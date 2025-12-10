#' Crossings Interaction of Factors
#'
#' Given two ordered factors in a square, n x n frequency table,
#' `Crossings` creates an n-1 column matrix corresponding to different
#' degrees of difficulty in crossing from one level to the next, as described
#' by Goodman (1972).
#'
#'
#' @param \dots Two factors
#' @return For two factors of `n` levels, returns a binary indicator
#' matrix of `n*n` rows and `n-1` columns.
#' @author Michael Friendly and Heather Turner
#' @seealso
#' \code{\link[stats]{glm}}, \code{\link[gnm]{gnm}} for model fitting
#'      functions for frequency tables;
#' \code{\link[gnm]{Diag}}, \code{\link[gnm]{Mult}}, \code{\link[gnm]{Symm}},
#' \code{\link[gnm]{Topo}} for similar extensions to terms in model formulas.
#'
#' @references
#' Goodman, L. (1972).  Some multiplicative models for the analysis
#' of cross-classified data. In: *Proceedings of the Sixth Berkeley
#' Symposium on Mathematical Statistics and Probability*, Berkeley, CA:
#' University of California Press, pp. 649-696.
#' @keywords models manip
#' @export
#' @examples
#'
#' data(Hauser79)
#' # display table
#' structable(~Father + Son, data=Hauser79)
#'
#' hauser.indep <- gnm(Freq ~ Father + Son,
#'                     data=Hauser79,
#'                     family=poisson)
#'
#' hauser.CR <- update(hauser.indep,
#'                     ~ . + Crossings(Father,Son))
#' LRstats(hauser.CR)
#'
#' hauser.CRdiag <- update(hauser.indep,
#'                         ~ . + Crossings(Father,Son) + Diag(Father,Son))
#' LRstats(hauser.CRdiag)
#'
#'
Crossings <- function(...) {
    dots <- list(...)
    if (length(dots) != 2) stop("Crossings() is defined for only two factors")
    if (length(dots[[1]]) != length(dots[[2]]))
    stop("arguments to Crossings() must all have same length")
    dots <- lapply(dots, as.factor)
    n <- nlevels(dots[[1]])
    if (nlevels(dots[[2]]) != n)
        stop("arguments to Crossings() must all have same number of levels")
    result <- crossings(as.numeric(dots[[1]]), as.numeric(dots[[2]]), n)
    rownames(result) <- do.call("paste", c(dots, sep = ""))
    result
}

crossings <- function(i, j, n) {
  npar <- n - 1
  result <- list()
  for(c in 1:npar) {
    overi <- c >= i
    overj <- c >= j
    result[[c]] <- (overi & !overj) + (overj & !overi)
  }
  result <- matrix(unlist(result), length(i), npar)
  colnames(result) <- paste('C', 1:npar, sep='')
  result
}





