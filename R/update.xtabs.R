#' Update method for a `xtabs` object
#'
#' Provides an `update` method for `"xtabs"` objects, typically by
#' removing terms from the formula to collapse over them.
#'
#'
#' @param object An existing `"xtabs"` object
#' @param formula. Changes to the formula ? see
#' \code{\link[stats]{update.formula}} for details
#' @param \dots Additional arguments to the call, or arguments with changed
#' values.
#' @param evaluate If `TRUE`, evaluate the new call else return the call
#' @return If `evaluate == TRUE`, the new `"xtabs"` object, otherwise
#' the updated call
#' @author Michael Friendly
#' @seealso \code{\link[stats]{update.formula}} for details on updates to model
#' formulae
#'
#' \code{\link[base]{margin.table}} does something similar,
#' \code{\link{collapse.table}} collapses category levels
#' @keywords models
#' @exportS3Method
#' @examples
#'
#' vietnam.tab <- xtabs(Freq ~ sex + year + response, data=Vietnam)
#'
#' update(vietnam.tab, formula = ~ . -year)
#'
#'
#'
#' @export update.xtabs
update.xtabs <-
function (object, formula., ..., evaluate = TRUE)
{
    if (is.null(call<-attr(object, "call")))
        stop("need an object with call component")
    extras <- match.call(expand.dots = FALSE)$...
    if (!missing(formula.))
        call$formula <- update.formula(call$formula, formula.)
    if (length(extras)) {
        existing <- !is.na(match(names(extras), names(call)))
        for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
        if (any(!existing)) {
            call <- c(as.list(call), extras[!existing])
            call <- as.call(call)
        }
    }
    if (evaluate)
        eval(call, parent.frame())
    else call
}

