#' Extract Model Formula from a loglm or glm Object
#'
#' `get_model()` extracts the model formula or bracket notation from a single
#' \code{loglm} or \code{glm} object. This is useful for labeling
#' models in summaries and plots. See \code{\link{get_models}} for the
#' corresponding function for \code{loglmlist} and \code{glmlist} objects.
#'
#' @param x A \code{loglm} or \code{glm} object
#' @param type Type of output: \code{"brackets"} for loglinear bracket notation
#'        (e.g., \code{"[AB] [C]"}), or \code{"formula"} for R formula notation.
#'        For \code{glm} objects, only \code{"formula"} is meaningful.
#' @param abbrev Logical or integer. If \code{TRUE} or a positive integer,
#'        abbreviate factor names to that many characters (default 1 when \code{TRUE}).
#'        Only applies to bracket notation.
#' @param \dots Additional arguments passed to \code{loglin2string}
#'        such as \code{sep} and \code{collapse}.
#'
#' @return A character string with the model formula or bracket notation.
#'
#' @details
#' For \code{loglm} objects created by \code{\link{seq_loglm}}, the bracket
#' notation is stored in the \code{model.string} component. For other \code{loglm}
#' objects, it is constructed from the \code{margin} component using
#' \code{\link{loglin2string}}.
#'
#' For \code{glm} objects, the formula is extracted using \code{formula()}.
#'
#' @seealso \code{\link{get_models}} for \code{glmlist} and \code{loglmlist} objects,
#'          \code{\link{loglin2string}}, \code{\link{seq_mosaic}}
#'
#' @family loglinear models
#' @export
#' @examples
#' data(Titanic)
#' tit.joint <- seq_loglm(Titanic, type = "joint")
#' get_model(tit.joint[[2]])
#' get_model(tit.joint[[4]])
#' get_model(tit.joint[[4]], abbrev = TRUE)
#' get_model(tit.joint[[4]], type = "formula")
#'
get_model <- function(x, type = c("brackets", "formula"), abbrev = FALSE, ...) {
  type <- match.arg(type)

  if (inherits(x, "loglm")) {
    if (type == "brackets") {
      if (!is.null(x$model.string)) {
        result <- x$model.string
      } else if (!is.null(x$margin)) {
        result <- loglin2string(x$margin, ...)
      } else {
        result <- deparse(formula(x))
      }

      # Apply abbreviation if requested
      if (!isFALSE(abbrev)) {
        n <- if (isTRUE(abbrev)) 1 else as.integer(abbrev)
        result <- .abbreviate_brackets(result, n)
      }
    } else {
      # Formula type
      if (!is.null(x$call$formula)) {
        result <- deparse(x$call$formula)
      } else if (!is.null(x$margin)) {
        result <- deparse(loglin2formula(x$margin))
      } else {
        result <- NA_character_
      }
    }
  } else if (inherits(x, "glm")) {
    f <- formula(x)
    if (type == "formula") {
      result <- deparse(f)
    } else {
      result <- deparse(f[[3]])  # RHS only
    }
  } else {
    stop("x must be a 'loglm' or 'glm' object")
  }

  result
}
