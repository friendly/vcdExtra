#' Extract Model Formulas from Model Objects or Model Lists
#'
#' `get_model()` extracts the model formula or bracket notation from a single
#' \code{loglm} or \code{glm} object.
#'
#' `get_models()` does the same for each model in a
#' \code{loglmlist} or \code{glmlist} object.
#' These are useful for labeling models in summaries and plots.
#'
#' @param x For `get_model()`: a \code{loglm} or \code{glm} object.
#'        For `get_models()`: a \code{glmlist} or \code{loglmlist} object.
#' @param type Type of output: \code{"brackets"} for loglinear bracket notation
#'        (e.g., \code{"[AB] [C]"}), or \code{"formula"} for R formula notation.
#'        For \code{glm} and \code{glmlist} objects, only \code{"formula"} is meaningful.
#' @param abbrev Logical or integer. If \code{TRUE} or a positive integer,
#'        abbreviate factor names to that many characters (default 1 when \code{TRUE}).
#'        Only applies to bracket notation.
#' @param \dots Additional arguments passed to \code{loglin2string}
#'        such as \code{sep} and \code{collapse}.
#'
#' @return For `get_model()`: a character string with the model formula or bracket notation.
#'
#'         For `get_models()`: a named character vector with the model formulas or bracket notations.
#'
#' @details
#' For \code{loglm} objects created by \code{\link{seq_loglm}}, the bracket
#' notation is stored in the \code{model.string} component. For other \code{loglm}
#' objects, it is constructed from the \code{margin} component using
#' \code{\link{loglin2string}}.
#'
#' For \code{glm} objects, the formula is extracted using \code{formula()}.
#'
#' **Model notation**
#'
#' For `loglmlist` objects created by \code{\link{seq_loglm}}, the bracket
#' notation distinguishes between models fit to marginal sub-tables and
#' models fit to the full table. Parentheses are used for marginal
#' sub-tables, e.g., `"(Class) (Sex)"`, while square brackets are used
#' for the full table, e.g., `"[Class,Sex,Age] [Survived]"`.
#'
#' The `abbrev` argument can be used to abbreviate factor names
#' for more compact display, e.g., `"[C,S,A] [S]"`.
#'
#' @seealso \code{\link{glmlist}}, \code{\link{loglmlist}},
#'          \code{\link{loglin2string}}, \code{\link{LRstats}},
#'          \code{\link{seq_mosaic}}
#'
#' @family glmlist functions
#' @family loglinear models
#' @export
#' @examples
#' data(Titanic)
#' tit.joint <- seq_loglm(Titanic, type = "joint")
#'
#' # Single model
#' get_model(tit.joint[[2]])
#' get_model(tit.joint[[4]])
#' get_model(tit.joint[[4]], abbrev = TRUE)
#' get_model(tit.joint[[4]], type = "formula")
#'
#' # Model list
#' get_models(tit.joint)
#' get_models(tit.joint, type = "formula")
#'
#' # With abbreviated factor names
#' get_models(tit.joint, abbrev = TRUE)
#' get_models(tit.joint, abbrev = 2)
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


#' @rdname get_model
#' @export
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
