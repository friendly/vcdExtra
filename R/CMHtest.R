# Cochran-Mantel-Haenszel tests for ordinal factors in contingency tables

# The code below follows Stokes, Davis & Koch, (2000).
#   "Categorical Data Analysis using the SAS System", 2nd Ed.,
#   pp 74--75, 92--101, 124--129.

# Ref: Landis, R. J., Heyman, E. R., and Koch, G. G. (1978),
#		Average Partial Association in Three-way Contingency Tables:
#		A Review and Discussion of Alternative Tests,
#		International Statistical Review, 46, 237-254.

# See: https://onlinecourses.science.psu.edu/stat504/book/export/html/90
# http://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_freq_a0000000648.htm

# DONE: this should be the main function, handling 2-way & higher-way tables
#  With strata, use apply() or recursion over strata
# DONE: With strata, calculate overall CMH tests controlling for strata
# FIXED: rmeans and cmeans tests were labeled incorrectly

#' Generalized Cochran-Mantel-Haenszel Tests
#'
#' Provides generalized Cochran-Mantel-Haenszel tests of association of two
#' possibly ordered factors, optionally stratified other factor(s).  With
#' strata, `CMHtest` calculates these tests for each level of the
#' stratifying variables and also provides overall tests controlling for the
#' strata.
#'
#' For ordinal factors, more powerful tests than the test for general
#' association (independence) are obtained by assigning scores to the row and
#' column categories.
#'
#' The standard \eqn{\chi^2} tests for association in a two-way table treat
#' both table factors as nominal (unordered) categories. When one or both
#' factors of a two-way table are quantitative or ordinal, more powerful tests
#' of association may be obtained by taking ordinality into account using row
#' and or column scores to test for linear trends or differences in row or
#' column means.
#'
#' The CMH analysis for a two-way table produces generalized
#' Cochran-Mantel-Haenszel statistics (Landis etal., 1978).
#'
#' These include the CMH **correlation** statistic (`"cor"`), treating
#' both factors as ordered. For a given statum, with equally spaced row and
#' column scores, this CMH statistic reduces to \eqn{(n-1) r^2}, where \eqn{r}
#' is the Pearson correlation between X and Y. With `"midrank"` scores,
#' this CMH statistic is analogous to \eqn{(n-1) r_S^2}, using the Spearman
#' rank correlation.
#'
#' The **ANOVA** (row mean scores and column mean scores) statistics, treat
#' the columns and rows respectively as ordinal, and are sensitive to mean
#' shifts over columns or rows. These are transforms of the \eqn{F} statistics
#' from one-way ANOVAs with equally spaced scores and to Kruskal-Wallis tests
#' with `"midrank"` scores.
#'
#' The CMH **general** association statistic treat both factors as
#' unordered, and give a test closely related to the Pearson \eqn{\chi^2} test.
#' When there is more than one stratum, the overall general CMH statistic gives
#' a stratum-adjusted Pearson \eqn{\chi^2}, equivalent to what is calculated by
#' \code{\link[stats]{mantelhaen.test}}.
#'
#' For a 3+ way table, one table of CMH tests is produced for each combination
#' of the factors identified as `strata`. If `overall=TRUE`, an
#' additional table is calculated for the same two primary variables,
#' controlling for (pooling over) the `strata` variables.
#'
#' These overall tests implicitly assume no interactions between the primary
#' variables and the strata and they will have low power in the presence of
#' interactions.
#'
#' Note that strata combinations with insufficient data (less than 2
#' observations) are automatically omitted from the analysis.
#'
#' @aliases CMHtest CMHtest.formula CMHtest.default Cochran Mantel Haenszel test print.CMHtest
#' @param x A 2+ way contingency table in array form, or a class `"table"`
#'          object with optional category labels specified in the dimnames(x) attribute.
#' @param formula a formula specifying the variables used to create a
#'           contingency table from `data`.  This should be a one-sided formula when
#'           `data` is in array form, and a two-sided formula with a response
#'           `Freq` if `data` is a data frame with a cell frequency variable.
#'            For convenience, conditioning formulas can be specified indicating strata.
#' @param data either a data frame, or an object of class `"table"` or `"ftable"`.
#' @param subset an optional vector specifying a subset of observations to be used.
#' @param na.action a function which indicates what should happen when the data contain `NA`s.
#'           Ignored if `data` is a contingency table.
#' @param strata For a 3- or higher-way table, the names or numbers of the
#'           factors to be treated as strata.  By default, the first 2 factors are
#'           treated as the main table variables, and all others considered stratifying factors.
#' @param rscores Row scores.  Either a set of numbers (typically integers,
#'            `1:R`) or the string `"midrank"` for standardized midrank scores,
#' or `NULL` to exclude tests that depend on row scores.
#' @param cscores Column scores. Same as for row scores.
#' @param types Types of CMH tests to compute: Any one or more of `c("cor", "cmeans", "rmeans", "general")`, or
#'           `"ALL"` for all of these.
#' @param overall logical. Whether to calculate overall tests, controlling for the stratifying factors.
#' @param details logical.  Whether to include computational details in the result
#' @param \dots Other arguments passed to default method.
#' @param digits Digits to print.
#'
#' @return An object of class `"CMHtest"` , a list with the following 4 components:
#'
#' \item{table}{A matrix containing the test statistics, with columns `Chisq`, `Df` and `Prob` }
#' \item{names}{The names of the table row and column variables}
#' \item{rscore}{Row scores}
#' \item{cscore}{Column scores}
#'
#' If `details==TRUE`, additional components are included.
#'
#' If there are strata, the result is a list of `"CMHtest"` objects. If
#' `overall=TRUE` another component, labeled `ALL` is appended to the
#' list.
#'
#' @author Michael Friendly
#'
#' @seealso \code{\link[coin]{cmh_test}} provides the CMH test of general
#' association; \code{\link[coin]{lbl_test}} provides the CMH correlation test
#' of linear by linear association.
#'
#' \code{\link[stats]{mantelhaen.test}} provides the overall general
#' Cochran-Mantel-Haenszel chi-squared test of the null that two nominal
#' variables are conditionally independent in each stratum, assuming that there
#' is no three-way interaction
#' @family association tests
#'
#' @references
#' Stokes, M. E. & Davis, C. S.  & Koch, G., (2000).
#' *Categorical Data Analysis using the SAS System*, 2nd Ed., Cary, NC:
#' SAS Institute, pp 74-75, 92-101, 124-129.  Details of the computation are
#' given at: <http://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_freq_a0000000648.htm>
#'
#' Cochran, W. G. (1954), Some Methods for Strengthening the Common
#' \eqn{\chi^2} Tests, *Biometrics*, 10, 417-451.
#'
#' Landis, R. J., Heyman, E. R., and Koch, G. G. (1978).  Average Partial
#' Association in Three-way Contingency Tables: A Review and Discussion of
#' Alternative Tests, *International Statistical Review*, **46**,
#' 237-254.
#'
#' Mantel, N. (1963), Chi-square Tests with One Degree of Freedom: Extensions
#' of the Mantel-Haenszel Procedure," *Journal of the American Statistical
#' Association*, 58, 690-700.
#' @keywords htest
#' @export
#' @examples
#'
#' data(JobSat, package="vcdExtra")
#' CMHtest(JobSat)
#' CMHtest(JobSat, rscores="midrank", cscores="midrank")
#'
#' # formula interface
#' CMHtest(~ ., data=JobSat)
#'
#' # A 3-way table (both factors ordinal)
#' data(MSPatients, package="vcd")
#' CMHtest(MSPatients)
#'
#'
#' # also calculate overall tests, controlling for Patient
#' CMHtest(MSPatients, overall = TRUE)
#' # compare with mantelhaen.test
#' mantelhaen.test(MSPatients)
#'
#' # formula interface
#' CMHtest(~ ., data = MSPatients, overall = TRUE)
#'
#' # using a frequency data.frame
#' CMHtest(xtabs(Freq~ses + mental, data = Mental))
#' # or, more simply
#' CMHtest(Freq~ses + mental, data = Mental)
#'
#' # conditioning formulae
#' CMHtest(Freq~right + left | gender, data = VisualAcuity)
#'
#' CMHtest(Freq ~ attitude + memory | education + age, data = Punishment)
#'
#'
#' # Stokes etal, Table 5.1, p 92: two unordered factors
#' parties <- matrix(
#' 	c(221, 160, 360, 140,
#' 	  200, 291, 160, 311,
#' 	  208, 106, 316, 97),
#' 	nrow=3, ncol=4,
#' 	byrow=TRUE)
#' dimnames(parties) <- list(party=c("Dem", "Indep", "Rep"),
#'              neighborhood=c("Bayside", "Highland", "Longview", "Sheffield"))
#' CMHtest(parties, rscores=NULL, cscores=NULL)
#'
#' # compare with Pearson chisquare
#' chisq.test(parties)
#'
CMHtest <- function(x, ...) {
  UseMethod("CMHtest")
}

#' @rdname CMHtest
#' @export
CMHtest.formula <-
  function(formula, data = NULL, subset = NULL, na.action = NULL, ...) {
    m <- match.call(expand.dots = FALSE)
    edata <- eval(m$data, parent.frame())

    fstr <- strsplit(paste(deparse(formula), collapse = ""), "~")
    vars <- strsplit(strsplit(gsub(" ", "", fstr[[1]][2]), "\\|")[[1]], "\\+")
    varnames <- vars[[1]]

    condnames <- if (length(vars) > 1) vars[[2]] else NULL

    dep <- gsub(" ", "", fstr[[1]][1])
    if (!dep %in% c("", "Freq")) {
      if (all(varnames == ".")) {
        varnames <- if (is.data.frame(data)) {
          colnames(data)
        } else {
          names(dimnames(as.table(data)))
        }
        varnames <- varnames[-which(varnames %in% dep)]
      }

      varnames <- c(varnames, dep)
    }

    if (
      inherits(edata, "ftable") ||
        inherits(edata, "table") ||
        length(dim(edata)) > 2
    ) {
      condind <- NULL
      dat <- as.table(data)
      if (all(varnames != ".")) {
        ind <- match(varnames, names(dimnames(dat)))
        if (any(is.na(ind))) {
          stop(paste(
            "Can't find",
            paste(varnames[is.na(ind)], collapse = " / "),
            "in",
            deparse(substitute(data))
          ))
        }

        if (!is.null(condnames)) {
          condind <- match(condnames, names(dimnames(dat)))
          if (any(is.na(condind))) {
            stop(paste(
              "Can't find",
              paste(condnames[is.na(condind)], collapse = " / "),
              "in",
              deparse(substitute(data))
            ))
          }
          ind <- c(condind, ind)
        }
        dat <- margin.table(dat, ind)
      }
      CMHtest.default(
        dat,
        strata = if (is.null(condind)) {
          NULL
        } else {
          match(condnames, names(dimnames(dat)))
        },
        ...
      )
    } else {
      m <- m[c(
        1,
        match(c("formula", "data", "subset", "na.action"), names(m), 0)
      )]
      m[[1]] <- as.name("xtabs")
      m$formula <-
        formula(paste(
          if ("Freq" %in% colnames(data)) "Freq",
          "~",
          paste(c(varnames, condnames), collapse = "+")
        ))
      tab <- eval(m, parent.frame())
      CMHtest.default(tab, ...)
    }
  }

#' @rdname CMHtest
#' @export
CMHtest.default <- function(
  x,
  strata = NULL,
  rscores = 1:R,
  cscores = 1:C,
  types = c("cor", "rmeans", "cmeans", "general"),
  overall = FALSE,
  details = overall,
  ...
) {
  snames <- function(x, strata) {
    sn <- dimnames(x)[strata]
    dn <- names(sn)
    apply(expand.grid(sn), 1, function(x) {
      paste(dn, x, sep = ":", collapse = "|")
    })
  }

  ## check dimensions
  L <- length(d <- dim(x))
  if (any(d < 2L)) {
    stop("All table dimensions must be 2 or greater")
  }
  if (L > 2L & is.null(strata)) {
    strata <- 3L:L
  }
  if (is.character(strata)) {
    strata <- which(names(dimnames(x)) == strata)
  }
  if (L - length(strata) != 2L) {
    stop("All but 2 dimensions must be specified as strata.")
  }

  ## rearrange table to put primary dimensions first
  x <- aperm(x, c(setdiff(1:L, strata), strata))

  d <- dim(x)
  R <- d[1]
  C <- d[2]

  # handle strata
  if (!is.null(strata)) {
    sn <- snames(x, strata)
    res <- c(apply(
      x,
      strata,
      CMHtest2,
      rscores = rscores,
      cscores = cscores,
      types = types,
      details = details,
      ...
    ))
    # Drop results for strata with insufficient data (NULL result)
    strata_gives_null <- sapply(res, is.null)
    res <- res[!strata_gives_null]
    sn <- sn[!strata_gives_null]
    # DONE: fix names if there are 2+ strata
    names(res) <- sn
    for (i in seq_along(res)) {
      res[[i]]$stratum <- sn[i]
    }
    # DONE: Calculate generalized CMH, controlling for strata
    if (overall) {
      if (!details) {
        warning("Overall CMH tests not calculated because details=FALSE")
      } else {
        resall <- CMHtest3(res, types = types)
        res$ALL <- resall
      }
    }
    return(res)
  } else {
    CMHtest2(
      x,
      rscores = rscores,
      cscores = cscores,
      types = types,
      details = details,
      ...
    )
  }
}

# handle two-way case, for a given stratum
#  DONE:  now allow rscores/cscores == 'midrank' for midrank scores
#  DONE:  allow rscores/cscores=NULL for unordered factors, where ordinal
#     scores don't make sense
#  DONE: modified to return all A matrices as a list
#  DONE: cmh() moved outside

CMHtest2 <- function(
  x,
  stratum = NULL,
  rscores = 1:R,
  cscores = 1:C,
  types = c("cor", "rmeans", "cmeans", "general"),
  details = FALSE,
  ...
) {
  # left kronecker product
  lkronecker <- function(x, y, make.dimnames = TRUE, ...) {
    kronecker(y, x, make.dimnames = make.dimnames, ...)
  }

  # midrank scores (modified ridits) based on row/column totals
  midrank <- function(n) {
    cs <- cumsum(n)
    (2 * cs - n + 1) / (2 * (cs[length(cs)] + 1))
  }

  L <- length(d <- dim(x))
  R <- d[1]
  C <- d[2]

  if (is.character(rscores) && rscores == "midrank") {
    rscores <- midrank(rowSums(x))
  }
  if (is.character(cscores) && cscores == "midrank") {
    cscores <- midrank(colSums(x))
  }

  nt <- sum(x)

  # If there are not at least 2 observations in this stratum,
  # the below computations won't work, therefore return early NULL here.
  if (nt <= 1) {
    return(NULL)
  }

  pr <- rowSums(x) / nt
  pc <- colSums(x) / nt

  m <- as.vector(nt * outer(pr, pc)) # expected values under independence
  n <- as.vector(x) # cell frequencies

  V1 <- (diag(pr) - pr %*% t(pr))
  V2 <- (diag(pc) - pc %*% t(pc))
  V <- (nt^2 / (nt - 1)) * lkronecker(V1, V2, make.dimnames = TRUE)

  if (length(types) == 1 && types == "ALL") {
    types <- c("general", "rmeans", "cmeans", "cor")
  }
  types <- match.arg(types, several.ok = TRUE)
  # handle is.null(rscores) etc here
  if (is.null(rscores)) {
    types <- setdiff(types, c("cmeans", "cor"))
  }
  if (is.null(cscores)) {
    types <- setdiff(types, c("rmeans", "cor"))
  }

  table <- NULL
  Amats <- list()
  for (type in types) {
    if ("cor" == type) {
      A <- lkronecker(t(rscores), t(cscores))
      df <- 1
      table <- rbind(table, cmh(n, m, A, V, df))
      Amats$cor <- A
    } else if ("rmeans" == type) {
      A <- lkronecker(cbind(diag(R - 1), rep(0, R - 1)), t(cscores))
      df <- R - 1
      table <- rbind(table, cmh(n, m, A, V, df))
      Amats$rmeans <- A
    } else if ("cmeans" == type) {
      A <- lkronecker(t(rscores), cbind(diag(C - 1), rep(0, C - 1)))
      df <- C - 1
      table <- rbind(table, cmh(n, m, A, V, df))
      Amats$cmeans <- A
    } else if ("general" == type) {
      A <- lkronecker(
        cbind(diag(R - 1), rep(0, R - 1)),
        cbind(diag(C - 1), rep(0, C - 1))
      )
      df <- (R - 1) * (C - 1)
      table <- rbind(table, cmh(n, m, A, V, df))
      Amats$general <- A
    }
  }

  colnames(table) <- c("Chisq", "Df", "Prob")
  rownames(table) <- types
  xnames <- names(dimnames(x))
  result <- list(
    table = table,
    names = xnames,
    rscores = rscores,
    cscores = cscores,
    stratum = stratum
  )
  if (details) {
    result <- c(result, list(A = Amats, V = V, n = n, m = m))
  }
  class(result) <- "CMHtest"
  result
}

# do overall test, from a computed CMHtest list
CMHtest3 <- function(object, types = c("cor", "rmeans", "cmeans", "general")) {
  nstrat <- length(object) # number of strata

  # extract components, each a list of nstrat terms
  n.list <- lapply(object, function(s) s$n)
  m.list <- lapply(object, function(s) s$m)
  V.list <- lapply(object, function(s) s$V)
  A.list <- lapply(object, function(s) s$A)
  nt <- sapply(lapply(object, function(s) s$n), sum)
  Df <- object[[1]]$table[, "Df"]

  if (length(types) == 1 && types == "ALL") {
    types <- c("general", "rmeans", "cmeans", "cor")
  }
  types <- match.arg(types, several.ok = TRUE)

  table <- list()
  for (type in types) {
    AVA <- 0
    Anm <- 0
    for (k in 1:nstrat) {
      A <- A.list[[k]][[type]]
      V <- V.list[[k]]
      n <- n.list[[k]]
      m <- m.list[[k]]
      AVA <- AVA + A %*% V %*% t(A)
      Anm <- Anm + A %*% (n - m)
    }
    Q <- t(Anm) %*% MASS::ginv(AVA) %*% Anm
    df <- Df[type]
    pvalue <- pchisq(Q, df, lower.tail = FALSE)
    table <- rbind(table, c(Q, df, pvalue))
  }
  rownames(table) <- types
  colnames(table) <- c("Chisq", "Df", "Prob")
  xnames <- object[[1]]$names
  result = list(table = table, names = xnames, stratum = "ALL")
  class(result) <- "CMHtest"
  result
}


# basic CMH calculation
cmh <- function(n, m, A, V, df) {
  AVA <- A %*% V %*% t(A)
  Q <- t(n - m) %*% t(A) %*% MASS::ginv(AVA) %*% A %*% (n - m)
  pvalue <- pchisq(Q, df, lower.tail = FALSE)
  c(Q, df, pvalue)
}

# DONE: incorporate stratum name in the heading
# TODO: handle the printing of pvalues better

#' @rdname CMHtest
#' @export
print.CMHtest <- function(x, digits = max(getOption("digits") - 2, 3), ...) {
  heading <- "Cochran-Mantel-Haenszel Statistics"
  if (!is.null(x$names)) {
    heading <- paste(heading, "for", paste(x$names, collapse = " by "))
  }
  if (!is.null(x$stratum)) {
    heading <- paste(
      heading,
      ifelse(
        x$stratum == "ALL",
        "\n\tOverall tests, controlling for all strata",
        paste("\n\tin stratum", x$stratum)
      )
    )
  }
  # TODO: determine score types (integer, midrank) for heading

  df <- x$table
  types <- rownames(df)
  labels <- list(
    cor = "Nonzero correlation",
    rmeans = "Row mean scores differ",
    cmeans = "Col mean scores differ",
    general = "General association"
  )
  labels <- unlist(labels[types]) # select the labels for the types
  df <- data.frame(
    "AltHypothesis" = as.character(labels),
    df,
    stringsAsFactors = FALSE
  )
  cat(heading, "\n\n")
  print(df, digits = digits, ...)
  cat("\n")

  invisible(x)
}
