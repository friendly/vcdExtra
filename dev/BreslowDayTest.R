# Source: https://github.com/AndriSignorell/DescTools/blob/master/R/Tests.r
# Lines 2912-2973 (approximately)
# Original author: Michael Hoehle <http://www.math.su.se/~hoehle>
# Adapted/packaged by Andri Signorell in DescTools

BreslowDayTest <- function(x, OR = NA, correct = FALSE) {

  # Function to perform the Breslow and Day (1980) test including the
  # corrected test by Tarone. Uses the equations in Lachin (2000),
  # Biostatistical Methods, Wiley, p. 124-125.
  #
  # Programmed by Michael Hoehle <http://www.math.su.se/~hoehle>
  # Code taken originally from a Biostatistical Methods lecture
  # held at the Technical University of Munich in 2008.
  #
  # Params:
  #  x       - a 2x2xK contingency table
  #  OR      - common odds ratio to test against (default: Mantel-Haenszel estimate)
  #  correct - if TRUE Tarone's correction is returned
  #
  # Returns: an "htest" object with:
  #   statistic - Breslow-Day (or Tarone-corrected) test statistic
  #   parameter - degrees of freedom (K - 1)
  #   p.value   - p-value from chi-squared distribution

  if (is.na(OR)) {
    # Find the common OR based on Mantel-Haenszel
    or.hat.mh <- mantelhaen.test(x)$estimate
  } else {
    or.hat.mh <- OR
  }

  # Number of strata
  K <- dim(x)[3]
  # Value of the statistic
  X2.HBD <- 0
  # Value of aj, tildeaj and Var.aj
  a <- tildea <- Var.a <- numeric(K)

  for (j in 1:K) {
    # Find marginals of table j
    mj <- apply(x[, , j], MARGIN = 1, sum)
    nj <- apply(x[, , j], MARGIN = 2, sum)

    # Solve for tilde(a)_j
    # Quadratic: -m1*n1*OR * a^2 + (n2-m1+OR*(n1+m1)) * a + (1-OR) = 0
    coef <- c(-mj[1] * nj[1] * or.hat.mh,
              nj[2] - mj[1] + or.hat.mh * (nj[1] + mj[1]),
              1 - or.hat.mh)
    sols <- Re(polyroot(coef))
    # Take the root which fulfills 0 < tilde(a)_j <= min(n1_j, m1_j)
    tildeaj <- sols[(0 < sols) & (sols <= min(nj[1], mj[1]))]
    # Observed value
    aj <- x[1, 1, j]

    # Determine other expected cell entries
    tildebj <- mj[1] - tildeaj
    tildecj <- nj[1] - tildeaj
    tildedj <- mj[2] - tildecj

    # Compute Var(a_j | OR_MH)
    Var.aj <- (1/tildeaj + 1/tildebj + 1/tildecj + 1/tildedj)^(-1)

    # Accumulate contribution to statistic
    X2.HBD <- X2.HBD + as.numeric((aj - tildeaj)^2 / Var.aj)

    a[j] <- aj
    tildea[j] <- tildeaj
    Var.a[j] <- Var.aj
  }

  # Tarone-corrected test statistic
  # Note: corrected 2015 — original formula used Var.aj (last stratum only) not Var.a (all strata)
  # See: Jean-Francois Bouzereau correction
  X2.HBDT <- as.numeric(X2.HBD - (sum(a) - sum(tildea))^2 / sum(Var.a))

  DNAME <- deparse(substitute(x))
  STATISTIC <- if (correct) X2.HBDT else X2.HBD
  PARAMETER <- K - 1
  PVAL <- 1 - pchisq(STATISTIC, PARAMETER)
  METHOD <- if (correct)
    "Breslow-Day Test on Homogeneity of Odds Ratios (with Tarone correction)"
  else
    "Breslow-Day test on Homogeneity of Odds Ratios"

  names(STATISTIC) <- "X-squared"
  names(PARAMETER) <- "df"
  structure(list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 method = METHOD,
                 data.name = DNAME),
            class = "htest")
}
