# A collection of three-way and four-way examples for testing woolf_test(),
# beyond what's shown in ?woolf_test. Kept here (rather than in the roxygen
# @examples) because some of these are exploratory, illustrate open issues
# (see issues/woolf.md), or use datasets that need reshaping first.

library(vcdExtra)
library(twoway)

## ---- 3-way tables (2 x 2 x k) ---------------------------------------------

data(CoalMiners, package = "vcd")
woolf_test(CoalMiners)

data(Heart, package = "vcdExtra")
woolf_test(Heart)

# Bartlett (1935): the classic example for testing no three-way interaction /
# homogeneity of the odds ratio across strata
data(Bartlett, package = "vcdExtra")
woolf_test(Bartlett)

## ---- 4-way tables (2 x 2 x R x C), with decompose = TRUE ------------------

# Fungicide: R = C = 2 (sex x strain). Degenerate case -- only 1 residual df,
# so the row/col/residual split happens to come out non-negative here even
# though the decomposition is not generally valid (see issues/woolf.md).
data(Fungicide, package = "vcdExtra")
woolf_test(Fungicide)
woolf_test(Fungicide, decompose = TRUE)

# woolf_twoway() is a separate, valid WLS decomposition of the log odds
# ratios themselves (not a split of the woolf_test() statistic), so it is
# unaffected by the decompose = TRUE bug above.
tw_fungicide <- woolf_twoway(Fungicide)
print(tw_fungicide)
plot(tw_fungicide, which = "fit")
plot(tw_fungicide, which = "diagnose")

# Detergent: rearrange so (Preference, M_User) form the 2x2 table of interest,
# with (Temperature, Water_softness) as an unequal R x C = 2 x 3 layout of
# strata. This is a genuine counterexample to the decompose = TRUE claim:
# the residual statistic comes out NEGATIVE (a chi-squared statistic cannot
# be negative), confirming the bug described in issues/woolf.md. Kept here
# deliberately as a regression case for whenever decompose is reworked.
data(Detergent, package = "vcdExtra")
names(dimnames(Detergent))
Detergent <- aperm(Detergent, c(3, 2, 1, 4))
woolf_test(Detergent)
woolf_test(Detergent, decompose = TRUE)  # note the negative residual X-squared

# woolf_twoway() on the same table, unaffected by the bug above -- a real
# non-square (2 x 3) row/column decomposition of the log odds ratios
tw_detergent <- woolf_twoway(Detergent)
print(tw_detergent)
plot(tw_detergent, which = "fit")
plot(tw_detergent, which = "diagnose")

# DaytonSurvey: a 1992 survey of Dayton, Ohio high-school seniors on
# cigarette/alcohol/marijuana use, by sex and race (Agresti 2002, Table 9.1).
# Collapsing (summing) over marijuana gives a 2x2x2x2 table: does the
# cigarette:alcohol association vary by sex and race?
data(DaytonSurvey, package = "vcdExtra")
Dayton.tab <- xtabs(Freq ~ cigarette + alcohol + marijuana + sex + race,
                     data = DaytonSurvey)

# collapse over marijuana
Dayton4 <- apply(Dayton.tab, c("cigarette", "alcohol", "sex", "race"), sum)
Dayton4

woolf_test(Dayton4)
woolf_test(Dayton4, decompose = TRUE)

tw_dayton <- woolf_twoway(Dayton4)
print(tw_dayton)
plot(tw_dayton, which = "fit")
plot(tw_dayton, which = "diagnose")
