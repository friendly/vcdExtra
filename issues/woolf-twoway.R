# Display the log odds ratios for a 2x2xRxC table using twoway::plot.twoway()
#
# Idea: woolf_test() on a 2x2xRxC table computes a log odds ratio for each
# of the R x C strata. Those RC log odds ratios form an ordinary two-way
# (R x C) table, so Tukey's median-polish / two-way fit machinery in the
# `twoway` package can be used to visualize row effects, column effects,
# and residuals in the log odds ratio -- entirely apart from the disputed
# ANOVA-style decompose=TRUE option in woolf_test() (see issues/woolf.md).
#
# As of 0.9.9, woolf_test() returns `LOR`/`LOR_se` directly (shaped as an
# R x C matrix for a 4-way table), and the exploration below has graduated
# into the package function `woolf_twoway()` (R/woolf_twoway.R) -- this
# script now just demonstrates using it.
#
# `twoway` is not (yet) a hard dependency of vcdExtra; install it separately:
# install.packages("twoway")

library(vcdExtra)
library(twoway)

data(Fungicide, package = "vcdExtra")

# Fungicide is a 2x2x2x2 array: group x outcome x sex x strain.
# The 2x2 table of interest is (group, outcome); the strata are
# (sex, strain), forming a 2 x 2 layout of odds ratios.
str(Fungicide)
dimnames(Fungicide)

# --- woolf_test() now exposes LOR / LOR_se directly -------------------------

wt <- woolf_test(Fungicide)
print(wt)

wt$LOR     # R x C matrix of log odds ratios (sex x strain)
wt$LOR_se  # matching matrix of standard errors

# --- Two-way (Tukey) decomposition of the log odds ratios, plain means -----
# (weighted = FALSE: every stratum's log odds ratio counted equally)

tw <- woolf_twoway(Fungicide, weighted = FALSE)
print(tw)

# Tukey additive-fit plot: row effect, column effect, and residuals for the
# log odds ratio of (group, outcome) across sex x strain strata
plot(tw, which = "fit")

# Diagnostic plot: is an additive model (in this scale) adequate, or does
# the pattern of residuals suggest a power transformation is needed?
plot(tw, which = "diagnose")

# --- Weighted (Woolf) mean-polish -------------------------------------------
#
# The plain mean-polish above gives every stratum's log odds ratio equal
# weight. But the log odds ratios are estimated with different precision in
# each stratum, so a Woolf-style analysis should weight each cell by its
# inverse variance 1 / LOR_se^2, exactly as woolf_test() itself does.
# woolf_twoway() does this by default (weighted = TRUE).

wtw <- woolf_twoway(Fungicide)
print(wtw)

# Compare row/column effects: weighted vs. plain mean-polish. For Fungicide
# the strata are similar in size, so the two are close; with more unequal
# sample sizes across strata the weighted effects would diverge more.
rbind(mean = tw$roweff, Woolf = wtw$roweff)
rbind(mean = tw$coleff, Woolf = wtw$coleff)

plot(wtw, which = "fit")
plot(wtw, which = "diagnose")
