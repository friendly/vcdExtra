# Test script for the multi-column stub enhancement to color_table()
# See dev/color_table/multi-col-stubs.md for the design note driving this.
#
# NOTE on terminology: this adds multi-column STUBS for the ROW variables
# (e.g. "Black_Male" -> separate "Hair"/"Sex" stub columns). Column SPANNERS
# for multi-variable COLUMNS already existed before this change -- see the
# "regression check" example below, which exercises that pre-existing
# feature to confirm it's undisturbed by the new row-stub code.
#
# This exercises the experimental version in dev/color_table/color_table.R,
# NOT the shipped R/color_table.R. To run:
#
#   devtools::load_all()                        # get gt/dplyr/vcd/MASS etc. + datasets
#   source("dev/color_table/color_table.R")      # override with the experimental version
#
# then run the examples below and inspect visually (this is a manual/visual
# test script, matching the style of dev/color_table/test-color_table.R --
# not a testthat unit test).

library(gt)
library(scales)
library(vcd)

data(HairEyeColor)
data(Titanic)

# ============================================================================
# Baseline: single row var, single col var -- should be unchanged from before
# ============================================================================

HEC <- margin.table(HairEyeColor, 1:2)  # Hair x Eye
color_table(HEC, title = "Baseline: single row var (Hair), single col var (Eye)")

# ============================================================================
# Regression check: multi *column* vars only -- column SPANNERS, a
# pre-existing feature (not part of this change). formula = Eye ~ Hair + Sex
# puts Hair/Sex on the columns. Confirms the new row-stub branch doesn't
# disturb the pre-existing col_vars spanner branch.
# ============================================================================

color_table(HairEyeColor,
            formula = Eye ~ Hair + Sex,
            legend  = TRUE,
            title   = "Regression check: pre-existing column spanners (Hair x Sex columns)")

# ============================================================================
# The actual TODO case: multi *row* vars -- formula = Hair + Sex ~ Eye.
# NEW in this change: row labels like "Black_Male" become two stub columns,
# "Hair" and "Sex", with repeated values blanked (gt's multi-column stub
# support, rowname_col = c("Hair", "Sex"), gt >= 0.11).
# ============================================================================

color_table(HairEyeColor,
            formula = Hair + Sex ~ Eye,
            legend  = TRUE,
            title   = "NEW: multi-row-var stub: Hair + Sex ~ Eye")

# Same, shaded by frequency instead of residuals
color_table(HairEyeColor,
            formula = Hair + Sex ~ Eye,
            shade   = "freq",
            title   = "NEW: multi-row-var stub, shade = freq")

# Same, with margins = FALSE -- confirms the Total-row branch is skipped
# cleanly (no stray "" padding row) when there's no Total row at all
color_table(HairEyeColor,
            formula = Hair + Sex ~ Eye,
            margins = FALSE,
            title   = "NEW: multi-row-var stub, margins = FALSE")

# Same, with values = "residuals" -- show_margins is forced FALSE internally
# even though margins defaults to TRUE; confirms that path is also clean
color_table(HairEyeColor,
            formula = Hair + Sex ~ Eye,
            values  = "residuals",
            title   = "NEW: multi-row-var stub, values = residuals")

# ============================================================================
# Stress test: NEW row stubs and pre-existing column spanners simultaneously,
# using Titanic (Class, Sex, Age, Survived) -- this is the combination
# referenced by the original TODO's pointer to
# dev/color-tab-figs/Titanic-residual-shading.png
# ============================================================================

color_table(Titanic,
            formula = Class + Sex ~ Age + Survived,
            legend  = TRUE,
            title   = "Combined: NEW row stub (Class x Sex) + pre-existing column spanner (Age x Survived)")

# ============================================================================
# 3+ row variables -- confirms the split/rebuild logic generalizes beyond 2
# ============================================================================

color_table(Titanic,
            formula = Class + Sex + Age ~ Survived,
            title   = "NEW: 3 row vars in stub: Class + Sex + Age ~ Survived")

# Same 3 variables, but on the columns instead -- exercises the pre-existing
# column-spanner code with 3 levels of spanner nesting (not new, but a good
# stress test since it wasn't tried with 3 column vars before)
color_table(Titanic,
            formula = Survived ~ Class + Sex + Age,
            title   = "Pre-existing: 3 col vars as spanners: Survived ~ Class + Sex + Age")
