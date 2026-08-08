# Illustrates a real bug in dev/loghistplot2.R's logist_plot(): the roxygen docs claim
# `y` can be "0/1, or a 2-level factor/character/logical", but only numeric 0/1 actually
# works. Run this script top to bottom to see exactly where and why the others break.
#
# Root cause: ggplot2 treats any non-numeric y (factor, character, logical) as *discrete*.
# `aes(y = .data$y)` sets no explicit `group=`, so ggplot2's implicit grouping splits
# stat_smooth()'s calculation into one glm() fit PER DISTINCT y VALUE. Each such subset has
# a *constant* y, which glm(family = binomial) rejects ("y values must be 0 <= y <= 1").
#
# A `ggplot` object is lazy: this only surfaces when the plot is actually rendered
# (ggplot_build()/print()/ggsave()), not when logist_plot() merely constructs it -- so a
# test that only checks `inherits(p, "ggplot")` (as in dev/loghist-test-basic-run.R-style
# smoke tests) will NOT catch this. Every case below forces a real render.

source(here::here("dev", "loghistplot2.R"))  # here package is already a declared dependency
library(vcdExtra)
data(Donner, package = "vcdExtra")

x <- Donner$age
y_numeric   <- Donner$survived                                  # 0/1 integer -- the only
                                                                  # type that currently works
y_factor    <- factor(Donner$survived, labels = c("died", "survived"))
y_character <- as.character(y_factor)
y_logical   <- as.logical(Donner$survived)

variants <- list(numeric = y_numeric, factor = y_factor,
                  character = y_character, logical = y_logical)

# Forces actual rendering (ggplot_build), which is where geom_smooth()'s internal glm()
# fit runs -- the step that lazy construction skips.
render_check <- function(label, y, marginal) {
  cat("\n---", label, "/ marginal =", marginal, "---\n")
  cat("class(y):", paste(class(y), collapse = ", "), "\n")
  p <- tryCatch(logist_plot(x, y, marginal = marginal),
                error = function(e) { cat("CONSTRUCT ERROR:", conditionMessage(e), "\n"); NULL })
  if (is.null(p)) return(invisible())
  tryCatch(
    { ggplot2::ggplot_build(p); cat("RENDER: OK\n") },
    error   = function(e) cat("RENDER ERROR:",   conditionMessage(e), "\n"),
    warning = function(w) cat("RENDER WARNING:", conditionMessage(w), "\n")
  )
}

for (nm in names(variants)) {
  render_check(nm, variants[[nm]], "points")
  render_check(nm, variants[[nm]], "hist")
}

# Expected results (confirmed 2026-08-08, ggplot2 3.6.x / vcdExtra dev/loghistplot2.R):
#
#   numeric,   points -> RENDER: OK
#   numeric,   hist    -> RENDER: OK
#   factor,    points -> RENDER WARNING: Failed to fit group 2 / y values must be 0 <= y <= 1
#   factor,    hist    -> CONSTRUCT ERROR: Discrete value supplied to a continuous scale
#   character, points -> RENDER WARNING: Failed to fit group 2 / y values must be 0 <= y <= 1
#   character, hist    -> CONSTRUCT ERROR: Discrete value supplied to a continuous scale
#   logical,   points -> RENDER WARNING: Failed to fit group 2 / y values must be 0 <= y <= 1
#   logical,   hist    -> RENDER: OK  (misleading -- scale_y_continuous() happens to tolerate
#                          logical->0/1 coercion, but the same implicit-grouping problem that
#                          breaks "points" mode is still present in the shared p_main smooth
#                          layer; it's very likely being swallowed inside cowplot's grob
#                          capture rather than genuinely absent. Don't treat "hist" + logical
#                          as safe on the strength of this result alone.)
#
# Fix (not yet applied here -- see dev/loghistplot2.R review notes): convert y to numeric
# 0/1 up front in .logist_plot_impl(), right after .check_binary_y(), instead of passing
# the raw factor/character/logical vector through to ggplot(). This also ties into the
# separate event-direction/ordering issue noted in loghistplot2.R (.check_binary_y() uses
# unique()-encounter order, so which level maps to 0 vs. 1 isn't currently controlled).
