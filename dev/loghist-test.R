# Regression test for a bug in dev/loghistplot2.R's logist_plot(): the roxygen docs claim
# `y` can be "0/1, or a 2-level factor/character/logical", but originally only numeric 0/1
# actually worked. Run this script top to bottom to see the original failure mode and confirm
# the fix (.to_binary01(), added 2026-08-08) resolves it for all four types.
#
# Root cause: ggplot2 treats any non-numeric y (factor, character, logical) as *discrete*.
# `aes(y = .data$y)` sets no explicit `group=`, so ggplot2's implicit grouping used to split
# stat_smooth()'s calculation into one glm() fit PER DISTINCT y VALUE. Each such subset has
# a *constant* y, which glm(family = binomial) rejects ("y values must be 0 <= y <= 1").
# .to_binary01() now converts y to numeric 0/1 *before* it ever reaches ggplot(), so this
# never triggers.
#
# A `ggplot` object is lazy: rendering errors only surface when the plot is actually built
# (ggplot_build()/print()/ggsave()), not when logist_plot() merely constructs it -- so a test
# that only checks `inherits(p, "ggplot")` will NOT catch this. Every case below forces a
# real render.

source(here::here("dev", "loghistplot2.R"))  # here package is already a declared dependency
library(vcdExtra)
data(Donner, package = "vcdExtra")

x <- Donner$age
y_numeric   <- Donner$survived                                  # 0/1 integer
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

# Expected: RENDER: OK for all 8 cases above (confirmed 2026-08-08, ggplot2 3.6.x).

# Also confirm *which* level each type maps to 0 vs. 1 -- deterministic, not row-order
# dependent (unlike the old .check_binary_y(), which used unique()-encounter order).
cat("\n\n--- .to_binary01() level mapping (0 = first, 1 = second) ---\n")
for (nm in names(variants)) {
  bin <- .to_binary01(variants[[nm]])
  cat(sprintf("%-9s levels: %s\n", nm, paste(bin$levels, collapse = " -> ")))
}
# Expected: numeric "0 -> 1" (pass-through); factor/character/logical all agree that
# "died"/FALSE/0 -> 0 and "survived"/TRUE/1 -> 1, i.e. all four types encode the *same*
# event direction for this dataset.
