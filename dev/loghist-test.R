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

# ---- mirrored-histogram direction demo -----------------------------------------------------
#
# Former review item (see dev/loghistplot2.R review notes, now FIXED): inside
# .logist_plot_impl(), marginal = "hist" builds two panels via a local
# marginal_hist(lev, reverse) closure:
#   p_hist_y0 <- marginal_hist(uy[1]=0, reverse = FALSE)   # plain, non-reversed y scale
#   p_hist_y1 <- marginal_hist(uy[2]=1, reverse = TRUE)    # scale_y_reverse()
#
# The logic was always correct -- the y=0 group is anchored at the BOTTOM of the panel,
# growing upward (y = 0 is at the bottom on a normal scale), and the y=1 group is anchored
# at the TOP, growing downward (scale_y_reverse() flips which end is "up") -- but the
# variables were originally named p_top/p_bottom, which was backwards from that (p_top was
# the bottom-growing one). Renamed to p_hist_y0/p_hist_y1 (named by response group, not
# assumed screen position) to fix the confusion. This is invisible in normal use either way,
# because both panels' axes are made transparent and overlaid full-size via
# cowplot::draw_plot() -- you only ever see the histogram bars, never which internal
# variable produced which one. It only matters to someone reading/editing the code.
#
# This block reconstructs both panels with their axes left VISIBLE (the real code hides
# them) so the mirrored direction can actually be seen, and writes them side by side to a
# PNG, confirming the rendered behavior still matches the intended design after the rename.

.demo_top_bottom_direction <- function(out_file = here::here("dev", "loghist-top-bottom-demo.png")) {
  data <- data.frame(x = x, y = y_numeric)
  bin <- .to_binary01(data$y)
  data$y <- bin$y01
  uy <- c(0, 1)

  bins <- 30
  min_x <- min(data$x); max_x <- max(data$x)
  bin_width <- (max_x - min_x) / bins
  hist_breaks <- seq(min_x, max_x, length.out = bins + 1)
  hist_counts <- lapply(uy, function(lev) {
    graphics::hist(data$x[data$y == lev], breaks = hist_breaks, right = FALSE,
                    include.lowest = TRUE, plot = FALSE)$counts
  })
  bin_no <- 4 * max(unlist(hist_counts))

  # Same construction as marginal_hist() in .logist_plot_impl(), but with visible
  # axes/border/background instead of transparent ones.
  panel <- function(lev, reverse, fill) {
    p <- ggplot2::ggplot(data[data$y == lev, ], ggplot2::aes(x = .data$x)) +
      ggplot2::theme_bw(base_size = 14) +
      ggplot2::geom_histogram(fill = fill, binwidth = bin_width, boundary = min_x,
                               closed = "left", alpha = .8) +
      ggplot2::coord_cartesian(xlim = c(min_x, max_x)) +
      ggplot2::labs(x = "age", y = "count (unreversed scale)")
    if (reverse) {
      p + ggplot2::scale_y_reverse(limits = c(bin_no, 0)) +
        ggplot2::labs(y = "count (REVERSED scale)")
    } else {
      p + ggplot2::scale_y_continuous(limits = c(0, bin_no))
    }
  }

  p_hist_y0 <- panel(uy[1], reverse = FALSE, fill = "steelblue") +
    ggplot2::ggtitle("p_hist_y0\nmarginal_hist(uy[1]=0, reverse = FALSE)")
  p_hist_y1 <- panel(uy[2], reverse = TRUE, fill = "tomato") +
    ggplot2::ggtitle("p_hist_y1\nmarginal_hist(uy[2]=1, reverse = TRUE)")

  combined <- cowplot::plot_grid(p_hist_y0, p_hist_y1, ncol = 2)
  print(combined)  # shows in the interactive graphics device when run top to bottom
  ggplot2::ggsave(out_file, combined, width = 10, height = 5, dpi = 110)
  cat("\nWrote", out_file, "-- p_hist_y0's bars sit on the bottom axis and grow up;",
      "p_hist_y1's bars hang from the top axis and grow down.\n")
  invisible(combined)
}

.demo_top_bottom_direction()
