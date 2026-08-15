# Test/exploration script backing dev/loghistplot-extensions.md's second section: a third
# marginal= option, "density", and whether ggdist could replace cowplot for the mirrored
# compositing. Not part of the package; nothing here is wired into logist_plot() yet.
#
# Versions this was run against: R 4.6.1, ggplot2 4.0.3, ggdist 3.3.3, cowplot 1.2.0.

library(vcdExtra); library(ggplot2)
data(Donner, package = "vcdExtra")
d <- Donner[, c("age", "survived")]
min_x <- min(d$age); max_x <- max(d$age)
out <- function(f) here::here("dev", f)  # write outputs alongside this script

# =====================================================================================
# 1. marginal = "density" via cowplot -- same architecture as the existing marginal =
#    "hist", geom_density() standing in for geom_histogram(). Works cleanly.
# =====================================================================================

# Headroom scaling is required, same as bin_no <- 4 * max_count in the real hist code --
# without it, each density panel's own [0, max_density] range gets stretched to fill the
# FULL [0,1] canvas by cowplot::draw_plot(), ballooning the fill to cover the whole plot.
dens0 <- density(d$age[d$survived == 0])
dens1 <- density(d$age[d$survived == 1])
headroom <- 4 * max(dens0$y, dens1$y)  # same "4x" convention as the histogram's bin_no

mk_density_panel <- function(lev, reverse, fill) {
  sub <- d[d$survived == lev, ]
  p <- ggplot(sub, aes(x = age)) +
    theme_bw(base_size = 14) +
    geom_density(fill = fill, colour = NA, alpha = .7) +
    coord_cartesian(xlim = c(min_x, max_x)) +
    theme(axis.text = element_text(colour = "transparent"),
          axis.ticks = element_line(colour = "transparent"),
          axis.title = element_text(colour = "transparent"),
          panel.border = element_blank(), panel.background = element_blank(),
          plot.background = element_blank(), panel.grid = element_blank())
  if (reverse) {
    p + scale_y_reverse(limits = c(headroom, 0), expand = expansion(mult = 0))
  } else {
    p + scale_y_continuous(limits = c(0, headroom), expand = expansion(mult = 0))
  }
}
p_dens0 <- mk_density_panel(0, reverse = FALSE, fill = "orange")
p_dens1 <- mk_density_panel(1, reverse = TRUE,  fill = "orange")

# NOTE the transparent panel/plot background on p_main below -- forgetting this the first
# time around made p_dens0/p_dens1 invisible (p_main, drawn last, opaquely covered them).
p_main <- ggplot(d, aes(x = age, y = survived)) +
  theme_bw(base_size = 14) +
  geom_smooth(method = "glm", formula = y ~ x, method.args = list(family = "binomial"),
              se = TRUE, colour = "steelblue", fill = "steelblue") +
  coord_cartesian(xlim = c(min_x, max_x), ylim = c(0, 1)) +
  theme(panel.background = element_blank(), plot.background = element_blank()) +
  labs(y = "survived", x = "age")

density_cowplot <- cowplot::ggdraw() + cowplot::draw_plot(p_dens0) +
  cowplot::draw_plot(p_dens1) + cowplot::draw_plot(p_main)
ggsave(out("loghist-density-cowplot.png"), density_cowplot, width = 6, height = 5, dpi = 120)
cat("1. density + cowplot: OK ->", out("loghist-density-cowplot.png"), "\n")
cat("   max_dens =", max(dens0$y, dens1$y), " headroom =", headroom, "\n")

# =====================================================================================
# 2. ggdist::stat_slab() attempts -- can it replace the cowplot compositing entirely,
#    as ONE ggplot object with no separate panels? side= solves the orientation/mirroring
#    half cleanly. Getting the fill to occupy a controlled fraction of the shared [0,1]
#    axis did not work in either attempt below.
# =====================================================================================

# --- 2a. naive: normalize = "none", tune scale=/height= directly ---
# Per ?ggdist::stat_slabinterval, normalize = "none" means "values are taken as is with
# no normalization (this should probably only be used with functions whose values are in
# [0,1])" -- our density() values are ~0.003-0.02, nowhere near [0,1], so this plots raw,
# tiny magnitudes directly as thickness. That's almost certainly why it renders flat.
p_ggdist_naive <- ggplot(d, aes(x = age)) +
  theme_bw(base_size = 14) +
  ggdist::stat_slab(data = subset(d, survived == 1), aes(y = 1), fill = "orange",
                     alpha = .7, side = "top", scale = 0.3, normalize = "none", height = 1) +
  ggdist::stat_slab(data = subset(d, survived == 0), aes(y = 0), fill = "orange",
                     alpha = .7, side = "bottom", scale = 0.3, normalize = "none", height = 1) +
  geom_smooth(data = d, aes(x = age, y = survived), method = "glm", formula = y ~ x,
              method.args = list(family = "binomial"), se = TRUE,
              colour = "steelblue", fill = "steelblue") +
  coord_cartesian(xlim = c(min_x, max_x), ylim = c(0, 1)) +
  labs(y = "survived", x = "age")
ggsave(out("loghist-ggdist-naive.png"), p_ggdist_naive, width = 6, height = 5, dpi = 120)
cat("2a. ggdist, normalize=\"none\": OK (renders flat) ->", out("loghist-ggdist-naive.png"), "\n")

# --- 2b. documented approach: normalize = "panels" (each slab's own max -> 1) +
#         ggdist::scale_thickness_shared(limits = c(0, K)) to control the output range.
#         This is the mechanism ggdist's own docs point to for aligning thickness across
#         layers -- and it still renders flat here. See the .md notes for why this seems
#         like a real question to raise with the ggdist maintainers, not just a tuning
#         mistake on our end (or at least: not an obviously-wrong one).
p_ggdist_scaled <- ggplot(d, aes(x = age)) +
  theme_bw(base_size = 14) +
  ggdist::stat_slab(data = subset(d, survived == 1), aes(y = 1), fill = "orange",
                     alpha = .7, side = "top", normalize = "panels") +
  ggdist::stat_slab(data = subset(d, survived == 0), aes(y = 0), fill = "orange",
                     alpha = .7, side = "bottom", normalize = "panels") +
  ggdist::scale_thickness_shared(limits = c(0, 4)) +
  geom_smooth(data = d, aes(x = age, y = survived), method = "glm", formula = y ~ x,
              method.args = list(family = "binomial"), se = TRUE,
              colour = "steelblue", fill = "steelblue") +
  coord_cartesian(xlim = c(min_x, max_x), ylim = c(0, 1)) +
  labs(y = "survived", x = "age")
ggsave(out("loghist-ggdist-scale-thickness.png"), p_ggdist_scaled, width = 6, height = 5, dpi = 120)
cat("2b. ggdist, scale_thickness_shared(): OK (still renders flat) ->",
    out("loghist-ggdist-scale-thickness.png"), "\n")

cat("\nDone. Compare loghist-density-cowplot.png (works) against the two ggdist attempts",
    "(both flat) -- see dev/loghistplot-extensions.md for the write-up and a possible",
    "question to file with ggdist about this.\n")
