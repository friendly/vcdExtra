data(Donner, package = "vcdExtra")

# Small helpers -------------------------------------------------------------------------------

# geom_rect layer is used for histogram mode regardless of layer position, so locate it by
# geom class rather than a hardcoded index.
rect_layer_data <- function(p) {
  is_rect <- vapply(p$layers, function(l) inherits(l$geom, "GeomRect"), logical(1))
  ggplot2::layer_data(p, which(is_rect)[1L])
}

expect_built <- function(p) {
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
}

# ---- three calling conventions + convenience wrappers --------------------------------------

test_that("vector, data.frame, and formula interfaces all build", {
  expect_built(logist_plot(Donner$age, Donner$survived, marginal = "hist"))
  expect_built(logist_plot(Donner[, c("age", "survived")], marginal = "points"))
  expect_built(logist_plot(survived ~ age, data = Donner, marginal = "hist"))
})

test_that("data.frame method resolves xvar/yvar by name or position", {
  p_name <- logist_plot(Donner, xvar = "age", yvar = "survived", marginal = "hist")
  p_pos  <- logist_plot(Donner[, c("age", "survived")], xvar = 1L, yvar = 2L, marginal = "hist")
  expect_built(p_name)
  expect_built(p_pos)
})

test_that("named formula dispatch works, matching boxplot()/lm() convention", {
  expect_built(logist_plot(formula = survived ~ age, data = Donner))
})

test_that("convenience wrappers fix marginal= but keep all three interfaces", {
  expect_built(logist_hist(survived ~ age, data = Donner))
  expect_built(logist_point(survived ~ age, data = Donner))
  expect_built(logist_density(survived ~ age, data = Donner))
  expect_built(logist_hist(Donner$age, Donner$survived))
  expect_built(logist_point(Donner[, c("age", "survived")]))
})

test_that("formula with more than one predictor is rejected", {
  expect_error(
    logist_plot(survived ~ age + sex, data = Donner),
    "exactly one response and one predictor"
  )
})

test_that("data.frame method requires at least 2 columns", {
  expect_error(
    logist_plot(Donner["age"], marginal = "hist"),
    "at least 2 columns"
  )
})

test_that("unknown xvar/yvar selectors error clearly", {
  expect_error(logist_plot(Donner, xvar = "nope", yvar = "survived"), "does not identify")
  expect_error(logist_plot(Donner, xvar = "age", yvar = 99L), "whole number between 1 and")
})

# ---- marginal modes ---------------------------------------------------------------------

test_that("all three marginal modes build and render", {
  expect_built(logist_plot(survived ~ age, data = Donner, marginal = "hist"))
  expect_built(logist_plot(survived ~ age, data = Donner, marginal = "points"))
  expect_built(logist_plot(survived ~ age, data = Donner, marginal = "density"))
})

test_that("marginal defaults to \"hist\" when omitted", {
  p1 <- logist_plot(survived ~ age, data = Donner)
  p2 <- logist_plot(survived ~ age, data = Donner, marginal = "hist")
  expect_equal(ggplot2::layer_data(p1), ggplot2::layer_data(p2))
})

# ---- accepted binary-response encodings -----------------------------------------------------

test_that("numeric, logical, factor, and character y all build", {
  y_num  <- Donner$survived
  y_log  <- as.logical(Donner$survived)
  y_fac  <- factor(Donner$survived, labels = c("died", "survived"))
  y_char <- ifelse(Donner$survived == 1, "survived", "died")

  for (y in list(y_num, y_log, y_fac, y_char)) {
    expect_built(logist_plot(Donner$age, y, marginal = "hist"))
    expect_built(logist_plot(Donner$age, y, marginal = "points"))
  }
})

test_that("numeric y must be coded 0/1", {
  expect_error(
    logist_plot(1:10, rep(c(1, 2), 5)),
    "must be coded 0/1"
  )
})

test_that("y must be binary (exactly 2 distinct values)", {
  expect_error(
    logist_plot(1:9, rep(c(0, 1, 2), 3)),
    "must be binary"
  )
})

test_that("non-numeric/list/matrix x and y are rejected", {
  expect_error(logist_plot(as.character(1:10), rep(0:1, 5)), "`x` must be numeric")
  expect_error(logist_plot(1:10, list(1:10)), "plain atomic vector")
  expect_error(logist_plot(matrix(1:10, 5), rep(0:1, 5)), "matrix/array/data.frame")
})

test_that("mismatched x/y lengths are rejected", {
  expect_error(logist_plot(1:10, rep(0:1, 4)), "same length")
})

test_that("zero-range x is rejected", {
  expect_error(logist_plot(rep(1, 10), rep(0:1, 5)), "zero range")
})

test_that("NA/Inf x rows are dropped consistently across calling conventions", {
  x <- c(Donner$age, NA, Inf)
  y <- c(Donner$survived, 1, 0)
  p_vec <- logist_plot(x, y, marginal = "hist")
  p_form <- logist_plot(survived ~ age, data = Donner, marginal = "hist")
  expect_equal(nrow(ggplot2::ggplot_build(p_vec)$data[[1]]),
               nrow(ggplot2::ggplot_build(p_form)$data[[1]]))
})

# ---- grouping ---------------------------------------------------------------------------

test_that("group= builds for points and density but errors for hist", {
  expect_built(logist_plot(survived ~ age, data = Donner, group = "sex", marginal = "points"))
  expect_built(logist_plot(survived ~ age, data = Donner, group = "sex", marginal = "density"))
  expect_error(
    logist_plot(survived ~ age, data = Donner, group = "sex", marginal = "hist"),
    "Grouping is not supported"
  )
})

test_that("group order is stable regardless of row order", {
  set.seed(1)
  shuffled <- Donner[sample(nrow(Donner)), ]
  p1 <- logist_plot(survived ~ age, data = Donner, group = "sex", marginal = "points")
  p2 <- logist_plot(survived ~ age, data = shuffled, group = "sex", marginal = "points")
  expect_identical(levels(ggplot2::ggplot_build(p1)$plot$data$group),
                    levels(ggplot2::ggplot_build(p2)$plot$data$group))
})

test_that("every group must contain both response outcomes", {
  bad <- data.frame(
    x = c(1, 2, 3, 4, 5, 6), y = c(0, 1, 0, 1, 1, 1),
    g = c("a", "a", "a", "b", "b", "b")
  )
  expect_error(
    logist_plot(y ~ x, data = bad, group = "g", marginal = "points"),
    "must contain both response outcomes"
  )
})

test_that("every group must have at least 2 distinct x values", {
  bad <- data.frame(
    x = c(1, 1, 1, 2, 3, 4), y = c(0, 1, 0, 1, 0, 1),
    g = c("a", "a", "a", "b", "b", "b")
  )
  expect_error(
    logist_plot(y ~ x, data = bad, group = "g", marginal = "points"),
    "at least 2 distinct predictor values"
  )
})

test_that("group.colors validates against observed group levels", {
  expect_error(
    logist_plot(survived ~ age, data = Donner, group = "sex", marginal = "points",
                group.colors = "red"),
    "must provide at least"
  )
  expect_error(
    logist_plot(survived ~ age, data = Donner, group = "sex", marginal = "points",
                group.colors = c(Female = "red")),
    "missing colour"
  )
  expect_error(
    logist_plot(survived ~ age, data = Donner, group = "sex", marginal = "points",
                group.colors = c("not-a-colour", "red")),
    "Invalid colour"
  )
  expect_built(
    logist_plot(survived ~ age, data = Donner, group = "sex", marginal = "points",
                group.colors = c(Female = "purple", Male = "darkgreen"))
  )
})

test_that("group.colors requires a non-NULL group", {
  expect_error(
    logist_plot(survived ~ age, data = Donner, group.colors = c("red", "blue")),
    "requires a non-NULL"
  )
})

test_that("grouped plots reject colour overrides through fit.args/marginal.args", {
  expect_error(
    logist_plot(survived ~ age, data = Donner, group = "sex", marginal = "points",
                fit.args = list(colour = "black")),
    "map fit colours to `group`"
  )
  expect_error(
    logist_plot(survived ~ age, data = Donner, group = "sex", marginal = "points",
                marginal.args = list(fill = "black")),
    "map marginal colours to `group`"
  )
})

# ---- marginal.height --------------------------------------------------------------------

test_that("default marginal.height reproduces the documented per-mode defaults", {
  p_hist_default <- logist_plot(survived ~ age, data = Donner, marginal = "hist")
  p_hist_explicit <- logist_plot(survived ~ age, data = Donner, marginal = "hist",
                                  marginal.height = 0.25)
  expect_equal(rect_layer_data(p_hist_default), rect_layer_data(p_hist_explicit))

  p_dens_default <- logist_plot(survived ~ age, data = Donner, marginal = "density")
  p_dens_explicit <- logist_plot(survived ~ age, data = Donner, marginal = "density",
                                  marginal.height = 0.15)
  expect_equal(ggplot2::layer_data(p_dens_default), ggplot2::layer_data(p_dens_explicit))
})

test_that("a custom marginal.height changes the rendered extents", {
  p_small <- logist_plot(survived ~ age, data = Donner, marginal = "hist",
                          marginal.height = 0.05)
  p_large <- logist_plot(survived ~ age, data = Donner, marginal = "hist",
                          marginal.height = 0.4)
  small_max <- max(abs(rect_layer_data(p_small)$ymax - rect_layer_data(p_small)$ymin))
  large_max <- max(abs(rect_layer_data(p_large)$ymax - rect_layer_data(p_large)$ymin))
  expect_lt(small_max, large_max)
})

test_that("marginal.height validation rejects bad values", {
  expect_error(
    logist_plot(survived ~ age, data = Donner, marginal.height = -1),
    "finite positive number"
  )
  expect_error(
    logist_plot(survived ~ age, data = Donner, marginal.height = 0.9, marginal = "hist"),
    "cannot exceed 0.5"
  )
  expect_error(
    logist_plot(survived ~ age, data = Donner, group = "sex", marginal = "density",
                marginal.height = 1.5),
    "cannot exceed 1"
  )
})

test_that("marginal.height is ignored (with a warning) for marginal = \"points\"", {
  expect_warning(
    logist_plot(survived ~ age, data = Donner, marginal = "points", marginal.height = 0.3),
    "ignored for `marginal = \"points\"`"
  )
})

test_that("grouped density stays clear of the panel border at extreme lane heights", {
  expect_built(
    logist_plot(survived ~ age, data = Donner, group = "sex", marginal = "density",
                marginal.height = 0.01)
  )
  expect_built(
    logist_plot(survived ~ age, data = Donner, group = "sex", marginal = "density",
                marginal.height = 0.99)
  )
})

test_that("marginal.height cannot be smuggled through marginal.args", {
  expect_error(
    logist_plot(survived ~ age, data = Donner, marginal = "hist",
                marginal.args = list(height = 0.1)),
    "Supply `marginal.height` as a top-level argument"
  )
})

test_that("histogram count-axis ticks stay symmetric and thin out at small heights", {
  p <- logist_plot(survived ~ age, data = Donner, marginal = "hist", marginal.height = 0.03)
  built <- ggplot2::ggplot_build(p)
  breaks <- built$layout$panel_params[[1]]$y$scale$secondary.axis$breaks
  breaks <- breaks[is.finite(breaks)]
  # symmetric around 0.5 (mirrored histogram)
  expect_equal(sort(breaks), sort(1 - breaks), tolerance = 1e-6)
  # no two adjacent labels closer than the documented minimum spacing
  expect_true(all(diff(sort(breaks)) >= 0.05 - 1e-6))
})

# ---- fit.args / marginal.args forwarding ----------------------------------------------------

test_that("fit.color and marginal.color retain their existing behavior", {
  p <- logist_plot(survived ~ age, data = Donner, marginal = "points",
                    fit.color = "navy", marginal.color = "goldenrod")
  built <- ggplot2::layer_data(p, 1)
  expect_true(all(built$colour %in% c("navy") | built$fill %in% c("navy")))
})

test_that("fit.args overrides fit.color for the same aesthetic", {
  p <- logist_plot(survived ~ age, data = Donner, marginal = "points",
                    fit.color = "navy", fit.args = list(colour = "red"))
  fit_data <- ggplot2::layer_data(p, 1)
  expect_true(all(fit_data$colour == "red"))
})

test_that("marginal.args overrides marginal.color for the same aesthetic", {
  p <- logist_plot(survived ~ age, data = Donner, marginal = "hist",
                    marginal.color = "goldenrod", marginal.args = list(fill = "grey70"))
  expect_true(all(rect_layer_data(p)$fill == "grey70"))
})

test_that("point shape/size/alpha/jitter are customizable via marginal.args", {
  p <- logist_plot(
    survived ~ age, data = Donner, marginal = "points",
    marginal.args = list(shape = 21, size = 3, alpha = 0.9,
                          position = ggplot2::position_jitter(h = 0.1))
  )
  expect_built(p)
})

test_that("histogram fill/outline/linewidth/linetype/alpha are customizable", {
  p <- logist_plot(
    survived ~ age, data = Donner, marginal = "hist",
    marginal.args = list(colour = "black", linewidth = 0.2, linetype = "dashed", alpha = 0.8)
  )
  expect_built(p)
})

test_that("color/colour alias is deduplicated deterministically", {
  p <- logist_plot(survived ~ age, data = Donner, marginal = "points",
                    marginal.args = list(color = "purple"))
  expect_true(all(ggplot2::layer_data(p, 2)$colour == "purple"))
  expect_error(
    logist_plot(survived ~ age, data = Donner, marginal = "points",
                marginal.args = list(color = "purple", colour = "green")),
    "conflicting `color` and `colour`"
  )
})

test_that("protected arguments are rejected with a clear error", {
  expect_error(
    logist_plot(survived ~ age, data = Donner, fit.args = list(data = Donner)),
    "cannot replace protected"
  )
  expect_error(
    logist_plot(survived ~ age, data = Donner, marginal = "hist",
                marginal.args = list(binwidth = 1)),
    "cannot replace protected"
  )
})

test_that("unsupported arguments are rejected per marginal mode", {
  expect_error(
    logist_plot(survived ~ age, data = Donner, marginal = "points",
                marginal.args = list(binwidth = 1)),
    "Unsupported argument"
  )
})

test_that("unnamed, duplicated, or non-list layer args are rejected", {
  expect_error(
    logist_plot(survived ~ age, data = Donner, fit.args = list(1, 2)),
    "must be named"
  )
  expect_error(
    logist_plot(survived ~ age, data = Donner, fit.args = "not a list"),
    "must be a named list"
  )
  expect_error(
    logist_plot(survived ~ age, data = Donner, fit.args = list(alpha = 0.5, alpha = 0.9)),
    "duplicate argument name"
  )
})

test_that("adjust cannot be smuggled through marginal.args", {
  expect_error(
    logist_plot(survived ~ age, data = Donner, marginal = "density",
                marginal.args = list(adjust = 2)),
    "Supply `adjust` as a top-level argument"
  )
})

test_that("misspelled top-level arguments still fail via check_dots_empty()", {
  expect_error(
    logist_plot(survived ~ age, data = Donner, fitcolor = "red"),
    "unused argument|fitcolor"
  )
})

test_that("all marginal modes still build after graphical customization", {
  expect_built(logist_plot(survived ~ age, data = Donner, marginal = "hist",
                            marginal.args = list(alpha = 0.5)))
  expect_built(logist_plot(survived ~ age, data = Donner, marginal = "points",
                            marginal.args = list(alpha = 0.5)))
  expect_built(logist_plot(survived ~ age, data = Donner, marginal = "density",
                            marginal.args = list(alpha = 0.5)))
})

# ---- adjust (density bandwidth) ----------------------------------------------------------

test_that("adjust must be one finite positive number", {
  expect_error(logist_plot(survived ~ age, data = Donner, marginal = "density", adjust = -1),
               "finite positive number")
})

test_that("adjust changes the rendered density curve", {
  p1 <- logist_plot(survived ~ age, data = Donner, marginal = "density", adjust = 0.3)
  p2 <- logist_plot(survived ~ age, data = Donner, marginal = "density", adjust = 3)
  expect_false(isTRUE(all.equal(ggplot2::layer_data(p1, 1), ggplot2::layer_data(p2, 1))))
})

# ---- bins -----------------------------------------------------------------------------------

test_that("bins must be one positive whole number", {
  expect_error(logist_plot(survived ~ age, data = Donner, bins = 0), "positive whole number")
  expect_error(logist_plot(survived ~ age, data = Donner, bins = 2.5), "positive whole number")
})

test_that("too many bins for the data range errors clearly", {
  expect_error(
    logist_plot(c(1, 1 + .Machine$double.eps), c(0, 1), bins = 2),
    "too small relative to"
  )
})
