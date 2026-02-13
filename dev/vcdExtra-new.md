# vcdExtra: New work
#

One important step: Illustrate new developments in vignettes

# labeling points

Described in `dev/labeling_points-plan.md` and implemented. It would be useful to have vignette describing the
background for showing frequencies as dot-densities in mosaics and related displays.

* Use a couple of simple examples (`HairEyeColor` most natural)
* Show mosaics with size ~ {observed, expected} frequencies, with points ~ {observed, expected}

* This suggests another `vcd::shading()` function: `shading_marimekko()`. Not residual-based. It simply uses
a collection of distinct colors for the 2nd, 3rd, ... variables split to show the sub-categories.   This
is what is done in the {ggmosaic} package. 

The current {vcd} implementation is contained in "C:\Dropbox\R\packages\vcd\R\shadings.R"
This would be useful for mosaic plots only showing the successive splits by factors,
combined with other enhancements like `labeling_points()`
