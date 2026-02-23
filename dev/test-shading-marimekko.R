# Test / demo script for shading_marimekko() and legend_marimekko()
# Run interactively after devtools::load_all()

library(vcd)
devtools::load_all()

# ---- 1. Basic 2-way table: Hair x Eye ----

HairEye <- margin.table(HairEyeColor, 1:2)

# Default: color by Eye (dim 2)
gp <- shading_marimekko(HairEye)
mosaic(HairEye, gp = gp, legend = FALSE,
       main = "HairEye: colored by Eye (dim 2)")
legend_marimekko(gp)

# Compare with vcd::shading_Marimekko (uppercase M)
mosaic(HairEye, gp = shading_Marimekko(HairEye),
       main = "HairEye: vcd::shading_Marimekko (no borders)")


# ---- 2. Color by different dimensions ----

# Color by Hair (dim 1)
gp1 <- shading_marimekko(HairEye, dim = 1)
mosaic(HairEye, gp = gp1, legend = FALSE,
       main = "HairEye: colored by Hair (dim 1)")
legend_marimekko(gp1)

# Equivalent using byrow = TRUE
gp_byrow <- shading_marimekko(HairEye, byrow = TRUE)
mosaic(HairEye, gp = gp_byrow, legend = FALSE,
       main = "HairEye: colored by Hair (byrow = TRUE)")
#legend_marimekko(gp_byrow)


# ---- 3. Custom palettes and border control ----

# Custom fill colors
gp_custom <- shading_marimekko(HairEye,
                                fill = c("pink", "lightblue", "lightgreen", "wheat"))
mosaic(HairEye, gp = gp_custom, legend = FALSE,
       main = "HairEye: custom palette")
#legend_marimekko(gp_custom)

# No borders (like vcd::shading_Marimekko)
gp_noborder <- shading_marimekko(HairEye, col = NA)
mosaic(HairEye, gp = gp_noborder, legend = FALSE,
       main = "HairEye: no borders (col = NA)")
legend_marimekko(gp_noborder)

# Custom fill function
gp_fn <- shading_marimekko(HairEye, fill = colorspace::sequential_hcl)
mosaic(HairEye, gp = gp_fn, legend = FALSE,
       main = "HairEye: sequential_hcl palette")
legend_marimekko(gp_fn)


# ---- 4. 3-way table: Hair x Eye x Sex ----

# Color by Eye (dim 2)
gp3_eye <- shading_marimekko(HairEyeColor, dim = 2)
mosaic(HairEyeColor, gp = gp3_eye, legend = FALSE,
       main = "HairEyeColor: colored by Eye (dim 2)")
legend_marimekko(gp3_eye)

# Color by Sex (dim 3)
gp3_sex <- shading_marimekko(HairEyeColor, dim = 3)
mosaic(HairEyeColor, gp = gp3_sex, legend = FALSE,
       main = "HairEyeColor: colored by Sex (dim 3)")
legend_marimekko(gp3_sex)

# Color by Hair (dim 1)
gp3_hair <- shading_marimekko(HairEyeColor, dim = 1)
mosaic(HairEyeColor, gp = gp3_hair, legend = FALSE,
       main = "HairEyeColor: colored by Hair (dim 1)")
legend_marimekko(gp3_hair)


# ---- 5. 4-way table: Titanic ----

# Color by Survived (dim 4)
gp_titanic <- shading_marimekko(Titanic, dim = 4)
mosaic(Titanic, gp = gp_titanic, legend = FALSE,
       main = "Titanic: colored by Survived (dim 4)")
legend_marimekko(gp_titanic)

# Color by Class (dim 1)
gp_class <- shading_marimekko(Titanic, dim = 1)
mosaic(Titanic, gp = gp_class, legend = FALSE,
       main = "Titanic: colored by Class (dim 1)")
legend_marimekko(gp_class)

# Color by Sex (dim 2)
gp_sex <- shading_marimekko(Titanic, dim = 2)
mosaic(Titanic, gp = gp_sex, legend = FALSE,
       main = "Titanic: colored by Sex (dim 2)")
legend_marimekko(gp_sex)


# ---- 6. Combination with labeling_points() ----

# Points overlaid on marimekko-colored tiles
gp <- shading_marimekko(HairEye)
mosaic(HairEye, gp = gp, legend = FALSE,
       labeling = labeling_points(scale = 2, seed = 42),
       main = "HairEye: marimekko + labeling_points (observed)")
legend_marimekko(gp)

# 3-way example with points
gp3 <- shading_marimekko(HairEyeColor, dim = 2)
mosaic(HairEyeColor, gp = gp3, legend = FALSE,
       labeling = labeling_points(scale = 2, seed = 42),
       main = "HairEyeColor: marimekko + labeling_points")
legend_marimekko(gp3)


# ---- 7. Expected-size tiles with observed-count points ----
# Reproducing Friendly (1995) style: tiles sized by expected,
# points representing observed counts

gp <- shading_marimekko(HairEye)
mosaic(HairEye, type = "expected",
       gp = gp, legend = FALSE,
       labeling = labeling_points(value_type = "observed", scale = 2, seed = 42),
       main = "HairEye: expected tiles + observed points + marimekko")
legend_marimekko(gp)
