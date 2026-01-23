# Test examples for labeling_points()
# These are working examples for development and testing

# library(vcd)
library(vcdExtra)   # also loads vcd, gnm
devtools::load_all()

# Simple 2-way table
HairEye <- margin.table(HairEyeColor, 2:1)

# Basic usage
# Reproduces Fig 5 in Friendly (1995)
mosaic(HairEye,  
       direction = c("v", "h"),
       labeling = labeling_points(scale = 1),
       main = "Hair-Eye Color\n# of points = cell n",
       main_gp = gpar(fontsize = 16))

# Show expected frequencies instead of observed in cells
mosaic(HairEye, 
       direction = c("v", "h"),
       labeling = labeling_points(
            value_type = "expected",
            scale = 1,
            seed = 42),
       main = "Hair-Eye Color\n # of points = expected n",
       main_gp = gpar(fontsize = 16)
       )

# Make tiles show expected frequencies, show points for observed frequencies
# Reproduces Fig 6 in Friendly (1995),
mosaic(HairEye, 
       type = "expected",
       direction = c("v", "h"),
       labeling = labeling_points(
         value_type = "observed",
         scale = 1,
         seed = 42)       
       )

# Combine with residual shading
mosaic(HairEye, # direction = c("v", "h"),
       shade = TRUE,
       legend = FALSE,
       labeling = labeling_points(scale = 1, 
                                  gp_points = gpar(col = "black")))

# Customize point appearance
mosaic(HairEye, # direction = c("v", "h"),
       labeling = labeling_points(
            scale = 1,
            pch = 21,
            size = unit(0.6, "char"),
            gp_points = gpar(col = "navy", fill = "lightblue"),
            margin = unit(0.05, "npc")
))

# -----------------------------------------------------------------------------
# 3-way table examples (HairEyeColor)
# -----------------------------------------------------------------------------

# Basic usage with HairEyeColor
mosaic(HairEyeColor, labeling = labeling_points(scale = 5))

# Show expected frequencies instead of observed
mosaic(HairEyeColor, labeling = labeling_points(
  value_type = "expected",
  scale = 5,
  seed = 42
))

# Combine with residual shading
mosaic(HairEyeColor,
       shade = TRUE,
       labeling = labeling_points(scale = 5, gp_points = gpar(col = "black")))

# Customize point appearance
mosaic(HairEyeColor, labeling = labeling_points(
  scale = 3,
  pch = 21,
  size = unit(0.4, "char"),
  gp_points = gpar(col = "navy", fill = "lightblue"),
  margin = unit(0.05, "npc")
))

# -----------------------------------------------------------------------------
# Other datasets to test
# -----------------------------------------------------------------------------

# Titanic - 4-way table
mosaic(Titanic, 
       shade = TRUE, legend = FALSE,
       labeling = labeling_points(scale = 2))

# UCBAdmissions
mosaic(UCBAdmissions, 
       shade = TRUE, legend = FALSE,
       labeling = labeling_points(scale = 2))

# -----------------------------------------------------------------------------
# Edge cases
# -----------------------------------------------------------------------------

# Very small counts - scale = 1
small_table <- as.table(matrix(c(2, 5, 3, 8), nrow = 2,
                                dimnames = list(A = c("a1", "a2"),
                                               B = c("b1", "b2"))))
mosaic(small_table, 
       labeling = labeling_points(scale = 1, size = 2))

# Table with zeros
zero_table <- as.table(matrix(c(0, 15, 10, 0), nrow = 2,
                               dimnames = list(A = c("a1", "a2"),
                                              B = c("b1", "b2"))))
mosaic(zero_table, 
       labeling = labeling_points(scale = 1))
