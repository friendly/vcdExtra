# test diag pairs with cell labels
#
library(vcdExtra)
Hoyt <- vcdExtra::Hoyt
Hoyt2 <- collapse.table(Hoyt,
                        Status = c("College", rep("Non-College", 3)),
                        Occupation = c(rep("High", 3),
                                       rep("Medium", 2),
                                       rep("Low", 3)))

pairs(
  Hoyt2,
  gp = shading_Friendly,
  space = 0.4,
  gp_args = list(interpolate = 1:4),
  diag_panel_args = list(NULL)
)
