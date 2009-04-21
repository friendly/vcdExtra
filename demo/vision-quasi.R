# VisualAcuity data: Quasi- and Symmetry models

library(vcdExtra)
library(gnm)
women <- subset(VisualAcuity, gender=="female", select=-gender)

indep <- glm(Freq ~ right + left,  data = women, family=poisson)
mosaic(indep, residuals_type="rstandard", gp=shading_Friendly,
       main="Vision data: Independence (women)"  )

quasi.indep <- glm(Freq ~ right + left + Diag(right, left), 
       data = women, family = poisson)
mosaic(quasi.indep, residuals_type="rstandard", gp=shading_Friendly,
       main="Quasi-Independence (women)"  )

symmetry <- glm(Freq ~ Symm(right, left), 
       data = women, family = poisson)
# BUGGED
# mosaic(symmetry, main="Symmetry model (women)"  )

quasi.symm <- glm(Freq ~ right + left + Symm(right, left), 
       data = women, family = poisson)
mosaic(quasi.symm, residuals_type="rstandard", gp=shading_Friendly,
       main="Quasi-Symmetry model (women)")

# model comparisons: for *nested* models
anova(indep, quasi.indep, quasi.symm, test="Chisq")
anova(symmetry, quasi.symm, test="Chisq")
