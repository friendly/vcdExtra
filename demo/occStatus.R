# Occupational status data from Goodman (1979) and Duncan (1979)
# Fit a variety of models.  Compare mosaic using expected= to mosaic.glm
# Occ status (1:8)-- professional, managerial, upper non-man, lower non-man,
#        ... unskilled

library(gnm)
library(vcdExtra)
data(occupationalStatus, package="datasets")
str(occupationalStatus)
occupationalStatus

# graphics::mosaicplot is the default plot method for a table
plot(occupationalStatus, shade=TRUE)

# define long labels for use in mosaics
long.labels <- list(set_varnames = c(origin="origin: Son's status", 
                                     destination="destination: Father's status"))

mosaic(occupationalStatus, shade=TRUE, 
       main="Occupational status: Independence model", 
       labeling_args = long.labels, 
       legend=FALSE)

# the standard model of independence
indep <- glm(Freq ~ origin + destination, 
             family = poisson, 
             data=occupationalStatus)

# the same mosaic, using the fitted model
mosaic(indep, 
       main="Independence model",
       labeling_args = long.labels, 
       legend=FALSE, 
       gp=shading_Friendly)


# fit the model of quasi-independence, ignoring the diagonal cells
quasi <-  gnm(Freq ~ origin + destination + Diag(origin,destination), 
                           family=poisson, data=occupationalStatus)
#str(quasi$data)
anova(quasi, test="Chisq")

## BUGLET (vcd): the diagonal cells should be considered 0 here--- outlined in black
mosaic(occupationalStatus, 
       expected=fitted(quasi), 
       main="Quasi-independence model", 
       labeling_args = long.labels, 
       legend=FALSE, gp=shading_Friendly)


## using mosaic.gnm
mosaic(quasi, main="Quasi-independence model",
  labeling_args = long.labels, 
  legend=FALSE, gp=shading_Friendly, 
  labeling=labeling_residuals)


# symmetry model
symmetry <- glm(Freq ~ Symm(origin, destination), 
                family=poisson, 
                data=occupationalStatus)
# mosaic(occupationalStatus, expected=fitted(symmetry), main="Symmetry model",
# 	gp=shading_Friendly, labeling=labeling_residuals, labeling_args = long.labels )

# using mosaic.glm --- OK
mosaic(symmetry, 
       main="Symmetry model", 
       gp=shading_Friendly, 
       labeling=labeling_residuals, 
       labeling_args = long.labels )

quasi.symm <- glm(Freq ~ origin + destination + Symm(origin, destination), 
                  family=poisson, 
                  data=occupationalStatus)
anova(quasi.symm)
mosaic(occupationalStatus, 
       expected=fitted(quasi.symm), 
       main="Quasi-symmetry model")

# model comparisons
anova(independence, quasi, quasi.symm, test="Chisq")

# compare symmetry to quasi summetry
anova(symmetry, quasi.symm, test="Chisq")

# association models
# uniform association, aka linear x linear association
Rscore <- as.vector(row(occupationalStatus))
Cscore <- as.vector(col(occupationalStatus))

uniform <- gnm(Freq ~ origin + destination + Rscore:Cscore, 
               family=poisson, 
               data=occupationalStatus)

mosaic(uniform, 
       main="Uniform association model", 
       labeling_args = long.labels, 
       legend=FALSE,
       gp=shading_Friendly, 
       labeling=labeling_residuals )

RChomog <- gnm(Freq ~ origin + destination + Diag(origin, destination) + 
                 MultHomog(origin, destination), 
               family=poisson, 
               data=occupationalStatus)

mosaic(RChomog, 
       main="RC homogeneous model", 
       labeling_args = long.labels, 
       legend=FALSE,
       gp=shading_Friendly, 
       labeling=labeling_residuals)

# RC1 - heterogeneous association
RC1 <- gnm(Freq ~ origin + destination + Diag(origin, destination) + 
             Mult(origin, destination),	
           family=poisson, 
           data=occupationalStatus)

mosaic(RC1, main="RC heterogeneous model", 
       labeling_args = long.labels, 
       legend=FALSE,
       gp=shading_Friendly, 
       labeling=labeling_residuals)
