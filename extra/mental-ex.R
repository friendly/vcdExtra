## Ordinal factors and structured associations
data(Mental)

str(Mental)
(Mental.tab <- xtabs(Freq ~ ses+mental, data=Mental))


# Fit loglinear models using `glm`, plot mosaics using `mosaic.glm`
# fit independence model

indep <- glm(Freq ~ mental+ses,
             family = poisson, data = Mental)
LRstats(indep)

# labels for table factors
long.labels <- list(set_varnames = c(mental="Mental Health Status", 
                                     ses="Parent SES"))

mosaic(indep,
       residuals_type="rstandard",
       labeling_args = long.labels, 
       labeling=labeling_residuals)


# fit linear x linear (uniform) association.  Use integer scores for rows/cols 
Cscore <- as.numeric(Mental$ses)
Rscore <- as.numeric(Mental$mental)

linlin <- glm(Freq ~ mental + ses + Rscore:Cscore,
              family = poisson, data = Mental)
mosaic(linlin, ~ ses + mental,
       residuals_type="rstandard", 
       labeling_args = long.labels, 
       labeling=labeling_residuals, 
       suppress=1, 
       gp=shading_Friendly,
       main="Lin x Lin model")

anova(linlin, test="Chisq")

# use update.glm method to fit other models

linlin <- update(indep, . ~ . + Rscore:Cscore)
roweff <- update(indep, . ~ . + mental:Cscore)
coleff <- update(indep, . ~ . + Rscore:ses)
rowcol <- update(indep, . ~ . + Rscore:ses + mental:Cscore)

# compare models
LRstats(indep, linlin, roweff, coleff, rowcol)

# tests of nested models

anova(indep, linlin, roweff, test = "Chisq")

