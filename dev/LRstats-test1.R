data(Hauser79)

hauser.indep <- gnm(Freq ~ Father + Son,
  data=Hauser79,
  family=poisson)

# Quasi-independence
hauser.quasi <-  update(hauser.indep,
                        ~ . + Diag(Father,Son))

# Quasi-symmetry
hauser.qsymm <-  update(hauser.indep,
                        ~ . + Diag(Father,Son) + Symm(Father,Son))

LRstats(hauser.indep, hauser.qsymm, hauser.quasi)

# Likelihood summary table:
#                 AIC    BIC LR Chisq Df Pr(>Chisq)
# hauser.indep 6390.8 6401.8   6170.1 16  < 2.2e-16 ***
# hauser.qsymm  268.2  291.3     27.4  6  0.0001193 ***
# hauser.quasi  914.1  931.1    683.3 11  < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# BUG: uses names, rather than formulas
LRstats(hauser.indep, hauser.qsymm, hauser.quasi, label="formula")

# Likelihood summary table:
#                 AIC    BIC LR Chisq Df Pr(>Chisq)
# hauser.indep 6390.8 6401.8   6170.1 16  < 2.2e-16 ***
# hauser.qsymm  268.2  291.3     27.4  6  0.0001193 ***
# hauser.quasi  914.1  931.1    683.3 11  < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


