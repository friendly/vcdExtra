# testing hurdle methods


library(pscl)
data("bioChemists", package = "pscl")
fm_hp1 <- hurdle(art ~ ., data = bioChemists,
                 , dist = "negbin", zero = "negbin")
coef(fm_hp1)

names(fm_hp1)

class(fm_hp1)
# [1] "hurdle"

methods(class = "hurdle")
# [1] coef         extractAIC   fitted       logLik       model.matrix predict      predprob    
# [8] print        residuals    summary      terms        vcov  


# sketch of something to be used as a new print method

hurdle_coefs <- function(coefs) {
  nms <- names(coefs)
  count_idx <- startsWith(nms, "count_")
  zero_idx  <- startsWith(nms, "zero_")

  count_coefs <- coefs[count_idx]
  zero_coefs  <- coefs[zero_idx]

  count_vars <- sub("^count_", "", names(count_coefs))
  zero_vars  <- sub("^zero_",  "", names(zero_coefs))

  all_vars <- union(count_vars, zero_vars)

  data.frame(
    term        = all_vars,
    count       = count_coefs[match(all_vars, count_vars)],
    zero        = zero_coefs[match(all_vars, zero_vars)],
    row.names   = NULL
  )
}

coef.fm_hp1 <- hurdle_coefs(coef(fm_hp1))

# term        count        zero
# 1 (Intercept)  0.355124754  8.06840601
# 2    femWomen -0.244671931 -2.36962058
# 3  marMarried  0.103417223  2.86148197
# 4        kid5 -0.153259854 -2.39726213
# 5         phd -0.002933257  0.05428915
# 6        ment  0.023738157  0.84586352

# do this in transposed for, with rows for the count and zero models:
coefs.wide <- t(coef.fm_hp1)
names <- coefs.wide["term",]
coefs.wide <- coefs.wide[-1,] |> as.numeric() |> matrix(nrow=2) |> as.data.frame()
colnames(coefs.wide) <- names
coefs.wide

#
fm <- hurdle(art ~ ., data = bioChemists, dist = "negbin", zero = "negbin")
hurdletest(fm)

