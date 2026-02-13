# assoc-weights-test.R -- Test/demo script for edge weights in assoc_graph()
# Phase 2: measure = "chisq" | "cramer" on assoc_graph.glm()

library(vcdExtra)

# ---- DaytonSurvey: 5-way data ----------------------------------------

data(DaytonSurvey)

# Conditional independence model: {M,C} indep {R,S} | A
#
# define groups for the model
# [AM][AC][MC][AR][AS][RS]
mod.cond <- glm(Freq ~ (cigarette + alcohol + marijuana)^2 +
                        (alcohol + sex + race)^2,
                data = DaytonSurvey, family = poisson)

# define groups for the model

gps <- list(c("cigarette", "marijuana"),
            "alcohol",
             c("sex", "race"))

## 1. Unweighted (default) -- should be identical to Phase 1
g0 <- assoc_graph(mod.cond)
g0
plot(g0,
     groups = gps,
     main = "Unweighted (default)",
     layout = igraph::layout_nicely)


## 2. Partial chi-squared weights
g_chi <- assoc_graph(mod.cond, measure = "chisq")
g_chi
# All weights should be positive (removing an edge worsens fit)
stopifnot(all(igraph::E(g_chi)$weight > 0))

plot(g_chi, edge.label = TRUE,
     groups = gps,
     main = "Partial chi-squared weights")


## 3. Cramer's V weights
g_V <- assoc_graph(mod.cond, measure = "cramer")
g_V
# All values in [0, 1]
stopifnot(all(igraph::E(g_V)$weight >= 0 & igraph::E(g_V)$weight <= 1))

plot(g_V, edge.label = TRUE,
     groups = gps,
     main = "Cramer's V weights")


## 4. Side-by-side comparison
op <- par(mfrow = c(1, 3))
# Use a fixed layout so all three are comparable
layout_fixed <- igraph::layout_in_circle(g0)

plot(g0, layout = layout_fixed,
     groups = gps,
     main = "Unweighted")

plot(g_chi, layout = layout_fixed, edge.label = TRUE,
     groups = gps,
     main = "Chi-squared")

plot(g_V, layout = layout_fixed, edge.label = TRUE,
     groups = gps,
     main = "Cramer's V")
par(op)


## 5. Result types with weights

# Weighted adjacency matrix
mat <- assoc_graph(mod.cond, result = "matrix", measure = "chisq")
print(round(mat, 2))

# Weighted edge list (returns a data.frame)
edges <- assoc_graph(mod.cond, result = "edge_list", measure = "cramer")
print(edges)


# ---- All two-way model: every pair connected --------------------------

mod.all2 <- glm(Freq ~ .^2, data = DaytonSurvey, family = poisson)
g_all2 <- assoc_graph(mod.all2, measure = "chisq")
g_all2

op <- par(mfrow = c(1, 2))
plot(g_all2, edge.label = TRUE,
     groups = list(c("cigarette", "alcohol", "marijuana"),
                   c("sex", "race")),
     main = "All 2-way: chi-squared")

g_all2_V <- assoc_graph(mod.all2, measure = "cramer")
plot(g_all2_V, edge.label = TRUE,
     groups = list(c("cigarette", "alcohol", "marijuana"),
                   c("sex", "race")),
     main = "All 2-way: Cramer's V")
par(op)


# ---- Bartlett data (3-way table via glm) --------------------------------

data(Bartlett)
bart_df <- as.data.frame(Bartlett)

mod.bart <- glm(Freq ~ (Alive + Time + Length)^2,
                data = bart_df, family = poisson)

g_bart <- assoc_graph(mod.bart, measure = "chisq")
g_bart
plot(g_bart, edge.label = TRUE,
     main = "Bartlett: all 2-way (chi-squared)")

g_bart_V <- assoc_graph(mod.bart, measure = "cramer")
g_bart_V
plot(g_bart_V, edge.label = TRUE,
     main = "Bartlett: all 2-way (Cramer's V)")


# ---- UCBAdmissions via glm ---------------------------------------------

ucb_df <- as.data.frame(UCBAdmissions)

mod.ucb <- glm(Freq ~ (Admit + Gender) * Dept,
               data = ucb_df, family = poisson)

g_ucb <- assoc_graph(mod.ucb, measure = "chisq")
g_ucb
plot(g_ucb, edge.label = TRUE,
     groups = list("Dept", c("Admit", "Gender")),
     main = "Berkeley: [AD][GD] (chi-squared)")

g_ucb_V <- assoc_graph(mod.ucb, measure = "cramer")
plot(g_ucb_V, edge.label = TRUE,
     groups = list("Dept", c("Admit", "Gender")),
     main = "Berkeley: [AD][GD] (Cramer's V)")


# ---- Sparse model: mutual independence + one edge ----------------------

mod.SR <- glm(Freq ~ . + sex*race, data = DaytonSurvey, family = poisson)
g_SR <- assoc_graph(mod.SR, measure = "chisq")
g_SR
# Only one edge, so weight is a single number
stopifnot(length(igraph::E(g_SR)$weight) == 1)

plot(g_SR, edge.label = TRUE,
     main = "Mutual indep + [SR] (chi-squared)")
