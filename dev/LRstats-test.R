# dev/LRstats.R -- test new `label` argument for LRstats methods
# Usage: source("dev/LRstats.R") from the package root

library(vcdExtra)

# --- Examples ---

cat("\n=== loglmlist: Sequential joint independence (Titanic) ===\n")
data(Titanic, package = "datasets")
tit.joint <- seq_loglm(Titanic, type = "joint")

cat("\n-- label = 'name' (default) --\n")
LRstats(tit.joint)

cat("\n-- label = 'formula' --\n")
LRstats(tit.joint, label = "formula")

cat("\n-- label = 'formula', abbrev = TRUE --\n")
LRstats(tit.joint, label = "formula", abbrev = TRUE)

cat("\n-- label = 'formula', abbrev = 2 --\n")
LRstats(tit.joint, label = "formula", abbrev = 2)


cat("\n=== loglmlist: Sequential conditional independence (Titanic) ===\n")
tit.cond <- seq_loglm(Titanic, type = "conditional")

cat("\n-- label = 'name' (default) --\n")
LRstats(tit.cond)

cat("\n-- label = 'formula' --\n")
LRstats(tit.cond, label = "formula")


cat("\n=== glmlist: Mental health data ===\n")
data(Mental)
indep  <- glm(Freq ~ mental + ses, family = poisson, data = Mental)
Cscore <- as.numeric(Mental$ses)
Rscore <- as.numeric(Mental$mental)
coleff <- glm(Freq ~ mental + ses + Rscore:ses, family = poisson, data = Mental)
roweff <- glm(Freq ~ mental + ses + mental:Cscore, family = poisson, data = Mental)
linlin <- glm(Freq ~ mental + ses + Rscore:Cscore, family = poisson, data = Mental)

mods <- glmlist(indep, coleff, roweff, linlin)

cat("\n-- label = 'name' (default) --\n")
LRstats(mods)

cat("\n-- label = 'formula' --\n")
LRstats(mods, label = "formula")
