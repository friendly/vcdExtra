# Debug script for mcaplot() error with Heather's data
# Error: "non-conformable arguments" in rsc %*% diag(sv) inside cacoord()
# Traceback: mcaplot() -> cacoord(obj, type=map, rows=FALSE)

library(ca)
library(vcdExtra)

load("issues/dat_categor-noNA.rda")  # loads dat_categor.noNA

# Build the 5x3x2 table (cluster x Gender x Opi)
CTO1 <- xtabs(~ cluster + Gender + Opi, data = dat_categor.noNA)
str(CTO1)  # confirm dimensions

CTO.mca <- mjca(CTO1)
CTO.mca

# Inspect the mjca object structure relevant to cacoord
cat("\n--- mjca structure ---\n")
cat("sv (singular values):\n"); print(CTO.mca$sv)
cat("length(sv):", length(CTO.mca$sv), "\n")
cat("dim(colcoord):", dim(CTO.mca$colcoord), "\n")
cat("dim(rowcoord):", dim(CTO.mca$rowcoord), "\n")
cat("lambda:", CTO.mca$lambda, "\n")

# Try cacoord directly — this is where the error fires
cat("\n--- Testing cacoord() directly ---\n")
tryCatch(
  { coords <- cacoord(CTO.mca, type = "symmetric", rows = FALSE)
    cat("cacoord succeeded; dim:", dim(coords), "\n") },
  error = function(e) cat("cacoord ERROR:", conditionMessage(e), "\n")
)

# Try each map type to see which (if any) work
map_types <- c("symmetric", "rowprincipal", "colprincipal",
               "symbiplot", "rowgab", "colgab", "rowgreen", "colgreen")
cat("\n--- cacoord() by map type (rows=FALSE) ---\n")
for (m in map_types) {
  tryCatch(
    { cc <- cacoord(CTO.mca, type = m, rows = FALSE)
      cat(sprintf("  %-14s OK   dim: %d x %d\n", m, nrow(cc), ncol(cc))) },
    error = function(e) cat(sprintf("  %-14s ERROR: %s\n", m, conditionMessage(e)))
  )
}

# Try mcaplot() with each map type
cat("\n--- mcaplot() by map type ---\n")
for (m in map_types) {
  tryCatch(
    { mcaplot(CTO.mca, type = m, main = m)
      cat(sprintf("  %-14s OK\n", m)) },
    error = function(e) cat(sprintf("  %-14s ERROR: %s\n", m, conditionMessage(e)))
  )
}

# For comparison: Titanic (the working example from ?mcaplot)
cat("\n--- Titanic (known-working example) ---\n")
titanic.mca <- mjca(Titanic)
cat("sv:", titanic.mca$sv, "\n")
cat("dim(colcoord):", dim(titanic.mca$colcoord), "\n")
tryCatch(
  { mcaplot(titanic.mca); cat("Titanic mcaplot OK\n") },
  error = function(e) cat("Titanic mcaplot ERROR:", conditionMessage(e), "\n")
)

# ca::plot works — compare object slots
cat("\n--- ca::plot (known working) ---\n")
ca::plot(CTO.mca)
