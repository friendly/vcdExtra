# Corrected: lvplot() ONLY works with Rank = 2 models!
# This is a key limitation that's easy to miss

library(VGAM)
library(vcdExtra)
library(tidyr)

# Mental Health Data Example
# ==========================

data(Mental, package = "vcdExtra")

# Check the actual column names
head(Mental)
# mental categories are: Well, Mild, Moderate, Impaired (capitalized!)

# Convert to wide format
Mental_wide <- Mental %>%
  pivot_wider(names_from = mental, 
              values_from = Freq,
              values_fill = 0)

print(Mental_wide)

# IMPORTANT: lvplot() requires Rank = 2
# Rank = 1 will give error: "can only handle rank-2 models"

mental_rr2 <- rrvglm(cbind(Well, Mild, Moderate, Impaired) ~ ses,
                     data = Mental_wide,
                     family = multinomial,
                     Rank = 2)  # Must be 2!

summary(mental_rr2)

# Now lvplot() works
par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))
lvplot(mental_rr2, 
#       label = TRUE,
       main = "Mental Health by SES (Rank-2 Biplot)",
       # lcol = "blue",
       # pcol = "red",
       pch = 16,
       las = 1,
      xlim = c(-2.5, 2),
      ylim = c(-1, 5),
      xpd = TRUE
)
abline(h = 0, v = 0, col = "gray80", lty = 2)


# For teaching, compare Rank-1 vs Rank-2
# =======================================

# Fit Rank-1 for comparison (can't plot with lvplot, but can assess fit)
mental_rr1 <- rrvglm(cbind(Well, Mild, Moderate, Impaired) ~ ses,
                     data = Mental_wide,
                     family = multinomial,
                     Rank = 1)

cat("\n=== Model Comparison ===\n")
cat("Rank 1 - AIC:", AIC(mental_rr1), "\n")
cat("Rank 2 - AIC:", AIC(mental_rr2), "\n")
cat("Rank 1 - BIC:", BIC(mental_rr1), "\n")
cat("Rank 2 - BIC:", BIC(mental_rr2), "\n\n")

# Discussion point: Is the 2D representation necessary?
# Often Rank=1 is sufficient statistically, but Rank=2 needed for biplot


# Alternative: Use Latvar() for Rank-1 visualization
# ===================================================

cat("For Rank-1 models, use Latvar() to extract latent variable scores:\n\n")

# Extract latent variable scores
lv_scores <- Latvar(mental_rr1)
print(lv_scores)

# Create manual plot for Rank-1
ses_levels <- Mental_wide$ses
plot(lv_scores, seq_along(ses_levels),
     xlab = "Latent Variable Score",
     ylab = "",
     yaxt = "n",
     pch = 16, col = "blue",
     main = "Rank-1 Model: SES Latent Scores")
axis(2, at = seq_along(ses_levels), labels = ses_levels, las = 1)
abline(v = 0, col = "gray", lty = 2)


# Example 2: Abortion Data (also needs Rank=2)
# =============================================

data(Abortion, package = "vcdExtra")

abort_wide <- Abortion %>%
  pivot_wider(names_from = support,
              values_from = Freq,
              values_fill = 0)

print(abort_wide)

# Check column names
names(abort_wide)

# Fit Rank-2 model for biplot
abort_rr2 <- rrvglm(cbind(Yes, No) ~ sex + status,
                    data = abort_wide,
                    family = multinomial,
                    Rank = 2)

summary(abort_rr2)

par(mfrow = c(1, 1))
lvplot(abort_rr2,
       label = TRUE,
       main = "Abortion Support by Sex and Status (Rank-2)",
       lcol = "darkgreen",
       pcol = "orange",
       pch = 17)


# Example 3: Finding good vcdExtra datasets for Rank-2 biplots
# =============================================================

# Look for datasets with enough complexity for 2D representation
# Good candidates: multiple response categories AND multiple predictors

# Arthritis data (from vcd package, but works here)
data(Arthritis, package = "vcd")

# Create contingency table
arth_tab <- xtabs(~ Treatment + Improved, data = Arthritis)
arth_wide <- as.data.frame.matrix(arth_tab)
arth_wide$Treatment <- rownames(arth_wide)
rownames(arth_wide) <- NULL

print(arth_wide)

# With only 2 treatment levels, Rank-2 may be overkill, but demonstrates
arth_rr2 <- rrvglm(cbind(None, Some, Marked) ~ Treatment,
                   data = arth_wide,
                   family = multinomial,
                   Rank = 2)

lvplot(arth_rr2, 
       label = TRUE,
       main = "Arthritis Treatment Effect (Rank-2)")


# TEACHING TIPS
# =============

cat("\n\n=== KEY TEACHING POINTS ===\n\n")

cat("1. lvplot() REQUIRES Rank = 2\n")
cat("   - This creates a 2D biplot\n")
cat("   - Cannot use lvplot() with Rank = 1\n\n")

cat("2. For Rank = 1 models:\n")
cat("   - Use Latvar() to extract scores\n")
cat("   - Create custom 1D visualization\n")
cat("   - Or use plot(model) for diagnostics\n\n")

cat("3. Check capitalization in response categories!\n")
cat("   - Mental: Well, Mild, Moderate, Impaired (capitals)\n")
cat("   - Abortion: Yes, No (capitals)\n")
cat("   - Use str(data) or head(data) to verify\n\n")

cat("4. When to use Rank = 2:\n")
cat("   - When you want lvplot() visualization\n")
cat("   - When data structure warrants 2D representation\n")
cat("   - When Rank = 1 fit is poor (check AIC/BIC)\n\n")

cat("5. Biplot interpretation (Rank = 2):\n")
cat("   - Two latent dimensions (axes)\n")
cat("   - Points in 2D space show relationships\n")
cat("   - More complex than Rank = 1, but richer\n\n")


# Diagnostic plots work for any rank
# ===================================

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(mental_rr2, which = 1:4, las = 1)