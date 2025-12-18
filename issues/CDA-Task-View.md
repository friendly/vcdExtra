# CRAN Task View: Categorical Data Analysis
## Proposed Package Categories and List

---

## 1. Core Packages: Contingency Tables & Loglinear Models

**Essential packages:**

- **vcd** - Visualizing Categorical Data; mosaic plots, association plots, strucplots
- **vcdExtra** - Extensions to vcd; additional datasets, glmlist methods, mosaic.glm()
- **MASS** - Modern Applied Statistics; loglm(), polr(), glm.nb()
- **gnm** - Generalized Nonlinear Models; RC models, quasi-symmetry, Goodman models

**Additional packages:**

- **contingencytables** - Statistical inference for contingency tables (updated Nov 2025)
- **catdata** - Categorical data examples and datasets (updated July 2025)
- **confreq** - Configural Frequency Analysis
- **DescTools** - Comprehensive tables utilities and categorical tests

---

## 2. Ordinal Regression Models

**Primary packages:**

- **ordinal** - Cumulative link models (CLM) and mixed models (CLMM); most downloaded OR package
- **VGAM** - Vector GLMs and GAMs; 150+ models including ordinal (updated Dec 2025)
- **MASS::polr()** - Proportional odds logistic regression (standard baseline)

**Specialized packages:**

- **mvord** - Multivariate ordinal regression
- **rms** - Regression modeling strategies including ordinal models
- **glmnetcr** - Penalized continuation ratio models

**Note:** A [2025 systematic review](https://wires.onlinelibrary.wiley.com/doi/abs/10.1002/wics.70025) identified 48 R packages for ordinal regression.

---

## 3. Multinomial & Polytomous Response Models

- **nnet** - Multinomial log-linear models via neural networks
- **mlogit** - Multinomial logit models
- **nestedLogit** - Nested dichotomy models for polytomous responses
- **mnlogit** - Efficient multinomial logit estimation
- **VGAM** - Multiple families for categorical responses

---

## 4. Mixed Effects Models for Categorical Data

**Frequentist approaches:**

- **lme4** - glmer() for binary/count mixed models
- **ordinal** - clmm() for ordinal mixed models
- **GLMMadaptive** - Generalized linear mixed models including ordinal
- **multgee** - GEE for correlated categorical data

**Bayesian approaches:**

- **brms** - Bayesian regression models using Stan; most flexible for categorical data
- **rstanarm** - Pre-compiled Stan models for standard GLMMs
- **MCMCglmm** - MCMC for multinomial, ordinal, zero-inflated models

---

## 5. Count Data Models: Zero-Inflated & Hurdle

- **pscl** - Zero-inflated and hurdle models (v1.5.9, July 2025)
- **MASS** - glm.nb() for negative binomial
- **glmmTMB** - Generalized linear mixed models with zero-inflation
- **brms** - Bayesian zero-inflated/hurdle models
- **VGAM** - Multiple zero-inflated families

**Resources:** [UCLA tutorial on zero-inflated models](https://stats.oarc.ucla.edu/r/seminars/zero-inflated-and-hurdle-models-for-count-data-in-r/)

---

## 6. Correspondence Analysis

- **FactoMineR** - CA, MCA, FAMD (v2.12, July 2025)
- **ca** - Simple and multiple correspondence analysis
- **factoextra** - Visualization for FactoMineR results
- **ade4** - Multivariate analysis including correspondence methods
- **MASS::corresp()** - Simple correspondence analysis
- **ExPosition** - Exploratory multivariate methods

**Resources:** [MCA in R Guide](https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/)

---

## 7. Survey Analysis with Categorical Data

- **survey** - Complex survey sampling analysis (v4.4-8, Aug 2025)
  - svyglm() - Survey-weighted GLMs
  - svytable() - Contingency tables from survey data
  - svychisq() - Chi-square tests for survey data

**Resources:** [UCLA Survey Data Analysis](https://stats.oarc.ucla.edu/r/seminars/survey-data-analysis-with-r/)

---

## 8. Item Response Theory & Latent Class Analysis

**IRT packages:**

- **mirt** - Multidimensional IRT for polytomous items
- **ltm** - Latent trait models
- **eRm** - Extended Rasch models
- **TAM** - Test analysis modules

**Latent class:**

- **poLCA** - Polytomous variable latent class analysis
- **flexmix** - Flexible mixture modeling
- **randomLCA** - Random effects latent class models

---

## 9. Association & Agreement Measures

**Measures of association:**

- **vcd** - assocstats(), Kappa statistics
- **vcdExtra** - CMHtest(), GKgamma()
- **DescTools** - Comprehensive association measures
- **psych** - Cohen's kappa, ICC

**Paired comparisons:**

- **prefmod** - Bradley-Terry models for rankings/ratings
- **BradleyTerry2** - Bradley-Terry models with extensions

---

## 10. Visualization Packages

**Mosaic & strucplot framework:**

- **vcd** - mosaic(), assoc(), sieve(), fourfold()
- **vcdExtra** - mosaic.glm(), mosaic3d()
- **ggmosaic** - Mosaic plots in ggplot2 framework

**General categorical visualization:**

- **ggplot2** - geom_bar(), geom_tile() for categorical data
- **sjPlot** - Plotting glm/lmer results
- **effects** - Effect displays for models
- **emmeans** - Estimated marginal means and contrasts

---

## 11. Data Manipulation & Utilities

- **vcdExtra** - expand.dft(), collapse.table(), cutfac()
- **tidyr** - pivot_wider(), pivot_longer() for tables
- **janitor** - tabyl() for frequency tables
- **forcats** - Tools for categorical variables (factors)
- **epitools** - Epidemiology tools including 2x2 tables

---

## 12. Specialized Methods

**Exact tests:**

- **exact2x2** - Exact tests for 2x2 tables
- **exactci** - Exact confidence intervals

**Configural analysis:**

- **confreq** - Configural Frequency Analysis

**Model diagnostics:**

- **DHARMa** - Residual diagnostics for GLMMs
- **performance** - Model assessment
- **broom** / **broom.mixed** - Tidy model outputs

---

## 13. Discrete Distributions

From the [Distributions Task View](https://cran.r-project.org/web/views/Distributions.html):
- **extraDistr** - Additional discrete distributions
- **VGAM** - Vector generalized linear models
- **gamlss.dist** - GAMLSS family distributions
- **actuar** - Actuarial distributions

---

## 14. Related Resources

**Books & Documentation:**

- Friendly & Meyer (2016): *Discrete Data Analysis with R* - supported by vcdExtra
- Agresti (2013): *Categorical Data Analysis* - examples at [stats.ufl.edu](https://users.stat.ufl.edu/~aa/ordinal/R_examples.pdf)
- Fagerland et al.: *Statistical Analysis of Contingency Tables* - supported by contingencytables package

**Online Courses:**
- [Psy 6136: Categorical Data Analysis](https://friendly.github.io/psy6136/)

---

## References & Sources

- [Package catdata](https://cran.r-project.org/web/packages/catdata/catdata.pdf)
- [Package contingencytables](https://cran.r-project.org/package=contingencytables)
- [Systematic Review of Ordinal Regression Packages](https://wires.onlinelibrary.wiley.com/doi/abs/10.1002/wics.70025)
- [VGAM Package](https://cran.r-project.org/web/packages/VGAM/VGAM.pdf)
- [Ordinal Regression Guide](https://cran.r-project.org/web/packages/ordinal/vignettes/clm_article.pdf)
- [CRAN Task View: Mixed Models](https://cran.r-project.org/web/views/MixedModels.html)
- [FactoMineR Package](https://cran.r-project.org/web/packages/FactoMineR/FactoMineR.pdf)
- [Package pscl](https://cran.r-project.org/web/packages/pscl/pscl.pdf)
- [Survey Package](https://cran.r-project.org/web/packages/survey/survey.pdf)
- [brms Overview](https://cran.r-project.org/web/packages/brms/vignettes/brms_overview.pdf)
- [CRAN Task Views Overview](https://cran.r-project.org/web/views/)
- [Distributions Task View](https://cran.r-project.org/web/views/Distributions.html)
- [Psychometrics Task View](https://cran.r-project.org/web/views/Psychometrics.html)

---

## Notes for Further Development

### Potential Additional Categories:

- **Longitudinal categorical data** (transition models, Markov models)
- **Spatial categorical data** (spatial point patterns with marks)
- **Bayesian nonparametrics** for categorical data
- **Machine learning** methods for categorical predictors/responses (random forests, boosting)
- **Penalized/regularized** methods (lasso, ridge for categorical models)
- **Causal inference** with categorical treatments/outcomes

### Packages to Consider Adding:

- **logistf** - Firth's penalized likelihood logistic regression
- **betareg** - Beta regression (for proportions)
- **DirichletReg** - Dirichlet regression for compositional data
- **mlogitBMA** - Bayesian model averaging for multinomial logit
- **glmx** - Generalized linear models with extra parameters
- **countreg** - Count data regression (development version)
- **mgcv** - GAMs for categorical responses
- **gam** - Generalized additive models

### Cross-References to Other Task Views:

- **Mixed Models** - For hierarchical categorical data
- **Distributions** - For discrete probability distributions
- **Psychometrics** - For IRT and latent class models
- **Bayesian** - For Bayesian categorical analysis methods
- **Survival** - For discrete-time survival models
- **MachineLearning** - For classification with categorical outcomes
