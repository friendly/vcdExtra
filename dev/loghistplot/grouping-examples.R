# Examples of grouping behaviour for loghistplot3.R and loghistplot4.R

library(vcdExtra)

data("Donner", package = "vcdExtra")

# Wanted to stratify by 3 groups so did this:
don <- collapse_levels(Donner, 
                       family = list(
                         A_to_I = c("Breen", "Donner", "Eddy", "FosdWolf", "Graves"), 
                         J_to_R = c("MurFosPik", "Keseberg", "Reed", "McCutchen"), 
                         Other = c("Other")))

logist_plot(survived ~ age, data = don, group = "family", 
            marginal = "density")

logist_plot(survived ~ age, data = don, group = "family", 
            marginal = "points")

# By 2 groups (sex) w/ custom colours:

logist_plot(survived ~ age, data = Donner, 
            group = "sex", group.colors = c("hotpink3", "steelblue"), 
            marginal = "density")

logist_plot(survived ~ age, data = Donner, 
            group = "sex", group.colors = c("hotpink3", "steelblue"), 
            marginal = "points")

# =========== Notes: =====================================================
# marginal = "hist" will produce an error when group != NULL

logist_plot(survived ~ age, data = Donner, 
            group = "sex", 
            marginal = "hist")

# Error: Grouping is not supported for `marginal = "hist"`; use 
# `marginal = "points"` or `marginal = "density"`.

# GPT said to save `|` conditioning in formula notation for faceting as opposed
# to grouping behaviour.
