# Imports
#
The master branch, which did not use roxygen, has imported functions listed as `importFrom`, but only in the `NAMESPACE` file.
Please convert these into roxygen `@importFrom` comments that I can use in my `vcdExtra-package.R` file

```
importFrom("grDevices", "hsv")
importFrom("stats",     "as.formula", "deviance", "family", "fitted", "formula", "glm", "logLik", "loglin", "model.frame", "na.pass", "nobs", "pchisq", "poisson", "qnorm", "quantile", "reformulate", "residuals", "rstandard", "runif", "terms", "update", "xtabs")
importFrom("utils",     "data", "menu", "type.convert")
importFrom("ca",        "cacoord", "multilines")
importFrom("grDevices", "rgb")
importFrom("graphics",  "abline", "plot", "points", "text")
importFrom("stats",      "update.formula")
```



# for datasets vignette -- maybe these don't need imports, if the pkgs are all in Suggests
importFrom("here",    "here")
importFrom("readxl",  "read_excel")
importFrom("glue",    "glue", "glue_collapse")
importFrom("purrr",   "map")
importFrom("stringr", "str_split_1")
importFrom("tidyr",   "separate_longer_delim")
importFrom("dplyr",   "select", "rename", "mutate", "group_by", "summarise", "left_join", "relocate")


