# Better labeling of `loglmlist` and `glmlist` objects
#

A strength of the package is functions for creating a structured collection of models as `loglmlist` or `glmlist` objects.
But a weakness is that the model formulas for these submodels are inaccessible to other functions. 
For example, for the models of joint independence for the Titanic data:

``` r
data(Titanic, package="datasets")
# variables are in the order Class, Sex, Age, Survived
tit.joint <- seq_loglm(Titanic, type = "joint")
# compare the models
LRstats(tit.joint)
```

We get:

```
Likelihood summary table:
           AIC    BIC LR Chisq Df Pr(>Chisq)    
joint.1 509.95 509.33   475.81  3  < 2.2e-16 ***
joint.2 478.75 479.14   412.60  3  < 2.2e-16 ***
joint.3 257.88 264.83   159.10  7  < 2.2e-16 ***
joint.4 833.36 858.28   671.96 15  < 2.2e-16 ***
---
```

It would be nicer if there was a way to display the model formulas as labels in loglin "[]"" notations, e.g.,

```
function	       3-way	      4-way	            5-way
mutual	         [A] [B] [C]	[A] [B] [C] [D]	  [A] [B] [C] [D] [E]
joint	           [AB] [C]	    [ABC] [D]	        [ABCE] [E]
joint (with=1)	 [A] [BC]	    [A] [BCD]	        [A] [BCDE]
```

Perhaps an initial step would be to add a function `get_models(x, type = c("brackets", "formula"))` which would extract the model notation from a `loglmlist` or `glmlist` object. Also consider the as yet unused argument, `abbrev` for `loglin2string()`, which could be handy to make compact model strings.

## Initial version

* added `get_model()` to R/glmlist.R

### Bug:

Using the `abbrev` argument generates an error:

```
> get_models(tit.joint, abbrev = TRUE)
Error in as.character(replacement) : 
  cannot coerce type 'closure' to vector of type 'character'
```

### Model notation

The notation used presently distinguished between models fit to margins, eg, "(Class) (Sex)", vs. to the full table "[Class,Sex,Age] [Survived]"
Describe this in the documentation.

## get_model()

With this as a guide, would be useful to have a similar function, `get_model()` to get the same results for `loglm()` and
`glm()` models. 
Note that in R/seq_mosaic.R sequential models automatically get a model formula as the plot `main = ` title.

This suggests that `get_model()` should be a new file in `R/`, and then perhaps `get_models()` should be moved there. But
let's do `R/get_model.R` first.

