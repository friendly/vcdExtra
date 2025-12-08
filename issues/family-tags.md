# Using @family Tags in R Package Documentation

## What is the `@family` tag?

The `@family` tag is a roxygen2 documentation tag that creates automatic cross-references between related functions. It groups functions into "families" and automatically generates "See also" links in the help files.

## How it works

When you add `@family tagname` to multiple function documentation blocks, roxygen2 automatically:
1. Creates a "See also" section in each function's help file
2. Lists all other functions with the same family tag
3. Makes them clickable links in the HTML help

## Example Usage

Here's how you could use it in vcdExtra:

```r
#' Fit all K-way models
#'
#' @description
#' Fits a collection of K-way models to a contingency table
#'
#' @param ... arguments
#' @return A glmlist object
#'
#' @family glmlist functions
#' @family model comparison
#' @export
Kway <- function(...) {
  # function code
}

#' Print a glmlist object
#'
#' @param x A glmlist object
#' @param ... additional arguments
#'
#' @family glmlist functions
#' @export
print.glmlist <- function(x, ...) {
  # function code
}

#' Likelihood ratio statistics for model comparison
#'
#' @param object A model object
#' @param ... additional models
#'
#' @family glmlist functions
#' @family model comparison
#' @export
LRstats <- function(object, ...) {
  # function code
}
```

## What gets generated

In the help file for `Kway()`, you'd see:

```
See Also
--------
Other glmlist functions: print.glmlist(), LRstats()
Other model comparison: LRstats()
```

## Suggested families for vcdExtra

Based on your package structure, here are some useful family groupings:

```r
# Mosaic plotting functions
@family mosaic plots

# Functions: mosaic.glm, mosaic.glmlist, mosaic3d, sieve.glm, assoc.glm

# Model list utilities
@family glmlist functions
# Functions: glmlist, print.glmlist, LRstats, Kway, mosaic.glmlist

# Statistical tests
@family association tests
# Functions: CMHtest, GKgamma, HLtest, zero.test

# Loglinear model utilities
@family loglinear models
# Functions: seq_loglm, loglmlist, conditional, joint, mutual, markov, saturated

# Data manipulation
@family data manipulation
# Functions: expand.dft, collapse.table, cutfac, datasets

# Model comparison
@family model comparison
# Functions: LRstats, Summarise, modFit, Kway
```

## Benefits for vcdExtra

1. **Discoverability**: Users looking at `mosaic.glm` would automatically see `mosaic3d` and `mosaic.glmlist`
2. **Consistency**: Shows which functions work together (e.g., all glmlist methods)
3. **Less maintenance**: Auto-generated, so no manual link updating needed
4. **Better UX**: Especially helpful for new users exploring the package

## How to add them

Simply add the `@family` line(s) to your roxygen2 comments in your R files, then run `devtools::document()` or `roxygen2::roxygenise()` to regenerate the .Rd files.

You can assign a function to multiple families - just use multiple `@family` tags as shown in the `LRstats` example above.

## Implementation Strategy

1. **Start with high-traffic functions**: Begin with commonly-used functions like `mosaic.glm`, `LRstats`, `CMHtest`
2. **Group by functionality**: Use the suggested families above as a starting point
3. **Test the output**: Run `devtools::document()` and check the generated .Rd files
4. **Review help pages**: Use `?function_name` to see how the cross-references look
5. **Iterate**: Add more families as you identify related function groups

## Notes

- Multiple `@family` tags per function are allowed and encouraged when appropriate
- Family names are free-form text - choose descriptive names
- The order of functions in "See also" is alphabetical
- Family tags work alongside manual `@seealso` entries (both will appear)
