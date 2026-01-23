# Implementation Plan: `labeling_points()` for vcdExtra

## Overview

A new labeling function for `vcd::mosaic()` displays that renders random points
within each tile, where the number of points represents either observed or expected
cell frequencies. This creates a "dot-density" visualization similar to dot-density
choropleth maps. It has the benefit that it provides novel way to visualize association
connected to a conceptual/physical model of frequency described by Friendly (1995)[^1],
where deviations from independence can be directly seen as variations in density of
points. (Sieve diagrams (`sieve()`) already do something similar.)

[^1]: Friendly, M. (1995). [Conceptual and Visual Models for Categorical Data](https://www.datavis.ca/papers/amstat95.pdf). 
_The American Statistician_, **49**, 153–160

This capability would be implemented in `vcdExtra`, hopefully requiring
no changes to `vcd`.

## Architecture Understanding

The `vcd` implementation of this class of visualization methods under the `strucplot()` framework is brilliant!
Mosaic plots, sieve plots, assoc plots, ... are all wrappers for the underlying `strucplot()`. 
But this generality makes it difficult to decide how to extend this.

### How vcd labeling functions work

1. **grapcon_generator pattern**: All labeling functions are function factories with
   class `"grapcon_generator"`. The outer function accepts styling/behavior parameters
   and returns an inner function that does the actual drawing.

2. **Inner function signature**:
   ```r
   function(d, split_vertical, condvars, prefix = "")
   ```
   - `d`: dimnames of the table
   - `split_vertical`: logical vector for split directions
   - `condvars`: conditioning variables
   - `prefix`: viewport name prefix

3. **Cell navigation**: Cells are named viewports like `"prefix cell:Var1=level1,Var2=level2"`.
   Use `seekViewport()` to navigate, draw, then return.

4. **Accessing data**: Observed/expected values can be retrieved from parent frame:
   ```r
   observed <- get("x", envir = parent.frame())
   expected <- get("expected", envir = parent.frame())
   ```

### Reference implementations

- `labeling_cells()` in labeling.R:103-143 - recursive cell traversal pattern
- `labeling_values()` in labeling.R:666-690 - accessing observed/expected from parent frame

## Proposed Function Design

### Function signature

```r
labeling_points <- function(
  value_type = c("observed", "expected"),
  scale = 1,                    # 1 point per `scale` observations
  pch = 19,                     # point character
  size = unit(0.5, "char"),     # point size
  gp_points = gpar(col = "black", alpha = 0.7),
  margin = unit(0.05, "npc"),   # margin inside cells
  seed = NULL,                  # for reproducible point placement
  clip = TRUE,                  # clip points to cell boundary
  ...                           # passed to labeling_border for axis labels
)
```

### Key parameters

| Parameter | Purpose |
|-----------|---------|
| `value_type` | Use observed or expected frequencies |
| `scale` | Points per observation (e.g., scale=10 means 1 point per 10 obs) |
| `pch`, `size`, `gp_points` | Point appearance |
| `margin` | Inset from cell edges to avoid edge crowding |
| `seed` | Reproducible random placement |
| `clip` | Whether to clip points at cell boundaries |

### Implementation approach

```r
labeling_points <- function(value_type = c("observed", "expected"),
                            scale = 1,
                            pch = 19,
                            size = unit(0.5, "char"),
                            gp_points = gpar(col = "black", alpha = 0.7),
                            margin = unit(0.05, "npc"),
                            seed = NULL,
                            clip = TRUE,
                            ...) {
  value_type <- match.arg(value_type)

  function(d, split_vertical, condvars, prefix = "") {
    # First, draw standard border labels
    labeling_border(...)(d, split_vertical, condvars, prefix)

    # Get the frequency table from parent frame
    lookup <- if (value_type == "observed") "x" else "expected"
    values <- get(lookup, envir = parent.frame())

    # Set seed for reproducibility if provided
    if (!is.null(seed)) set.seed(seed)

    # Navigate to each cell and draw points
    # (recursive traversal like labeling_cells)
    # ...
  }
}
class(labeling_points) <- "grapcon_generator"
```

## Design Decisions to Consider

### 1. Point distribution method

**Option A: Uniform random**
- Simple `runif()` for x and y coordinates
- Points may cluster or leave gaps by chance

**Option B: Jittered grid**
- Start with a grid, add jitter
- More even distribution, less natural looking

**Option C: Quasi-random** (maybe too much?)
- Use Halton or Sobol sequences for low-discrepancy sampling
- Even coverage without obvious patterns
- Could use `randtoolbox::halton()` or implement simple version

### 2. Scaling approach

**Option A: Direct count**
- `n_points = round(count / scale)`
- Simple, intuitive

**Option B: Square root scaling**
- `n_points = round(sqrt(count) / scale)`
- Avoids overcrowding in large cells, maintains perceptibility in small cells

**Option C: User-specified transform**
- `transform` parameter accepting a function
- Maximum flexibility

### 3. Color/styling of points

**Option A: Single style**
- All points same color (parameter `gp_points`)

**Option B: Inherit from cell shading**
- Match point color to residual-based shading, but darker or outlined in black?
- Access `gp$fill` from the cell's graphical parameters

**Option C: Both options**
- Parameter `inherit_color = FALSE` to toggle

### 4. Handling zero cells

- Draw no points (natural behavior)
- Consider whether to also call `labeling_border` for context

### 5. Handling large counts

- Cap maximum points per cell (`max_points` parameter)
- Or rely on `scale` parameter for user control
- Warning if cell would have > threshold points

## Implementation Steps

1. **Create basic structure**
   - Function factory with grapcon_generator class
   - Inner function with correct signature
   - Call `labeling_border()` for axis labels

2. **Implement cell traversal**
   - Adapt recursive pattern from `labeling_cells()`
   - Build cell viewport names correctly

3. **Point generation**
   - Calculate number of points from frequency and scale
   - Generate coordinates with margin inset
   - Handle seed for reproducibility

4. **Drawing**
   - `seekViewport()` to each cell
   - `grid.points()` with specified parameters
   - Respect clip parameter

5. **Testing**
   - Test with HairEyeColor, Titanic, UCBAdmissions
   - Test edge cases: zero cells, single-cell tables, very large counts
   - Verify with different scale values

6. **Documentation**
   - Roxygen documentation
   - Examples showing observed vs expected
   - Vignette section (optional)

## Example Usage

```r
library(vcd)
library(vcdExtra)

# Basic usage - observed frequencies
mosaic(HairEyeColor,
       labeling = labeling_points(scale = 5))

# Expected frequencies with custom styling
mosaic(HairEyeColor,
       labeling = labeling_points(
         value_type = "expected",
         scale = 5,
         pch = 21,
         gp_points = gpar(col = "navy", fill = "lightblue"),
         seed = 42
       ))

# With residual shading
mosaic(HairEyeColor,
       shade = TRUE,
       labeling = labeling_points(scale = 5))
```

## Open Questions

1. Should points inherit color from residual shading by default?

2. Should there be a `labeling_points_border()` convenience wrapper that
   combines `labeling_points()` with `labeling_border()`?

3. How to handle the interaction with `type = "expected"` in `strucplot()`?
   (When `type = "expected"`, the tile areas show expected, not observed)

4. Should we support different point symbols/colors for positive vs negative
   residuals?

5. Alternative implementation: Use `panel` argument in `struc_mosaic()` instead
   of labeling function? This would give direct access to cell data but different
   integration pattern.

## Files to Create/Modify

- `R/labeling_points.R` - main function implementation
- `man/labeling_points.Rd` - documentation (via roxygen)
- `tests/testthat/test-labeling_points.R` - unit tests (if using testthat)
- `NAMESPACE` - export the function

## Dependencies

- **grid** (already a vcd dependency)
- Optional: **randtoolbox** for quasi-random sequences (or implement simple version)
