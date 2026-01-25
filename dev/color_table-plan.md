# Implementation Plan: color_table()

## Goal
Create a function to display n-way frequency tables with colored cell backgrounds showing patterns in observed frequencies or residuals from a loglinear model.

## Desired Features

Based on the example images:

1. **Input flexibility**:
   - Accept table, xtabs, ftable, or structable objects
   - Formula interface to specify row/column layout (like structable)
   - Optional model or expected frequencies for residual shading

2. **Display options**:
   - `shade = c("freq", "residuals", "pearson", "deviance")` - what determines colors
   - Sequential palette for frequencies (white → red)
   - Diverging palette for residuals (blue ↔ white ↔ red)
   - Show observed frequencies as cell text (even when shading by residuals)
   - Include row/column marginal totals

3. **Output format**:
   - Return a gt, tinytable, or ggplot object that can be further customized
   - Print method for immediate display

## Package Choice Analysis

### Option 1: gt (recommended)
**Pros:**
- `data_color()` is intuitive for cell shading
- Excellent table formatting
- Good documentation
- Can export to HTML, PDF, Word
- Active development

**Cons:**
- Heavier dependency
- Works with data frames, need to convert ftable/structable

### Option 2: ggplot2 + geom_tile()
**Pros:**
- Already a dependency of many packages
- Flexible and familiar to users
- Easy heatmap-style display

**Cons:**
- Doesn't look like a "table" - more like a heatmap
- Harder to show nested row/column headers for structable
- Text positioning can be tricky

### Option 3: tinytable
**Pros:**
- Minimal dependencies
- Lightweight

**Cons:**
- Less intuitive syntax for coloring
- Less feature-rich

### Recommendation
**Primary: gt** - Best balance of features and usability for table display
**Alternative: ggplot2** - For users who want plot-style output

## Function Design

```r
color_table <- function(x,
                        formula = NULL,
                        shade = c("residuals", "freq", "pearson", "deviance"),
                        model = NULL,
                        expected = NULL,
                        palette = NULL,
                        legend = TRUE,
                        margins = TRUE,
                        digits = 0,
                        title = NULL,
                        ...)
```

### Parameters

- `x`: A table, xtabs, matrix, ftable, or structable object
- `formula`: Formula specifying row ~ col layout (passed to structable if needed)
- `shade`: What values determine cell shading
  - `"freq"`: shade by observed frequencies (sequential palette)
  - `"residuals"`: shade by Pearson residuals from independence model (diverging)
  - `"pearson"`: same as residuals
  - `"deviance"`: shade by deviance residuals
- `model`: A fitted model (loglm or glm) to compute residuals from. If NULL and shade="residuals", uses independence model.
- `expected`: Expected frequencies (alternative to model)
- `palette`: Color palette. Default depends on shade type.
- `legend`: Logical, show color legend/scale?
- `margins`: Logical, include row/column totals?
- `digits`: Number of decimal places for displayed values
- `title`: Optional table title

### Return Value
A `gt` object (class `"gt_tbl"`) that can be further modified with gt functions.

## Implementation Steps

### Step 1: Core function structure
1. Validate inputs
2. Convert input to structable if formula provided
3. Convert to data frame suitable for gt

### Step 2: Compute shading values
1. If shade = "freq", use observed frequencies
2. If shade = "residuals" and no model provided, fit independence model
3. Compute Pearson or deviance residuals as needed

### Step 3: Build gt table
1. Create base gt table from data frame
2. Apply data_color() with appropriate palette
3. Add row/column totals if margins = TRUE
4. Add title, formatting

### Step 4: Handle multi-way tables
- Use structable to flatten to 2D
- Preserve nested header structure in gt using tab_spanner()

## Color Palettes

### For frequencies (sequential):
```r
scales::col_numeric(palette = c("white", "firebrick"), domain = NULL)
```

### For residuals (diverging):
```r
scales::col_numeric(palette = c("steelblue", "white", "firebrick"), domain = NULL)
# or use colorspace::diverging_hcl()
```

Match the blue/red scheme in the example images.

## Example Usage

```r
# Basic usage - shade by residuals from independence
data(HairEyeColor)
HEC <- margin.table(HairEyeColor, 2:1)
color_table(HEC)

# Specify layout with formula
color_table(HairEyeColor, formula = Sex ~ Hair + Eye)

# Shade by frequencies instead
color_table(HEC, shade = "freq")

# Use a specific model
mod <- MASS::loglm(~ Hair + Eye, data = HEC)
color_table(HEC, model = mod)

# 3-way table with structable-style layout
color_table(HairEyeColor, formula = Eye ~ Hair + Sex)
```

## Dependencies to Add

- gt (Suggests, for color_table functionality)
- scales (likely already available via ggplot2)

## Files to Create

1. `dev/color_table.R` - Main function implementation
2. `dev/test-color_table.R` - Test script with examples

## Open Questions

1. Should there be a ggplot2-based alternative function (e.g., `color_tile()`)?
2. How to handle very large tables (many levels)?
3. Should we support interactive output (e.g., via reactable)?
4. Export formats: just display, or also save to file?

## Next Steps

1. Create basic implementation for 2-way tables with gt
2. Test with HairEyeColor data
3. Extend to handle structable/ftable for multi-way tables
4. Add residual computation
5. Refine color scales to match example images
6. Add documentation and examples
