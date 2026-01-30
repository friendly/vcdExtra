# Revised Plan: Column Spanner Headings for color_table()

## Key Insight from MF Notes

The `structable()` object already contains all the information we need:

```r
pre_st <- structable(PremaritalSex + ExtramaritalSex ~ MaritalStatus + Gender,
                     data = PreSex)
str(pre_st)
```

Attributes available:
- `attr(st, "col.vars")` - list of column variables with their levels
- `attr(st, "row.vars")` - list of row variables with their levels
- `attr(st, "dnames")` - all dimension names

This information is currently **lost** when we do `as.matrix(st)` in the methods.

## Revised Strategy

### 1. Preserve structable metadata before matrix conversion

In `color_table.table()` around line 209, after creating the structable but before converting to matrix:

```r
if (ndim > 2) {
  if (requireNamespace("vcd", quietly = TRUE)) {
    if (!is.null(formula)) {
      st <- vcd::structable(formula, data = x)
    } else {
      st <- vcd::structable(x)
    }

    # Extract variable structure BEFORE converting to matrix
    col_vars <- attr(st, "col.vars")  # List: name -> levels
    row_vars <- attr(st, "row.vars")  # List: name -> levels

    x_mat <- as.matrix(st)

    # Attach as attributes to the matrix
    attr(x_mat, "col_vars") <- col_vars
    attr(x_mat, "row_vars") <- row_vars
  }
}
```

### 2. Update `.color_table_impl()` signature

Add parameters to receive the variable structure:

```r
.color_table_impl <- function(x,
                               x_orig = NULL,
                               formula = NULL,
                               col_vars = NULL,  # NEW: list of col variable name -> levels
                               row_vars = NULL,  # NEW: list of row variable name -> levels
                               values = "freq",
                               ...)
```

Or simpler: extract from attributes on `x` if present:

```r
# Inside .color_table_impl, early on:
col_vars <- attr(x, "col_vars")
row_vars <- attr(x, "row_vars")
has_multi_col_vars <- !is.null(col_vars) && length(col_vars) > 1
```

### 3. Build hierarchical column spanners

When `has_multi_col_vars` is TRUE, build nested spanners from the `col_vars` structure.

**Example**: For `col_vars = list(PremaritalSex = c("Yes", "No"), ExtramaritalSex = c("Yes", "No"))`

The column names in the matrix are: `"Yes_Yes", "Yes_No", "No_Yes", "No_No"`

We want to build:
```
         |        Yes         |         No         |   <- PremaritalSex (outer)
         |   Yes   |    No    |   Yes   |    No    |   <- ExtramaritalSex (inner)
```

**Implementation approach using `gt::tab_spanner()`:**

```r
if (has_multi_col_vars) {
  col_var_names <- names(col_vars)
  n_col_vars <- length(col_var_names)

  # The column names are combinations like "Yes_Yes", "Yes_No", etc.
  # Split them to understand the hierarchy
  split_cnames <- strsplit(cnames, "_", fixed = TRUE)

  # Build spanners from outermost to innermost
  # The FIRST col_var is the outermost (top-level spanner)
  # The LAST col_var is the innermost (appears as column labels)

  # For each level except the last (which becomes column labels)
  for (level in seq_len(n_col_vars - 1)) {
    # Get the value at this level for each column
    level_values <- sapply(split_cnames, `[`, level)

    # Group consecutive columns with the same value
    rle_result <- rle(level_values)

    end_pos <- cumsum(rle_result$lengths)
    start_pos <- c(1, end_pos[-length(end_pos)] + 1)

    for (i in seq_along(rle_result$values)) {
      group_cols <- cnames[start_pos[i]:end_pos[i]]
      spanner_label <- rle_result$values[i]

      gt_tbl <- gt_tbl |>
        gt::tab_spanner(
          label = spanner_label,
          columns = dplyr::all_of(group_cols),
          level = n_col_vars - level  # Higher level = more outer
        )
    }
  }

  # Optional: Add overall variable name as top-level spanner
  # This puts "PremaritalSex" above the "Yes" and "No" spanners
  gt_tbl <- gt_tbl |>
    gt::tab_spanner(
      label = col_var_names[1],
      columns = dplyr::all_of(cnames),
      level = n_col_vars
    )

} else {
  # Single column variable - current behavior
  gt_tbl <- gt_tbl |>
    gt::tab_spanner(
      label = cvar,
      columns = dplyr::all_of(cnames)
    )
}
```

### 4. Rename innermost column labels

The innermost variable values should become the actual column labels. Currently they show "Yes_Yes", but we want just "Yes" (the ExtramaritalSex value).

```r
if (has_multi_col_vars) {
  # Get the innermost (last) value from each column name
  inner_labels <- sapply(split_cnames, function(x) x[length(x)])

  # Rename columns in the gt table
  rename_list <- setNames(inner_labels, cnames)
  gt_tbl <- gt_tbl |> gt::cols_label(.list = rename_list)
}
```

### 5. Handle the "Total" column with margins

When `margins = TRUE`, exclude "Total" from spanner processing:

```r
if (has_multi_col_vars) {
  data_cnames <- setdiff(cnames, "Total")
  # ... apply spanner logic only to data_cnames ...
}
```

### 6. Where to show variable names

**Option A (Recommended)**: Put variable names in the spanner hierarchy
- Top spanner level shows first col_var name (e.g., "PremaritalSex")
- Second level shows first col_var values (e.g., "Yes", "No")
- Column labels show second col_var values

**Option B**: Put variable names in stub header area
- The empty cell above row labels could show column variable names
- This is what `print(structable())` does, but may be cramped

For now, Option A is simpler and uses gt's built-in spanner hierarchy.

## Files to Modify

1. **`R/color_table.R`**
   - `color_table.table()` (~line 209): Extract and attach col_vars/row_vars attributes
   - `color_table.structable()`: Similarly extract attributes
   - `.color_table_impl()` (~line 734): Replace single spanner with hierarchical logic

2. **`dev/test-color_table.R`** or new **`dev/presex-examples.R`**
   - Add test cases for multi-variable column spanners

## Edge Cases

1. **Underscores in factor levels**: If original levels contain "_", the split will fail.
   - Solution: Use a different delimiter in `as.matrix()` or document the limitation
   - `structable` uses `_` by default but this can be changed

2. **Three or more column variables**: The logic should generalize - just add more spanner levels

3. **Row variables**: This plan focuses on columns. Row hierarchy (the other TODO) would need `gt::row_group()` which is more complex.

## Test Cases

```r
library(vcd)
data(PreSex)

# Two column variables
color_table(PreSex,
            formula = MaritalStatus ~ PremaritalSex + ExtramaritalSex,
            title = "Two column vars with spanners")

# Two column variables, with margins
color_table(PreSex,
            formula = MaritalStatus ~ PremaritalSex + ExtramaritalSex,
            margins = TRUE,
            title = "Two column vars with margins")

# Swapped: two row variables, one column variable
color_table(PreSex,
            formula = MaritalStatus + Gender ~ PremaritalSex,
            title = "Two row vars (not yet handled)")

# Three column variables
color_table(PreSex,
            formula = Gender ~ MaritalStatus + PremaritalSex + ExtramaritalSex,
            title = "Three column vars")
```

## Expected Output

For `formula = MaritalStatus ~ PremaritalSex + ExtramaritalSex`:

**Before (current ugly output):**
```
|               | Yes_Yes | Yes_No | No_Yes | No_No |
| MaritalStatus |   ...   |  ...   |  ...   |  ...  |
```

**After (with hierarchical spanners):**
```
|               |         PremaritalSex          |
|               |      Yes      |      No        |
| MaritalStatus |  Yes  |  No   |  Yes  |   No   |  <- ExtramaritalSex values
```

## Implementation Order

1. First get the simple two-variable case working
2. Then generalize to three+ variables
3. Handle margins edge case
4. Consider the underscore delimiter issue
5. (Future) Address row variable hierarchy - separate TODO
