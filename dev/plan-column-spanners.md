# Implementation Plan: Column Spanner Headings for color_table()

## Problem Statement

When `color_table()` is used with a formula that has multiple variables on the column side (RHS), the resulting column names are concatenated with underscores (e.g., `"Yes_Yes"`, `"Yes_No"`), which is confusing and ugly.

**Examples that currently produce poor output:**
```r
color_table(PreSex, formula = MaritalStatus + Gender ~ PremaritalSex + ExtramaritalSex)
color_table(PreSex, formula = Gender + PremaritalSex + ExtramaritalSex ~ MaritalStatus)
```

## Proposed Solution

Use `gt::tab_spanner_delim()` to automatically parse the underscore-delimited column names and create hierarchical column headers. This function:
- Splits column names on a delimiter (`"_"`)
- Creates nested spanner labels automatically
- Coalesces identical values at each hierarchy level

## Implementation Details

### Step 1: Detect Multi-Variable Columns

In `.color_table_impl()`, after line 484 where `cnames` is extracted, add logic to detect whether the columns contain multiple variables:

```r
# Check if column names have multiple variables (contain underscore delimiter)
has_multi_col_vars <- any(grepl("_", cnames, fixed = TRUE))
```

### Step 2: Extract Column Variable Names (Optional Enhancement)

For better spanner labeling, extract the original column variable names from the formula or structable. The `cvar` currently contains names like `"PremaritalSex_ExtramaritalSex"` which can be split:

```r
# Parse column variable names from cvar
col_var_names <- strsplit(cvar, "_", fixed = TRUE)[[1]]
```

### Step 3: Modify Spanner Creation Logic

Replace the current spanner code at lines 732-737:

**Current code:**
```r
# Add column spanner for the column variable name
gt_tbl <- gt_tbl |>
  gt::tab_spanner(
    label = cvar,
    columns = dplyr::all_of(cnames)
  )
```

**New code:**
```r
# Add column spanners - use hierarchical spanners for multi-variable columns
if (has_multi_col_vars) {
  # Use tab_spanner_delim to create nested hierarchy from underscore-separated names
  gt_tbl <- gt_tbl |>
    gt::tab_spanner_delim(
      delim = "_",
      columns = dplyr::all_of(cnames),
      split = "last"  # rightmost becomes column label, leftmost becomes spanner
    )
} else {
  # Single column variable - use simple spanner
  gt_tbl <- gt_tbl |>
    gt::tab_spanner(
      label = cvar,
      columns = dplyr::all_of(cnames)
    )
}
```

### Step 4: Handle the "Total" Column

When `margins = TRUE`, the "Total" column must be excluded from the delimiter-based spanner parsing:

```r
if (has_multi_col_vars) {
  # Exclude Total column from spanner_delim processing
  data_cnames <- setdiff(cnames, "Total")
  gt_tbl <- gt_tbl |>
    gt::tab_spanner_delim(
      delim = "_",
      columns = dplyr::all_of(data_cnames),
      split = "last"
    )
} else {
  # Single column variable - use simple spanner
  gt_tbl <- gt_tbl |>
    gt::tab_spanner(
      label = cvar,
      columns = dplyr::all_of(cnames)
    )
}
```

### Step 5: Edge Case - Underscore in Original Level Names

If the original factor levels contain underscores (e.g., `"high_school"`), `tab_spanner_delim()` will incorrectly split them. Two options:

**Option A (Recommended):** Use a different delimiter that won't appear in level names. Modify the `as.matrix()` call for structable to use `sep = "|"` or another delimiter:
- Requires passing the separator through and coordinating with structable

**Option B:** Document this as a known limitation - factor levels should not contain underscores when using multi-variable column formulas.

### Step 6: Test Cases

Add test cases to `dev/test-color_table.R`:

```r
# Test multi-variable columns with spanners
cat("\n=== Testing Column Spanners ===\n")

# Two column variables
color_table(PreSex,
            formula = MaritalStatus ~ PremaritalSex + ExtramaritalSex,
            title = "Two column vars: PremaritalSex x ExtramaritalSex")

# Three column variables
color_table(PreSex,
            formula = Gender ~ MaritalStatus + PremaritalSex + ExtramaritalSex,
            title = "Three column vars with spanners")

# With margins
color_table(PreSex,
            formula = MaritalStatus ~ PremaritalSex + ExtramaritalSex,
            margins = TRUE,
            title = "Multi-column with margins")
```

## Files to Modify

1. **`R/color_table.R`**
   - Lines ~480-496: Add `has_multi_col_vars` detection
   - Lines ~732-737: Replace spanner logic with conditional multi-variable handling

2. **`dev/test-color_table.R`**
   - Add new test cases for multi-variable column spanners

## Alternative: Manual Spanner Construction

If `tab_spanner_delim()` doesn't provide enough control, manually construct spanners:

```r
if (has_multi_col_vars) {
  # Split column names
  split_names <- strsplit(cnames, "_", fixed = TRUE)
  n_col_vars <- length(split_names[[1]])

  # Build spanners from outermost to innermost
  for (var_idx in 1:(n_col_vars - 1)) {
    # Get parent-level values at this position
    parent_vals <- sapply(split_names, `[`, var_idx)

    # Find groups of consecutive columns with same parent
    rle_groups <- rle(parent_vals)

    end_positions <- cumsum(rle_groups$lengths)
    start_positions <- c(1, end_positions[-length(end_positions)] + 1)

    for (i in seq_along(rle_groups$values)) {
      group_cols <- cnames[start_positions[i]:end_positions[i]]
      gt_tbl <- gt_tbl |>
        gt::tab_spanner(
          label = rle_groups$values[i],
          columns = dplyr::all_of(group_cols),
          level = n_col_vars - var_idx  # Outer vars at higher levels
        )
    }
  }
}
```

## Expected Result

For `formula = MaritalStatus ~ PremaritalSex + ExtramaritalSex`:

**Before (ugly):**
```
| MaritalStatus | Yes_Yes | Yes_No | No_Yes | No_No |
```

**After (with spanners):**
```
|               |    PremaritalSex    |    PremaritalSex    |
| MaritalStatus |   Yes   |    No   |   Yes   |    No   |
```

Or with proper nesting:
```
|               |       Yes       |       No        |
| MaritalStatus |   Yes  |   No   |   Yes  |   No   |
```

Where the top level shows PremaritalSex values and the bottom shows ExtramaritalSex values.

## Complexity Assessment

- **Effort:** Medium - Straightforward use of existing gt functionality
- **Risk:** Low - Changes are isolated to display formatting
- **Testing:** Manual visual inspection required for spanner appearance
