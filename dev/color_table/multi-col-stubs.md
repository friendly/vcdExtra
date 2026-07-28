# Color_table: multi-column stubs for row labels
#

The TODO at lines 39:47 in R/color_table.R describe the problem: Two or more row variables should
appear as a nested hierarchy, rather than being labeled with the concatenation of their
row variables, eg "Black-Male"

Here's an example

```
# 3-way table with a formula to specify the layout
color_table(HairEyeColor,
            formula  = Eye ~ Hair + Sex,
            legend   = TRUE,
            title    = "Hair × Eye × Sex (complete independence residuals)")
```

The post: https://zimanaanalytics.medium.com/whats-new-in-gt-multi-column-stubs-row-wise-summaries-and-functions-since-version-0-11-e7b4823fb21a
describes the use of `rowname_col()` with a _vector_ of column names, that seems to do what I want.

Try this first in a copy, `dev/color_table/color_table.R` and include several new test cases for this in a separate test file.
