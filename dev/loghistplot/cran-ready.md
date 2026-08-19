# CRAN readiness notes for `logist_plot()`

## Summary

The plotting implementation is behaviorally solid, but it is not yet ready to move directly into `R/` and submit to CRAN. The function needs two small package-check fixes and permanent tests before promotion. The package also has pre-existing vignette issues that must be resolved before a CRAN submission.

## Stress-test results

The following checks passed:

- 60 randomized ungrouped datasets across histogram, points, and density marginals, with randomized valid values of `marginal.height`, `bins`, and `adjust`.
- 35 randomized grouped datasets containing two to six groups, testing grouped points and density marginals across `marginal.height` values from 0.002 to 1.
- 20 random seeds at 11 histogram heights, checking count-axis symmetry, positions, and minimum label spacing.
- Numeric, integer, logical, factor, ordered-factor, and character binary responses.
- Vector, data-frame, formula, named-formula, and convenience-wrapper interfaces.
- Column selectors, missing and infinite values, response direction, and stable group ordering after row shuffling.
- Invalid dimensions, response encodings, selectors, formulas, heights, bins, density adjustments, grouping values, colors, layer arguments, and nonempty `...`.
- Plot construction through `ggplotGrob()` during randomized testing.

Histogram count labels remained symmetric and correctly positioned. At small `marginal.height` values, the adaptive breaks reduced the labels sufficiently to prevent overlap. The secondary-axis guide retains `check.overlap = TRUE` as a final rendering safeguard.

Grouped-density curves also retained clearance from the plot border at extreme lane heights; the outline-padding floor prevented the outermost curve from intersecting the border.

Fixed-seed plots using the default settings were rendered before and after the `marginal.height` work. Histograms, ungrouped densities, grouped densities, and grouped points were byte-identical, confirming that the defaults did not change existing output.

## Package lifecycle verification

An isolated package copy was used so the working tree was not altered. In that copy, `logist-plot.R` was placed in `R/` and the following lifecycle was exercised:

1. Generate documentation and namespace entries with roxygen2.
2. Build the source package.
3. Install the built package.
4. Exercise exported functions and S3 dispatch from the installed package.
5. Render histogram, points, density, and grouped-density plots from the installed package.
6. Run examples, the existing test suite, and `R CMD check`.

Roxygen generated the expected exports, S3 registrations, and `man/logist_plot.Rd`. The package built and installed successfully, installed-package dispatch worked, and the plotting examples rendered.

With the current source, `R CMD check --no-manual --ignore-vignettes` produced one NOTE for unqualified `head()` and `tail()` calls. In a second isolated copy, only those calls and the ggplot2 dependency placement were corrected. The package was then documented, built, installed, and checked successfully:

```text
Status: OK
```

## Required before moving into `R/`

✅  [**GK: DONE**] 1. Qualify the four calls around the histogram-bin construction as `utils::head()` and `utils::tail()`, or add an explicit `utils` import. The current unqualified calls cause an `R CMD check` NOTE.

2. Move `ggplot2 (>= 3.4.0)` from `Suggests` to `Imports` in `DESCRIPTION`. Every execution path requires ggplot2, the function returns a ggplot object, and its examples use ggplot2 unconditionally.

3. Add permanent `testthat` coverage. The stress tests described above were temporary and do not currently protect the package. At minimum, the permanent tests should cover:

   - all three marginal modes;
   - vector, data-frame, formula, and convenience-wrapper interfaces;
   - accepted binary-response encodings;
   - grouped points and grouped densities;
   - default and custom `marginal.height` geometry;
   - histogram count-axis breaks, labels, symmetry, and spacing at small heights;
   - grouped-density border clearance;
   - warnings and validation errors, including the ignored points height;
   - successful plot building or grob rendering.

4. Move `dev/loghistplot/logist-plot.R` into `R/`, regenerate `NAMESPACE` and the Rd documentation, and review the generated files.

5. Add a NEWS entry describing the new functions

6. Run documentation with roxygen2 8.1.0, as configured in `DESCRIPTION`, or deliberately align the configured version. Documentation generated with the locally installed roxygen2 8.0.0 produced a version warning.

7. Consider trimming the long development and review history from the promoted source file. This is source hygiene rather than a technical CRAN blocker.

## Existing package-wide CRAN issues

These findings are not caused by `logist_plot()`, but they prevent declaring the package ready for CRAN:

- A full check fails while building `datasets.Rmd` because `here::here("inst", "extdata", "vcdExtra-datasets.xlsx")` resolves incorrectly in the check directory and the workbook cannot be found.
- The CRAN incoming check reports that the package has a `VignetteBuilder` field but no prebuilt vignette index.
- A complete `--as-cran` run should be repeated on the standard submission platforms after the vignette issues and function-integration work are complete.

## Readiness decision

The function itself is ready for promotion after the dependency/import correction, qualification of `head()` and `tail()`, and addition of permanent tests. The package as a whole is not yet CRAN-ready because of the existing vignette failures.
