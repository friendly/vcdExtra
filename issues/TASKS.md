# vcdExtra — development tasks

Broken out from the cross-package working list in `C:\Users\friendly\Dropbox\R\TASKS.md`
(2026-07-28). This package's local clone was significantly behind origin/master (HEAD was at
`c758cb9`, ~73 commits stale) when this file was created — pulled forward to `0dfa83b` first, so
the list below reflects the current repo state (v0.9.7), not the older audit. Update here as items
are finished; sync back to the main list only if it's useful to see vcdExtra status at a glance.

By far the messiest of the packages reviewed so far — uses `dev/`, `extra/`, *and* `issues/` as
scratch folders.

As these items are resolved, check them off as [X]

## TODOs

- [ ] **Breslow-Day test** — adapted from `DescTools::BreslowDayTest`, not yet exported/added to the
  package. Still unshipped even after the pull brought in commit `0fc3d3d implement
  dev/breslow_day_test.R` (that commit updated the dev script itself, not `R/`).
  Files: `dev/BreslowDayTest.R`, `dev/breslow_day_test.R`

- [ ] **Hurdle-model test/methods** — adapted from `pscl::hurdletest`; unclear final intent, not
  shipped. Files: `dev/hurdletest.R`, `dev/hurdle-methods.R`, `dev/hurdle-test.R`

- [ ] **Log-histogram plot** — no NEWS mention, not shipped. This is an idea to display logistic regression fits
  with histograms of the 0/1 values at bottom/top. File: `dev/loghistplot.R`

- [ ] **New shading idea**: `shading_marimekko()` — a non-residual-based mosaic shading using distinct
  colors per split (like `ggmosaic`), sketched in `dev/vcdExtra-new.md`; would extend a function
  in the separate `vcd` package (`C:\Dropbox\R\packages\vcd\R\shadings.R`).

- [ ] **Vignette on `labeling_points()`** — the function shipped (v0.9.1) but the planned vignette
  illustrating dot-density mosaics (per `dev/vcdExtra-new.md`) was never written.
  Files: `dev/vcdExtra-new.md`, `dev/labeling_points-plan.md`

- [ ] **Density displays article/vignette (new)** — draft notes for an article ("Density Displays for
  Categorical Data" / "Visual Attributes for Categorical Data") using `labeling_points()` to show
  density of points as a visual attribute for frequency tables. Overlaps with the dot-density
  vignette item above — worth merging the two efforts.
  File: `dev/density-diplays.md`

- [ ] **Better labeling of `loglmlist`/`glmlist` objects (new)** — notes on the model formulas for
  submodels in a `loglmlist`/`glmlist` being inaccessible to other functions. May be partially
  addressed already by the now-shipped `get_model()`/`get_models()` (exported, in `R/get_model.R`)
  — check whether this note is still live or can be closed out.
  File: `dev/label-loglmlist.md`

- [ ] **Draft vignette not promoted**: `extra/vignettes-new/demo-occStatus.Rmd` ("Occupational
  Status") doesn't exist in the real `vignettes/` folder.
- [ ] `R/mosaic3d.R` — 5 live `# TODO/FIXME` comments in the source: formula interface, zero-margin
  handling, alpha transparency for side walls, an interline-gap kludge, passing `labeling_args`.
- [ ] `R/CMHtest.R` — 2 live TODOs in the source: better p-value printing, determining/labeling score
  types (integer vs midrank).

- [ ] **`Summarise()` vs `LRstats()` duplication** — `Summarise.R` is still a near-duplicate of
  `LRstats.R`, never formally deprecated (no `vcdExtra-deprecated.R` exists; the `.Deprecated()`
  call in `Summarise()` is commented out). Flagged in `issues/improvement-suggestions.md` as
  unresolved. Note: don't confuse with the lowercase `summarise()` generic, which *is* already
  deprecated (see clean-up list below) — that's a separate, already-resolved item.
- [ ] `issues/improvement-suggestions.md` — a large Dec-2025 backlog audit (12 sections: testing, CI,
  performance, docs, community...) written against v0.8-7. Partly stale now (package is v0.9.7,
  CI already exists via `.github/`), but several items look still open (test coverage, error
  messages, new vignettes). Treat as a standing backlog reference rather than itemized here.

- [ ] Not read in full, still unverified: `issues/anova-like.md`, `issues/assocstats.md`,
  `issues/vcd-extensions.Rmd` (688KB — unclear if draft vignette or notes).

## Clean-up candidates

Identified, not yet deleted (held for manual review).

- [ ] `issues/CMHtest/` (`CMH-test-fix.R`, `CMHtest-new.R`, `CMHtest-old.R`, `CMHtest_issue.Rmd`) —
  `CMHtest.R` shipped long ago.

- [ ] `issues/data-roxygenize.R`, `issues/fix_roxy.R`, `issues/fix_roxygen_items.R` — one-time roxygen
  migration scripts.

- [ ] `issues/mcaplot-debug.R`, `issues/mjca-cacoord-bug.Rmd` — `mcaplot()` bugs fixed per NEWS
  v0.9.3/0.9.4.

- [ ] `dev/pairstable.R`, `dev/test-pairs-diag.R` — superseded by shipped
  `R/pairs_diagonal_mosaic.R`.
- [ ] `dev/labeling_points-test.R` — `labeling_points()` shipped (keep `-plan.md`, referenced by the
  vignette TODO above).

- [ ] `extra/vignettes-new/mobility.Rmd` — superseded, `a6-mobility.Rmd` already shipped in
  `vignettes/`.

- [ ] `dev/color_table/color_table-plan.md`, `color_table.md` and `dev/assoc_graph/assoc-graph.md`,
  `assoc-rendering.md` — design notes for now-shipped features (color_table's multi-row-stub
  support shipped in v0.9.7); worth a skim before deleting.

- [ ] `extra/include_gt_test.{Rmd,html}`, `extra/knit_include_test.{Rmd,html}` — demo/test docs for
  shipped `knit_include()`.

- [X] **`R/summarise-old.R` (new)** — leftover from deprecating the lowercase `summarise()` generic
  (commits: "rename summarise -> summarise-old", "summarise() is now deprecated"). Sitting in
  `R/` rather than `dev/`, so it still ships as internal package code even though dead. Confirm
  nothing calls it, then either delete or move to `dev/`.

## Worked examples / infra (not classified)

- [X] `extra/Asbestos.R`, `housetasks.R`,  -> moved to `data-raw/`
- [X] `dev/presex-examples.R` -> `dev/color_table/`
- [X] `extra/datasets/` (active infra for the pkgdown dataset table, keep for reference)
- [X] `extra/mental-ex.R` (lines 29-57: linlin/roweff/coleff/rowcol model comparison) -> examples
  added to the `Mental` dataset docs in `R/data.R` (mosaic plot call omitted)
- [X] `birthwt.R`, retained; could be used an an example of collapsing levels with `dplyr`

`titanicp-recode.R`, `sim*.R`,
`vgam-lvplot-ex.R`, `dev/gRbase-example-AI.R`, `dev/Butterfly-ex.R`

## New work

- an initial sketch of a novel 2x2 diplay for CMHtest, `dev/CMH-2x2.md`.

## Documentation

- [X] `vignettes/tidyCats.Rmd` — added a note at the top pointing readers to
  `a1a-convert-collapse.Rmd` ("Steps Toward Tidy Categorical Data Analysis"), since Gavin
  Klorfine's `as_*()`/`collapse_levels()` work has now accomplished much of what this vignette
  originally proposed.

