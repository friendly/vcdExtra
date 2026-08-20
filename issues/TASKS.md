# vcdExtra — development tasks

Broken out from the cross-package working list in `C:\Users\friendly\Dropbox\R\TASKS.md`
(2026-07-28). This package's local clone was significantly behind origin/master (HEAD was at
`c758cb9`, ~73 commits stale) when this file was created — pulled forward to `0dfa83b` first, so
the list below reflects the current repo state, not the older audit. Update here as items
are finished; sync back to the main list only if it's useful to see vcdExtra status at a glance.

**Updated 2026-08-20**: package is at v0.9.8 (unreleased; current CRAN version is 0.9.7), and
Michael is actively working toward a CRAN release of the accumulated 0.9.8 work — see "Release
checklist status" below for where that stands.

By far the messiest of the packages reviewed so far — uses `dev/`, `extra/`, *and* `issues/` as
scratch folders.

As these items are resolved, check them off as [X]

## Recent work (2026-08-17 to 2026-08-20)

- [X] **`CMHtest3()` `Df`/`Prob` bug** (#2, PR #26) — `CMHtest3()` (the internal `overall = TRUE`
  helper) extracted the overall `Df` column as an unnamed vector, so `Df[type]` returned `NA`
  whenever `types` wasn't the full default set of four. Reported by @bill-raynor (2018) and
  @imazubi (2026); @danielinteractive's fix in PR #26 was correct but the PR itself could no longer
  merge cleanly (`R/CMHtest.R` had been rewritten via a roxygen2md conversion after the PR was
  opened) — ported the one-line `stats::setNames()` fix directly onto `master`, plus PR #26's two
  regression tests and its `DESCRIPTION`/ORCID update for Daniel. PR #26 closed, #2 auto-closed.

- [X] **`drop1.loglm()`/`LRanova()`/`assoc_strength()`** — promoted from `dev/anova-like/` to `R/`.
  Term-level deletion tests for a `loglm` model's generating class (`drop1.loglm()`, also an S3
  method for `stats::drop1()`), a partial-R² wrapper (`LRanova()`), and a partial Cramer's V/Cohen's
  w wrapper (`assoc_strength()`). Comparison against `stats::drop1.glm()` on the equivalent Poisson
  GLM documented in `dev/anova-like/drop1-compare.R` and summarized in the `@details` of
  `drop1.loglm()`'s roxygen.

- [X] **pkgdown GHA workflow stuck queuing** — `.github/workflows/pkgdown.yaml`'s concurrency group
  intentionally collapses to the same literal group for every push (by design, to stop concurrent
  writes to `docs/`), but was missing `cancel-in-progress: true` — so pushes queued strictly FIFO
  instead of the newest push preempting a stale one, observed as a run stuck "pending" with zero
  steps started for 25+ minutes. Fixed by adding `cancel-in-progress: true`; confirmed working
  (a stuck run got cancelled immediately once a new push landed).

### Release checklist status (toward CRAN submission of 0.9.8)

Added `.release_checks.R` (adapted from `C:\R\Projects\heplots\.release_checks.R`) — see that
file's own header for what each `release_*()` step does. Local/automatable steps have all been run
once already:

- `release_preflight`, `release_document`, `release_spelling`: clean.

- `release_urls`: 20 URLs flagged. One real fix made (README's CRAN vignettes link wasn't in
  canonical form). Rest are noise: one `vignettes/vcd.bib` hit is a checker false-positive on an
  already-commented-out (`%%`) line; the other 15 in `vcd.bib` turned out to belong to citation keys
  that are never actually cited in any of the 9 vignettes (`vcd.bib` is inherited wholesale from
  `vcd`'s own bibliography, most entries unused here) — left alone per Michael's call, revisit later
  if CRAN's own incoming check flags them (raw `.bib` source ships in the tarball regardless of
  citation status, so it's still theoretically visible to CRAN even though invisible to readers).

- `release_site`: README re-rendered; pkgdown site confirmed live via the (now-fixed) GHA workflow.

- `release_build`: OK. Two non-blocking notes: a handful of vignettes emit `[WARNING] Citeproc:
  citation ref/fig not found` (looks like literal `@ref`/`@fig` text meant as bookdown-style
  cross-references, written without the backslash — cosmetic, not investigated further);
  `devtools::build_vignettes()` (used internally) is deprecated as of devtools 2.5.0, script may
  need updating eventually.

- `release_check`: **0 errors, 0 warnings, 0 notes.**

- `release_revdep`: **10/10 OK, 0 broken, 0 new problems** (junco's 1 pre-existing error is present
  under CRAN's current vcdExtra too, confirmed via `revdep/cran.md`'s "0 new problems").

- `release_check_win` (win-builder): **blocked** — both this session's sandbox and Michael's own
  network get a hard connection timeout hitting `win-builder.r-project.org` specifically (confirmed
  via direct `curl`: `cran.r-project.org` responds fine in 0.4s, win-builder times out completely on
  both HTTPS and FTP). Not a transient retry-fixable issue. Worth trying again later, or consider
  adding a `release_check_rhub()` step (`rhub::rhub_check()`, GitHub-Actions-based, no FTP
  dependency) as an alternative — flagged as a TODO in heplots' own `.release_checks.R` too, so
  worth doing once and reusing the pattern across both packages.

- `release_cran_comments`: not yet run (wants `release_check_win`/`release_revdep` output first,
  though revdep's part is now ready).

Still manual/not started: bump `DESCRIPTION` `Version`/`Date` right before actual submission (not
before — see `release_preflight()`'s own staleness warning), review/finalize `cran-comments.md`,
`devtools::release()`.

## `vcd` package migration (2026-07-29, updated 2026-07-30)

David Meyer (maintainer of `vcd`) migrated its repo off R-Forge (going end-of-life) to Codeberg:
https://codeberg.org/davidjohannesmeyer/vcd (git-based). **This is now the authoritative source
for `vcd`.**

Cloned fresh to `C:\Dropbox\R\projects\vcd` (2026-07-30) — `main` branch, SSH auth confirmed
working (reused the existing GitHub SSH key, added to Codeberg account settings), collaborator
access confirmed. Opens directly via `vcd.Rproj`.

Two stale copies to be aware of, both superseded, neither touched further:
- `C:\Dropbox\R\packages\vcd` — an extracted package/tarball dump (no `.git`), not a dev checkout.
- `C:\Dropbox\R\projects\vcd-svn-old-2016` — an old R-Forge-era **SVN** checkout (has `.svn`),
  found unexpectedly already sitting at the `projects\vcd` path and renamed aside to make room for
  the fresh git clone.

Codeberg runs on **Forgejo** (a GitHub-like forge), so the workflow is close to GitHub's:

- Add an SSH key under Account Settings → SSH/GPG Keys (can reuse the existing GitHub key), or use
  HTTPS with a personal access token (no plain password auth, same as GitHub).
- As a collaborator, direct push access to `davidjohannesmeyer/vcd` should work — no fork needed:
  `git clone git@codeberg.org:davidjohannesmeyer/vcd.git`.
- From there it's normal git — branch, commit, push; open a PR from the web UI's "New Pull
  Request" button if direct pushes to `main` aren't allowed.
- Optional: `tea` is the official Forgejo/Gitea CLI (like GitHub's `gh`), for PR/issue management
  from the terminal — not required, the web UI covers the same ground.

Once comfortable with that workflow, consider replicating/moving some of the `vcd`-enhancing work
currently living in `vcdExtra` over to that repo directly — e.g. the `shading_marimekko()` idea
below, which extends a function in `vcd` itself rather than in `vcdExtra`.

## Migrate back to vcd

Functions in `vcdExtra` that supersede or extend a same-named function in `vcd` (`vcd` being
otherwise inactive) — candidates for PRs to the new Codeberg repo instead of staying
vcdExtra-only. Confirmed via `library(vcdExtra)`'s own startup message ("Registered S3 methods
overwritten by 'vcdExtra': `pairs.table`, `print.Kappa`"; "masked from 'package:vcd':
`pairs_diagonal_mosaic`, `woolf_test`") plus each file's own roxygen documentation.

**Note (2026-07-30)**: the Codeberg `vcd` repo doesn't use roxygen at all — docs are hand-written
`.Rd` files. Any PR moving one of these over needs a corresponding `man/*.Rd` submitted alongside
the `R/*.R` file, not just the roxygen-commented source; the `.Rd` would need to be hand-written
(or roxygen-generated locally then treated as the deliverable, since roxygen itself won't be part
of `vcd`'s build).

- [ ] **`R/woolf_test.R`** — `woolf_test()` supersedes `vcd::woolf_test()`, adding a `decompose`
  option: for a 2×2×R×C table, decomposes the overall homogeneity test into row effects, column
  effects, and residual (interaction), analogous to a two-way ANOVA. The function's own docs call
  this decomposition "a novel extension... appears [not to exist] in the existing literature."

- [ ] **`R/pairs_diagonal_mosaic.R`** — `pairs_diagonal_mosaic()` is "an enhanced replacement for
  `vcd::pairs_diagonal_mosaic()`": fixes two bugs where the original hardcoded/ignored its
  `labeling` and `alternate_labels` arguments, and changes the default labeling scheme from
  `labeling_values` to `labeling_border` (so cell counts are off by default). Also provides a
  companion `pairs.table()` S3 method using this fixed version as the default diagonal panel —
  this is what overwrites `vcd`'s `pairs.table` method.

- [ ] **`R/print.Kappa.R`** — `print.Kappa()` is "a replacement for the `print.Kappa` method in
  `vcd`," adding display of `z` values and an optional `CI` argument for confidence intervals,
  neither present in `vcd`'s version.

## TODOs

- [X] **Breslow-Day test** — shipped 2026-07-30 as `breslow_day_test()`, adapted from
  `DescTools::BreslowDayTest`. How: verified numerically against `DescTools::BreslowDayTest()`
  (`dev/test-breslow_day_test.R`, incl. its `decompose = TRUE` row/col/residual breakdown
  matching `woolf_test()`'s structure) before shipping; along the way fixed a stray `names`
  attribute bug that was masquerading as a precision issue. Added attribution (Michael Höhle's
  original algorithm, via Andri Signorell's `DescTools`, GPL (>= 2)) and an `@author` tag; moved
  `dev/breslow_day_test.R` → `R/breslow_day_test.R` (`git mv`); ran `devtools::document()`; added
  to `_pkgdown.yml`'s Statistical tests section; added a `NEWS.md` entry; fixed an unrelated
  `R CMD check` NOTE found along the way (`data-raw` missing from `.Rbuildignore`); deleted the
  now-redundant raw `dev/BreslowDayTest.R` reference copy. Formal `testthat` coverage deliberately
  deferred — `dev/test-breslow_day_test.R` stands in as the verification record for now.
  See the general step-by-step process this followed, noted in `TASKS-all.md`.

- [ ] **Hurdle-model test/methods** — adapted from `pscl::hurdletest`; unclear final intent, not
  shipped. Files: `dev/hurdletest.R`, `dev/hurdle-methods.R`, `dev/hurdle-test.R`

- [X] **Log-histogram plot** — shipped 2026-08-19 as `R/logist_plot.R` (moved from
  `dev/loghistplot/logist-plot.R`, formerly "v3"). `logist_plot()` (generic, with vector/
  data.frame/formula methods) plots a `glm(y ~ x, family = binomial)` fit with a representation of
  the marginal distribution of `x` within each response group — mirrored histograms
  (`marginal = "hist"`), filled density estimates (`"density"`), or jittered points (`"points"`),
  per Smart et al. (2004). `logist_hist()`/`logist_point()`/`logist_density()` are fixed-`marginal=`
  convenience wrappers. Also shipped: optional `group=` for grouped fits/marginals with
  `group.colors=`; `fit.args=`/`marginal.args=` scoped-list layer customization (design in
  `dev/loghistplot/implemented-plans/forwarding.md`); `marginal.height=` to control the vertical
  space given to the marginal display (design in `dev/loghistplot/marginal_height.md`). `ggplot2`
  moved from `Suggests` to `Imports` accordingly. 46 `testthat` tests added
  (`tests/testthat/test-logist_plot.R`). A copy is kept at `dev/loghistplot/logist-plot.R` pending
  cleanup (see Clean-up candidates below); `dev/loghistplot/loghistplot4.R` (an alternate,
  thinner-strip grouped-density rendering, kept for visual comparison — never shipped) and
  `dev/loghistplot/logist-plot-history.md` (the file's development log, split out before promotion
  to `R/`) are still there too. **Deferred, not yet implemented**: expressing `group=` as
  `y ~ x | group` in the formula, matching `CMHtest()`'s `|`-strata convention — design notes in
  `dev/loghistplot/formula-groups.md` (base R's `model.frame()`/`terms()` don't support `|` as a
  conditioning operator at all, so this needs hand-parsing like `CMHtest.formula()` already does,
  not a `model.frame()` trick).

- [ ] **New shading idea**: `shading_marimekko()` — a non-residual-based mosaic shading using distinct
  colors per split (like `ggmosaic`), sketched in `dev/vcdExtra-new.md`; would extend a function
  in the separate `vcd` package (`C:\Dropbox\R\packages\vcd\R\shadings.R`). Candidate for
  contributing directly to `vcd`'s new Codeberg repo instead of `vcdExtra` — see note above.

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

- [X] **`Summarise()` vs `LRstats()` duplication** — resolved 2026-08-20. Motivated by `Summarise()`'s
  capitalized name being a near-collision with `dplyr::summarise()`, which vcdExtra also imports
  internally (confirmed: `NAMESPACE` has `importFrom(dplyr, ..., summarise)` alongside
  `export(Summarise)`). Activated the previously-commented-out `.Deprecated("LRstats", package =
  "vcdExtra")` call in `Summarise()`'s generic (fires on every call regardless of dispatch method);
  added `R/vcdExtra-deprecated.R` (the `?vcdExtra-deprecated` topic `.Deprecated()` already pointed
  to but which never existed); updated `Summarise()`'s roxygen with a deprecation notice pointing to
  `LRstats()`; NEWS entry added. No internal callers (`R/`, `tests/`, `vignettes/`) needed updating.
  Note: don't confuse with the lowercase `summarise()` generic, which was a *different*, older,
  already-fully-removed function (see clean-up list below) — that was a separate, already-resolved
  item.

- [ ] `issues/improvement-suggestions.md` — a large Dec-2025 backlog audit (12 sections: testing, CI,
  performance, docs, community...) written against v0.8-7. Partly stale now (package is v0.9.7,
  CI already exists via `.github/`), but several items look still open (test coverage, error
  messages, new vignettes). Treat as a standing backlog reference rather than itemized here.

- [X] `issues/vcd-extensions.Rmd` was an early draft of ways to extend CDA. It mentions a CRAN Task View,
  tidyCDA enhancements, woolf_test and other topics. Keep for future reference.

- [ ] Not read in full, still unverified: `issues/anova-like.md`, `issues/assocstats.md`


## Clean-up candidates

Identified, not yet deleted (held for manual review).

- [X] `issues/CMHtest/` (`CMH-test-fix.R`, `CMHtest-new.R`, `CMHtest-old.R`, `CMHtest_issue.Rmd`) —
  `CMHtest.R` shipped long ago. -> Leave these for now

- [X] `issues/data-roxygenize.R`, `issues/fix_roxy.R`, `issues/fix_roxygen_items.R` — one-time
  roxygen migration scripts. Moved to `issues/roxygenize/` (2026-07-30) — keep for reference.

- [ ] `issues/mcaplot-debug.R`, `issues/mjca-cacoord-bug.Rmd` — `mcaplot()` bugs fixed per NEWS
  v0.9.3/0.9.4.

- [ ] `dev/pairstable.R`, `dev/test-pairs-diag.R` — superseded by shipped
  `R/pairs_diagonal_mosaic.R`.
- [ ] `dev/labeling_points-test.R` — `labeling_points()` shipped (keep `-plan.md`, referenced by the
  vignette TODO above).

- [X] `extra/vignettes-new/mobility.Rmd` — superseded, `a6-mobility.Rmd` already shipped in
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

- [X] an initial sketch of a novel 2x2 display for CMHtest, `dev/CMH-2x2.md`. Implemented in new print methods
  for CMHtest().  

## Documentation

- [X] `vignettes/tidyCats.Rmd` — added a note at the top pointing readers to
  `a1a-convert-collapse.Rmd` ("Steps Toward Tidy Categorical Data Analysis"), since Gavin
  Klorfine's `as_*()`/`collapse_levels()` work has now accomplished much of what this vignette
  originally proposed.


## Build gotcha: `README.md` / `devtools::build_readme()`

Found 2026-07-30: the pkgdown site's homepage showed a stale "Version 0.9.6; documentation built
for `pkgdown` 2026-07-01" (from `README.md`) even right after a successful pkgdown GHA build. Root
cause: pkgdown copies `README.md` as-is — it does **not** re-knit `README.Rmd` — so a stale
committed `README.md` stays stale no matter how many times the site rebuilds. `README.Rmd`'s
version/date line is dynamic (`` `r getNamespaceVersion("vcdExtra")` ``, `` `r Sys.Date()` ``), so
it only updates when someone re-knits it locally and commits the result.

`.build-steps.R` already has the right shape for this — install with vignettes, then rebuild the
README if stale:
```r
# The README.Rmd references the vignettes, so they must be installed first
devtools::install(build_vignettes = TRUE)

# build the README.md if it is older than README.Rmd
if (!file.exists("README.md") || file.mtime("README.Rmd") > file.mtime("README.md")) {
  devtools::build_readme()
}
```

**However**: reproduced on 2026-07-30 (fresh `Rscript` sessions) that `devtools::build_readme()`
*still fails* even right after `devtools::install(build_vignettes = TRUE)`, with:
```
Error in `$<-.data.frame`(...) : replacement has 1 row, data has 0
```
from the `vignettes` chunk in `README.Rmd` (`tools::getVignetteInfo("vcdExtra")` returns 0 rows in
that context). Cause: `build_readme()` renders via `pkgload::load_all()` (a dev/source-loaded
namespace) rather than the installed library, and `load_all()` doesn't populate the installed
vignette index that `getVignetteInfo()` reads from — so the chunk sees 0 vignettes and crashes
assigning a length-1 `paste0()` result onto a 0-row column.

**Workaround that worked**: after `devtools::install(build_vignettes = TRUE)`, call
`rmarkdown::render("README.Rmd")` **directly** instead of `devtools::build_readme()`.

- [ ] Worth checking whether this is new (a `devtools`/`pkgload` version change) or has always been
  broken and `.build-steps.R` simply hasn't been run end-to-end recently.
- [ ] Consider hardening the `vignettes` chunk in `README.Rmd` itself (e.g. skip/guard when
  `nrow(vigns) == 0`) so it degrades gracefully instead of crashing the whole render, regardless of
  which tool renders it.

