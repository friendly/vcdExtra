# CRAN release checklist for vcdExtra, organized as discrete, re-runnable
# steps instead of one long linear script (see .build-steps.R for the
# original). Source this file to define the functions below, then either
# call release_run_all() for the full automatable sequence, or run
# individual release_*() functions to retry just the step that failed /
# needs re-checking.
#
# Adapted from heplots' C:\R\Projects\heplots\.release_checks.R (2026-08-11
# version) -- functions are package-agnostic (read DESCRIPTION/NEWS.md
# directly), only release_cran_comments()'s `known_issues` default differs
# (heplots has a standing vignette-URL issue; vcdExtra has none known yet,
# so it defaults to NULL here).
#
# See issues/TASKS.md's "Build gotcha: README.md / devtools::build_readme()"
# entry before running release_site() -- devtools::build_readme() crashes
# on the vignettes chunk unless the package is already installed with
# vignettes (devtools::install(build_vignettes = TRUE)) first; the
# documented workaround is rmarkdown::render("README.Rmd") directly.

library(devtools)

# keep rgl from popping up windows during examples/vignettes
Sys.setenv(RGL_USE_NULL = TRUE)

# ---- 1. Pre-flight: versions & NEWS ----------------------------------------

#' Cross-check DESCRIPTION's Version/Date against NEWS.md and CRAN
release_preflight <- function() {
  desc <- read.dcf("DESCRIPTION", fields = c("Package", "Version", "Date"))
  package <- desc[1, "Package"]
  version <- desc[1, "Version"]
  date <- desc[1, "Date"]
  news_top <- readLines("NEWS.md", n = 1)

  cat(glue::glue("DESCRIPTION: Version {version}, Date {date}\n"))
  cat(glue::glue("NEWS.md top line: {news_top}\n"))

  # CRAN's incoming-feasibility check flags this at ~1 month old
  date_age <- as.numeric(Sys.Date() - as.Date(date))
  if (is.na(date_age) || date_age > 30) {
    warning(glue::glue(
      "DESCRIPTION's Date field ({date}) is {date_age} days old -- CRAN ",
      "flags this once it's over a month old. Bump it right before actual ",
      "submission (not earlier, or it'll just go stale again)."
    ))
  }

  if (!grepl(version, news_top, fixed = TRUE)) {
    warning(glue::glue(
      "NEWS.md's first line doesn't mention version {version} -- did you ",
      "forget to add a NEWS entry, or to bump Version/Date in DESCRIPTION?"
    ))
  }

  cran_version <- tryCatch({
    avail <- utils::available.packages(repos = "https://cran.r-project.org/")
    unname(avail[package, "Version"])
  }, error = function(e) NA)
  cat(glue::glue("Version currently on CRAN: {cran_version}\n"))

  invisible(list(package = package, version = version, date = date, cran_version = cran_version))
}

# ---- 2. Documentation & spelling -------------------------------------------

release_document <- function() {
  devtools::document()
}

#' @param update Replace inst/WORDLIST wholesale via
#'        spelling::update_wordlist()? Default FALSE, and deliberately
#'        hard to trigger by accident -- see the warning below.
#'
#' Careful: spelling::update_wordlist() doesn't add *selected* words, it
#' replaces the ENTIRE wordlist with whatever spell_check_package() finds
#' at that moment. Its confirmation prompt only fires when interactive();
#' in a non-interactive Rscript run (e.g. release_run_all() in the
#' background) that check is silently skipped, so it would accept every
#' flagged word -- real typos included -- with no confirmation at all.
#' Use release_spelling_add() instead to add specific words you've
#' actually reviewed.
release_spelling <- function(update = FALSE) {
  wds <- spelling::spell_check_package()
  print(wds)
  if (update) {
    if (!interactive()) {
      stop(glue::glue(
        "release_spelling(update = TRUE) calls spelling::update_wordlist(), ",
        "which replaces the whole wordlist non-interactively -- refusing ",
        "outside an interactive session. Use release_spelling_add() to add ",
        "specific words instead."
      ))
    }
    spelling::update_wordlist()
  }
  invisible(wds)
}

#' Add specific words to inst/WORDLIST -- selective and additive, unlike
#' spelling::update_wordlist() (see release_spelling() above). Existing
#' entries are kept; `words` is merged in, deduplicated, and re-sorted.
#'
#' @param words Character vector of words to accept, e.g. copied from
#'        release_spelling()'s printed output after reviewing which
#'        flagged words are genuine false positives (proper nouns,
#'        technical terms) rather than actual typos.
release_spelling_add <- function(words) {
  wordfile <- "inst/WORDLIST"
  old_words <- readLines(wordfile)
  new_words <- sort(union(old_words, words), method = "radix")
  added <- setdiff(new_words, old_words)

  if (length(added) == 0) {
    cat("No new words to add -- already in the wordlist.\n")
    return(invisible(old_words))
  }

  writeLines(enc2utf8(new_words), wordfile, useBytes = TRUE)
  cat(glue::glue(
    "Added {length(added)} word(s) to {wordfile}: {paste(added, collapse = ', ')}\n"
  ))
  invisible(new_words)
}

#' @param update Rewrite URLs found to have moved? Default FALSE, same
#'        reasoning as release_spelling().
release_urls <- function(update = FALSE) {
  urlchecker::url_check()
  if (update) urlchecker::url_update()
}
# one-time setup if urlchecker isn't installed:
# install.packages('urlchecker', repos = 'https://r-lib.r-universe.dev')

# ---- 3. Site & README -------------------------------------------------------

#' Note: devtools::build_readme() crashes on README.Rmd's vignettes chunk
#' (tools::getVignetteInfo() returns 0 rows under load_all()) -- see
#' issues/TASKS.md's "Build gotcha" entry. Requires the package already
#' installed with vignettes (devtools::install(build_vignettes = TRUE))
#' and renders directly via rmarkdown::render() instead of build_readme().
release_site <- function() {
  if (!file.exists("README.md") || file.mtime("README.Rmd") > file.mtime("README.md")) {
    rmarkdown::render("README.Rmd", output_format = "github_document", quiet = TRUE)
    unlink("README.html")
  } else {
    cat("README.md is already up to date with README.Rmd; skipping re-render.\n")
  }
  pkgdown::build_site(lazy = TRUE)
}

# ---- 4. Build & local check -------------------------------------------------

#' Note: devtools::build_vignettes() *moves* (rather than copies) any
#' vignettes/*.pdf.asis-referenced PDF into doc/, deleting it from
#' vignettes/ -- the actual source location for that vignette. We restore
#' those from doc/ afterward so vignettes/ never ends up missing a file.
release_build <- function() {
  devtools::build()

  asis_pdfs <- sub("\\.asis$", "", list.files("vignettes", pattern = "\\.pdf\\.asis$"))

  # to test vignettes, this builds them into doc/ -- remove that directory
  # afterwards if you don't want it left around
  devtools::build_vignettes()

  missing_pdfs <- asis_pdfs[!file.exists(file.path("vignettes", asis_pdfs))]
  for (pdf in missing_pdfs) {
    src <- file.path("doc", pdf)
    if (file.exists(src)) {
      file.copy(src, file.path("vignettes", pdf))
      cat(glue::glue("Restored vignettes/{pdf} (moved to doc/ by build_vignettes())\n"))
    } else {
      warning(glue::glue(
        "vignettes/{pdf} went missing after build_vignettes() and isn't in ",
        "doc/ either -- restore it manually, e.g. `git checkout -- vignettes/{pdf}`"
      ))
    }
  }

  devtools::build_manual()
}

#' The actual `R CMD check --as-cran` run, locally. Persists the result to
#' .release_check_result.rds so release_cran_comments() can build the
#' "R CMD check results" section from real output instead of you having
#' to transcribe it by hand.
release_check <- function() {
  res <- devtools::check()
  saveRDS(res, ".release_check_result.rds")
  invisible(res)
}

# ---- 5. Remote checks --------------------------------------------------------

release_check_win <- function() {
  devtools::check_win_devel()
}

# ---- 6. Reverse dependencies -------------------------------------------------

release_revdep <- function(num_workers = 4) {
  revdepcheck::revdep_reset()
  revdepcheck::revdep_check(num_workers = num_workers, bioc = FALSE)
  print(devtools::revdep())
}
# one-time setup if revdepcheck isn't installed:
# remotes::install_github("r-lib/revdepcheck")

# ---- 7. cran-comments.md ---------------------------------------------------

#' Regenerate cran-comments.md: fills in R CMD check results (from
#' release_check()'s saved .release_check_result.rds), the reverse
#' dependency summary (from revdep/cran.md, written by release_revdep()),
#' and replaces the whole `## Comments` section with the NEWS.md entries
#' added since the version currently on CRAN -- so it only describes what
#' changed since CRAN last saw this package, not the full changelog.
#'
#' Run release_check() and release_revdep() first; this just assembles
#' their output; it doesn't re-run either.
#'
#' @param known_issues Optional character vector of standing explanations
#'        to append as a "## Known issues" section -- for things a remote
#'        check (win-builder, R-hub) flags but a local devtools::check()
#'        never sees, so they wouldn't otherwise show up here. No standing
#'        issues known for vcdExtra yet; pass a character vector if one
#'        turns up (e.g. from release_check_win()'s incoming-feasibility
#'        output), or leave NULL to omit the section.
release_cran_comments <- function(known_issues = NULL) {
  info <- release_preflight()

  check_section <- if (file.exists(".release_check_result.rds")) {
    res <- readRDS(".release_check_result.rds")
    n_errors <- length(res$errors)
    n_warnings <- length(res$warnings)
    n_notes <- length(res$notes)
    issues <- c(res$errors, res$warnings, res$notes)
    detail <- if (length(issues)) paste(trimws(issues), collapse = "\n\n") else ""
    glue::glue("{n_errors} error(s) | {n_warnings} warning(s) | {n_notes} note(s)\n\n{detail}")
  } else {
    warning("No .release_check_result.rds found -- run release_check() first. Leaving a placeholder.")
    "(run release_check() first)"
  }

  revdep_section <- if (file.exists("revdep/cran.md")) {
    revdep_lines <- readLines("revdep/cran.md")
    # drop revdep/cran.md's own "## revdepcheck results" heading --
    # redundant with the "## Reverse dependencies checks" heading below
    revdep_lines <- revdep_lines[!grepl("^## ", revdep_lines)]
    paste(trimws(revdep_lines), collapse = "\n")
  } else {
    warning("No revdep/cran.md found -- run release_revdep() first. Leaving a placeholder.")
    "(run release_revdep() first)"
  }

  news <- readLines("NEWS.md")
  cran_row <- grep(glue::glue("^## Version {info$cran_version}\\b"), news)
  end <- if (length(cran_row)) cran_row[1] - 1 else length(news)
  comments_section <- paste(trimws(news[seq_len(end)]), collapse = "\n")

  known_issues_section <- if (!is.null(known_issues)) {
    # glue::glue() always strips exactly one trailing newline from its
    # result regardless of `trim` -- the extra \n here compensates so the
    # blank line separating this from the next section survives
    glue::glue(
      "## Known issues\n\n{paste(known_issues, collapse = '\n\n')}\n\n\n",
      trim = FALSE
    )
  } else {
    ""
  }

  content <- glue::glue(
    "## Test environments\n",
    "* local {Sys.info()[['sysname']]} {Sys.info()[['release']]} install, {R.version.string}\n",
    "* win-builder (R-devel)\n",
    "\n",
    "## R CMD check results\n",
    "{check_section}\n",
    "\n",
    "{known_issues_section}",
    "## Reverse dependencies checks\n",
    "\n",
    "{revdep_section}\n",
    "\n",
    "## Comments\n",
    "\n",
    "{comments_section}\n"
  )

  writeLines(content, "cran-comments.md")
  cat("Wrote cran-comments.md\n")
  invisible(content)
}

# ---- 8. Orchestration ---------------------------------------------------------

#' Run one release step, catching (not swallowing) any error so the rest
#' of the sequence still runs; failures are collected and reported at the
#' end by release_run_all() instead of halting things partway through.
run_step <- function(name, step) {
  cat(glue::glue("\n=== {name} ===\n"))
  tryCatch({
    step()
    NULL
  }, error = function(e) {
    msg <- conditionMessage(e)
    cat(glue::glue("✖ {name} failed: {msg}\n"))
    glue::glue("{name}: {msg}")
  })
}

#' Run every automatable release-check step in sequence
#'
#' Does not call release_check_win() or release_revdep() by default --
#' both are slow (win-builder is an external upload; revdep checks every
#' reverse dependency twice) and worth running deliberately rather than as
#' part of a routine re-run. Set `full = TRUE` to include them.
#'
#' Each step runs even if an earlier one fails (e.g. release_urls()
#' finding a dead link) -- failures are collected and reported together
#' at the end instead of halting the sequence.
release_run_all <- function(full = FALSE, num_workers = 4) {
  steps <- list(
    release_preflight = release_preflight,
    release_document  = release_document,
    release_spelling  = release_spelling,
    release_urls      = release_urls,
    release_site      = release_site,
    release_build     = release_build,
    release_check     = release_check
  )
  if (full) {
    steps$release_check_win <- release_check_win
    steps$release_revdep <- function() release_revdep(num_workers = num_workers)
    steps$release_cran_comments <- release_cran_comments
  }

  failures <- Map(run_step, names(steps), steps)
  failures <- Filter(Negate(is.null), failures)

  cat("\nAutomated steps done.\n")
  if (length(failures) > 0) {
    cat(glue::glue("\n{length(failures)} step(s) failed:\n"))
    cat(paste(" -", unlist(failures)), sep = "\n")
  } else {
    cat("All steps completed without error.\n")
  }

  cat("\nRemaining manual steps:\n")
  cat("  1. Review spelling/URL/check output above; fix anything flagged\n")
  if (!full) {
    cat("  2. release_check_win(), release_revdep(), then release_cran_comments() (slow; not run by default)\n")
  } else {
    cat("  2. Review the regenerated cran-comments.md\n")
  }
  cat("  3. Confirm Version/Date in DESCRIPTION and NEWS.md entry are correct\n")
  cat("  4. devtools::release()\n")

  invisible(failures)
}
