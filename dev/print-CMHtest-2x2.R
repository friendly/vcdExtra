# Prototype: print.CMHtest() with a 2x2 layout option
# See dev/CMH-2x2.md for the design notes this implements.
#
# Adds a `layout = c("table", "2x2")` argument to print.CMHtest(). The "table" layout
# is unchanged (the existing flat 4-row printout). The "2x2" layout reorganizes the four
# CMH statistics (cor, rmeans, cmeans, general) into a 2x2 table crossing how the row and
# column variables are each treated (general/nominal vs ordered/scored), with row/column
# margin diffs and an experimental corner "diff of diffs" cell.
#
# To try it: source this file after loading vcdExtra (library(vcdExtra) or
# devtools::load_all()), which redefines print.CMHtest in the global environment,
# shadowing the package version for S3 dispatch. Then run the if (FALSE) block at the
# bottom, or call print(cmh_result, layout = "2x2") directly.

print.CMHtest <- function(x, digits = max(getOption("digits") - 2, 3),
                           layout = c("table", "2x2"), stars = FALSE,
                           scale = FALSE, ...) {
  layout <- match.arg(layout)
  stars <- isTRUE(stars)
  scale <- isTRUE(scale)

  heading <- "Cochran-Mantel-Haenszel Statistics"
  if (!is.null(x$names)) {
    heading <- paste(heading, "for", paste(x$names, collapse = " by "))
  }
  if (!is.null(x$stratum)) {
    heading <- paste(
      heading,
      ifelse(
        x$stratum == "ALL",
        "\n\tOverall tests, controlling for all strata",
        paste("\n\tin stratum", x$stratum)
      )
    )
  }

  tbl <- x$table
  types <- rownames(tbl)

  if (layout == "2x2") {
    required <- c("cor", "rmeans", "cmeans", "general")
    if (!all(required %in% types)) {
      warning("layout = '2x2' needs all of ", paste(required, collapse = ", "),
              "; falling back to layout = 'table'. Missing: ",
              paste(setdiff(required, types), collapse = ", "))
      layout <- "table"
    }
  }

  if (layout == "table") {
    labels <- list(
      cor = "Nonzero correlation",
      rmeans = "Row mean scores differ",
      cmeans = "Col mean scores differ",
      general = "General association"
    )
    labels <- unlist(labels[types])
    df <- data.frame(
      "AltHypothesis" = as.character(labels),
      tbl,
      stringsAsFactors = FALSE
    )
    cat(heading, "\n\n")
    print(df, digits = digits, ...)
    cat("\n")
    return(invisible(x))
  }

  # ---- layout == "2x2" -------------------------------------------------------

  # NB: for multi-stratum results, CMHtest3()'s `table` (in R/CMHtest.R) is built via
  # rbind() starting from an empty list(), which produces a "list matrix" -- each cell
  # a length-1 list, not a plain double -- even though it prints identically to a normal
  # numeric matrix. print.CMHtest()'s original "table" layout never noticed, since it
  # just wraps the values in a data.frame; arithmetic on the raw values (needed here)
  # breaks on that without unlisting first. Worth fixing at the source (CMHtest3()
  # itself, e.g. `table <- matrix(nrow = 0, ncol = 3)` instead of `table <- list()`),
  # but defending against it here too so this works with existing package versions.
  Chisq <- setNames(as.numeric(unlist(tbl[, "Chisq"])), types)
  Df    <- setNames(as.numeric(unlist(tbl[, "Df"])), types)

  stat  <- function(type) unname(Chisq[type])
  dfree <- function(type) unname(Df[type])

  pval <- function(q, d) {
    if (is.na(q) || is.na(d) || q < 0 || d <= 0) return(NA_real_)
    pchisq(q, d, lower.tail = FALSE)
  }
  sig_stars <- function(p) {
    if (is.na(p)) ""
    else if (p < .001) "***"
    else if (p < .01)  "**"
    else if (p < .05)  "*"
    else ""
  }
  cell <- function(q, d, flag_negative = FALSE) {
    note <- if (flag_negative && !is.na(q) && q < 0) " [invalid: negative]" else ""
    star <- if (stars) sig_stars(pval(q, d)) else ""
    if (scale) {
      sprintf("%.*f%s%s", digits, q / d, star, note)
    } else {
      sprintf("%.*f (%d)%s%s", digits, q, d, star, note)
    }
  }

  # core 2x2
  g  <- stat("general"); g_df  <- dfree("general")
  rm <- stat("rmeans");  rm_df <- dfree("rmeans")
  cm <- stat("cmeans");  cm_df <- dfree("cmeans")
  cr <- stat("cor");     cr_df <- dfree("cor")

  # margins: within a fixed column/row, general-treatment minus ordered-treatment
  colgen_diff <- g - cm;  colgen_diff_df <- g_df - cm_df   # col: general;  row gen - row ord
  colord_diff <- rm - cr; colord_diff_df <- rm_df - cr_df  # col: ordered;  row gen - row ord
  rowgen_diff <- g - rm;  rowgen_diff_df <- g_df - rm_df   # row: general;  col gen - col ord
  rowor_diff  <- cm - cr; rowor_diff_df  <- cm_df - cr_df  # row: ordered;  col gen - col ord

  # corner: diff of diffs -- EXPERIMENTAL, see dev/CMH-2x2.md. Not guaranteed >= 0:
  # these are four different quadratic forms, not a nested LR sequence, so the naive
  # inclusion-exclusion combination can go negative (observed with MSPatients).
  corner    <- g - rm - cm + cr
  corner_df <- g_df - rm_df - cm_df + cr_df

  out <- matrix(
    c(
      cell(g, g_df),                       cell(rm, rm_df),                     cell(rowgen_diff, rowgen_diff_df),
      cell(cm, cm_df),                     cell(cr, cr_df),                     cell(rowor_diff, rowor_diff_df),
      cell(colgen_diff, colgen_diff_df),   cell(colord_diff, colord_diff_df),   cell(corner, corner_df, flag_negative = TRUE)
    ),
    nrow = 3, byrow = TRUE,
    dimnames = list(
      c("row: general", "row: ordered", "diff (gen−ord)"),
      c("col: general", "col: ordered", "diff (gen−ord)")
    )
  )

  cat(heading, "\n")
  cat("\tRow/column display: general (nominal) vs. ordered (scored)\n")
  cat(if (scale) "\tCell values: X^2/df\n\n" else "\tCell values: X^2 (df)\n\n")
  print(as.data.frame(out), right = TRUE)
  if (stars) {
    cat("\nSignif. codes: '***' p<.001  '**' p<.01  '*' p<.05\n")
  }
  cat("\n")
  # cat("Corner cell (diff of diffs) is algebraically consistent but its distribution as\n")
  # cat("chi-square(df) is unverified, and isn't guaranteed to be >= 0 in general (these are\n")
  # cat("four different quadratic-form statistics, not a nested LR sequence) -- it just\n")
  # cat("happens to come out positive for this data. Treat as experimental (see\n")
  # cat("dev/CMH-2x2.md).\n\n")

  invisible(x)
}

# CMHtest(..., overall = TRUE) (or any strata call) returns a plain, unclassed
# list of per-stratum "CMHtest" objects (plus "ALL" when overall = TRUE) -- each
# element already carries class "CMHtest", so printing all of them with the same
# layout/stars/scale is just an lapply() over print.CMHtest(); the list itself
# has no class to dispatch print(x, layout = "2x2") on directly.
print_CMHtest_list <- function(x, ...) {
  invisible(lapply(x, print.CMHtest, ...))
}


# ==============================================================================
# Test cases -- guarded so this file can be `source()`d without side effects.
# Run interactively after: library(vcdExtra); source("dev/print-CMHtest-2x2.R")
# ==============================================================================
if (FALSE) {

  library(vcdExtra)

  # --- Mental: 6 x 4, unequal R and C -----------------------------------------
  data(Mental)
  cmh_mental <- CMHtest(Freq ~ ses + mental, data = Mental)
  print(cmh_mental)                  # unchanged default behavior
  print(cmh_mental, layout = "2x2")  # new display, no stars (default)
  print(cmh_mental, layout = "2x2", stars = TRUE)  # opt in to significance stars
  print(cmh_mental, layout = "2x2", scale = TRUE)  # X^2/df instead of raw X^2

  # --- MSPatients: 4 x 4 x 2 strata, square table, overall CMH ----------------
  # CMHtest(..., overall = TRUE) returns a *list* of per-stratum CMHtest objects
  # plus a combined one named "ALL".
  data(MSPatients, package = "vcd")
  cmh_ms <- CMHtest(MSPatients, overall = TRUE)  # list: 2 strata + "ALL"
  print(cmh_ms)                                  # default list-print: all 3, layout = "table"
  print_CMHtest_list(cmh_ms, layout = "2x2")                 # all 3, layout = "2x2"
  print_CMHtest_list(cmh_ms, layout = "2x2", stars = TRUE)   # all 3, with stars

  # ALL only, for isolated testing -- also exercises the CMHtest3() "list matrix"
  # table quirk noted above (the overall-across-strata path): confirms the
  # defensive unlist() in print.CMHtest() handles it.
  print(cmh_ms$ALL, layout = "2x2")

  # --- Fallback check: restrict types so not all four are available ----------
  cmh_partial <- CMHtest(Freq ~ ses + mental, data = Mental, types = c("cor", "general"))
  print(cmh_partial, layout = "2x2")  # should warn and fall back to layout = "table"
}
