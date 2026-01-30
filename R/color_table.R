# ✔️DONE: Initial implementation, using `gt` pkg for the table
#
# ✔️DONE: When the cell is colored a dark shade, use white text for readability.
#         Uses colorspace::contrast_ratio() to pick optimal text color.
# ✔️DONE: The default palette for diverging uses darker shades from RdBu (#2166AC, #B2182B)
# ✔️DONE: Suppress Warning `In chisq.test(x) : Chi-squared approximation may be incorrect`
# ✔️DONE: Test cases in dev/test-color_table.R using vcd::Suicide, vcd::PreSex, vcdExtra::Abortion
# ✔️DONE: Fixed bug in format(round(...)) - now checks is.numeric() before applying round()
#         The error occurred because as.data.frame() on a matrix can include factor columns.
# ✔️DONE: Make the display of the Total row and column optional. -- handled by `margins`
#
# ✔️DONE: Row category labels (stub) now bold, matching column labels; Total row stub is italic.
#         Future enhancement: could extend `margins` to accept a list for custom styling.
#
# ✔️DONE: Add filename arg, which if not NULL saves the `gt` result as an image via gt::gtsave().
#         Supports .png, .svg, .pdf, .html, .rtf, .docx formats. Additional args passed via `...`.
#
# ✔️DONE: Refactored as S3 generic with methods for table, xtabs, ftable, structable, data.frame, matrix.
#         The .color_table_impl() internal function handles the core gt table building.
#
# ✔️DONE: Allow input argument x to be a dataset in frequency form (data.frame with Freq column).
#         New freq_col parameter allows specifying the frequency column name.
#
# ✔️DONE: For multi-way tables, use MASS::loglm() to fit complete independence model and compute
#         proper Pearson residuals. Print message with X^2, df, p-value when shade != "freq".
#         Uses cat() for output to ensure visibility in all R environments.
#
# ✔️DONE: Note legend for table shading interpretation implemented via `gt::tab_source_note()`.
#         `legend = TRUE` or `legend = "note"` adds note (default); `legend = FALSE` suppresses.
#         For `shade = "freq"`: "Shading based on values of observed frequencies".
#         For residuals: uses same text as printed to console (X^2, df, p-value).
#         Future: `legend = "graphic"` could show graphic legend like
#         `dev/color-tab-figs/shading-legend.png`.
#
# ✔️DONE: Add an argument `values = "frequency" | "residuals"` (possibly
#         abbreviated) to all the values displayed in the table to be the cell
#         frequencies (as now, and default), or to be the residuals for the `model` fit.
#
# 🚩TODO: [HARD] When there are two (or more) variables for the row, these should appear as a nested
#         hierarchy similar to what is shown in the table for the Titanic data in
#         `dev/color-tab-figs/Titanic-residual-shading.png`. That is, the
#         Hair-Sex combinations that appear like "Black_Male" should be two
#         columns for Hair and Sex, and the rows for the other cases of black
#         hair should have Sex empty. Not sure whether nested row groups can be
#         shown to look nested otherwise. One solution would be to make the variables
#         that define the rows into multiple columns, e.g., labeled "Hair", "Sex" for this
#         example.
#
# 🚩TODO: Column spanner headings: When two or more variables are in the columns, the output
#         is confusing and ugly. Examples:
#     color_table(PreSex,  formula = MaritalStatus + Gender ~ PremaritalSex + ExtramaritalSex)
#     color_table(PreSex,  formula = Gender + PremaritalSex + ExtramaritalSex ~  MaritalStatus)
#         This can e handled using column spanners: https://gt.rstudio.com/reference/tab_spanner.html
#         


#' Display Frequency Table with Colored Cell Backgrounds
#'
#' Creates a formatted, semi-graphic "heatmap" table display of frequency data with cell backgrounds
#' colored according to observed frequencies or their residuals from a loglinear model.
#' This is an S3 generic function with methods for different input types.
#'
#' @param x A `"table"`, `"xtabs"`, `"matrix"`, `"ftable"`, `"structable"`, or `"data.frame"` object
#' @param ... Additional arguments passed to methods
#'
#' @details
#' This function provides a heatmap-style representation of a frequency table,
#' where background coloring is used to visualize patterns and anomalies in the data.
#' When shading by residuals (the default), cells with large positive residuals
#' (more observations than expected) are shaded red, while cells with large negative
#' residuals (fewer than expected) are shaded blue. This makes it easy to identify
#' cells that deviate substantially from what would be expected under a given model
#' (by default, the independence model).
#'
#' For multi-way tables (3 or more dimensions), residuals are computed from the
#' model of complete independence among all factors using \code{\link[MASS]{loglm}},
#' unless you specify a model using the `model` or `expected` arguments.
#' A message is printed showing the chi-squared statistic, degrees of freedom,
#' and p-value for this test.
#'
#' **Contrast shading**
#'
#' For cells with dark background colors, black text can be difficult to read.
#' This function automatically selects white or black text for each cell based
#' on which provides better contrast against the background color. If the
#' \pkg{colorspace} package is available, \code{\link[colorspace]{contrast_ratio}}
#' is used to determine the optimal text color according to WCAG 2.1 guidelines.
#' Otherwise, a fallback based on relative luminance (ITU-R BT.709) is used.
#'
#' **Use in documents**
#'
#' In R Markdown (\code{.Rmd}) or Quarto (\code{.qmd}) documents, \pkg{gt} tables
#' may not render correctly in all output formats. The \code{filename} argument
#' provides a workaround: save the table as an image, then include it using
#' \code{\link[knitr]{include_graphics}}. For example:
#'
#' \preformatted{
#'     color_table(my_table, filename = "my_table.png")
#'     knitr::include_graphics("my_table.png")
#' }
#'
#' For higher quality output, \code{.svg} format is recommended. You can control
#' the image dimensions using the \code{vwidth} and \code{vheight} arguments
#' (passed via \code{...}).
#'
#' If you need a caption for cross-referencing (especially in Quarto or R Markdown),
#' you can use `gt::tab_caption()`
#' \preformatted{
#'      gt_object |> tab_caption(caption = "Table 1: Pattern of Association in MyTable")
#'  }
#'
#' @return A gt table object that can be further customized
#'
#' @examples
#' \dontrun{
#' # Basic usage with 2-way table - shade by residuals from independence
#' data(HairEyeColor)
#' HEC <- margin.table(HairEyeColor, 1:2)  # 2-way: Hair x Eye
#' color_table(HEC)
#'
#' # Shade by frequencies instead (no message printed)
#' color_table(HEC, shade = "freq")
#'
#' # 3-way table - using a formula to specify layout
#' color_table(HairEyeColor, formula = Eye ~ Hair + Sex)
#'
#' # Display residual values in cells instead of frequencies
#' color_table(HEC, values = "residuals")
#'
#' # From a data.frame in frequency form (2-way)
#' hec_df <- as.data.frame(HEC)
#' color_table(hec_df)
#'
#' # Save table as an image file
#' color_table(HEC, filename = "hair_eye_table.png")
#' }
#'
#' @importFrom stats chisq.test residuals xtabs pchisq
#' @importFrom grDevices col2rgb
#' @importFrom dplyr all_of everything
#' @export
color_table <- function(x, ...) {
  UseMethod("color_table")
}

#' @describeIn color_table Method for table objects (including result of xtabs)
#' @param formula Formula specifying a `row_vars ~ col_vars` layout (for multi-way tables) to
#'        make them "flat" as defined for `vcd::structable()` and `stats::ftable()`.
#' @param values What values to display in cells: `"freq"` for observed frequencies (default),
#'   or `"residuals"` to display the residual values. When `values = "residuals"`, margins
#'   are suppressed since residuals don't have meaningful totals.
#' @param shade What values determine cell shading: `"residuals"` (default), `"freq"`,
#'        `"pearson"`, or `"deviance"`
#' @param model A fitted model (loglm or glm) to compute residuals from.
#'        If NULL and shade involves residuals, uses an independence model for all factors.
#' @param expected Expected frequencies (alternative to `model`), a data structure of the same shape
#'        as `x`
#' @param palette Color palette function or vector for background colors. Default depends on shade
#'        type. When `shade = "freq"` the default is `palette = c("white", "firebrick")`; otherwise
#'        `c("#B2182B", "white", , "#2166AC")` ranging from red to blue for negative and positive
#'         residuals. The background colors are computed by interpolation using
#'         `scales::col_numeric()`.
#'
#' @param legend Controls display of shading interpretation note:
#'        \code{TRUE} or \code{"note"} (default) adds a source note explaining the shading;
#'        \code{FALSE} (default) suppresses the note, but a message is printed in the console.
#' @param margins Logical, include row/column totals?
#' @param digits Number of decimal places for displayed values
#' @param title Optional table title
#' @param filename Optional filename to save the table as an image. If provided,
#'        the table is saved using \code{\link[gt]{gtsave}}. Supported formats include
#'        \code{.png}, \code{.svg}, \code{.pdf}, \code{.html}, \code{.rtf}, and \code{.docx}.
#'        The file format is determined by the file extension. Other arguments can be passed
#'        to \code{\link[gt]{gtsave}} via `...`.
#' @export
color_table.table <- function(x,
                               formula = NULL,
                               values = c("freq", "residuals"),
                               shade = c("residuals", "freq", "pearson", "deviance"),
                               model = NULL,
                               expected = NULL,
                               palette = NULL,
                               legend = FALSE,
                               margins = TRUE,
                               digits = 0,
                               title = NULL,
                               filename = NULL,
                               ...) {

  values <- match.arg(values)
  shade <- match.arg(shade)
  if (shade == "pearson") shade <- "residuals"

  # Store original table for proper residual computation
  x_orig <- x

  dims <- dim(x)
  ndim <- length(dims)

  # For multi-way tables, use structable to flatten
  if (ndim > 2) {
    if (requireNamespace("vcd", quietly = TRUE)) {
      if (!is.null(formula)) {
        st <- vcd::structable(formula, data = x)
      } else {
        # Default: first variable as rows, remaining as columns
        st <- vcd::structable(x)
      }
      x_mat <- as.matrix(st)
    } else {
      stop("Package 'vcd' is required for multi-way tables.")
    }
  } else {
    x_mat <- as.matrix(x)
  }

  .color_table_impl(x_mat,
                    x_orig = x_orig,
                    formula = formula,
                    values = values,
                    shade = shade,
                    model = model,
                    expected = expected,
                    palette = palette,
                    legend = legend,
                    margins = margins,
                    digits = digits,
                    title = title,
                    filename = filename,
                    ...)
}

#' @describeIn color_table Method for ftable objects
#' @export
color_table.ftable <- function(x,
                                values = c("freq", "residuals"),
                                shade = c("residuals", "freq", "pearson", "deviance"),
                                model = NULL,
                                expected = NULL,
                                palette = NULL,
                                legend = FALSE,
                                margins = TRUE,
                                digits = 0,
                                title = NULL,
                                filename = NULL,
                                ...) {

  values <- match.arg(values)
  shade <- match.arg(shade)
  if (shade == "pearson") shade <- "residuals"

  # Convert ftable to matrix
  x_mat <- as.matrix(x)

  # ftable loses the original structure, so we can only use 2D residuals
  .color_table_impl(x_mat,
                    x_orig = NULL,
                    formula = NULL,
                    values = values,
                    shade = shade,
                    model = model,
                    expected = expected,
                    palette = palette,
                    legend = legend,
                    margins = margins,
                    digits = digits,
                    title = title,
                    filename = filename,
                    ...)
}

#' @describeIn color_table Method for structable objects (vcd package)
#' @export
color_table.structable <- function(x,
                                    values = c("freq", "residuals"),
                                    shade = c("residuals", "freq", "pearson", "deviance"),
                                    model = NULL,
                                    expected = NULL,
                                    palette = NULL,
                                    legend = FALSE,
                                    margins = TRUE,
                                    digits = 0,
                                    title = NULL,
                                    filename = NULL,
                                    ...) {

  values <- match.arg(values)
  shade <- match.arg(shade)
  if (shade == "pearson") shade <- "residuals"

  # Convert structable to matrix (flattened 2D form)
  x_mat <- as.matrix(x)

  # structable loses the original structure, so we can only use 2D residuals
  .color_table_impl(x_mat,
                    x_orig = NULL,
                    formula = NULL,
                    values = values,
                    shade = shade,
                    model = model,
                    expected = expected,
                    palette = palette,
                    legend = legend,
                    margins = margins,
                    digits = digits,
                    title = title,
                    filename = filename,
                    ...)
}

#' @describeIn color_table Method for data.frame in frequency form
#' @param freq_col Name of the frequency column. If NULL, looks for "Freq" or "count".
#' @export
color_table.data.frame <- function(x,
                                    formula = NULL,
                                    freq_col = NULL,
                                    values = c("freq", "residuals"),
                                    shade = c("residuals", "freq", "pearson", "deviance"),
                                    model = NULL,
                                    expected = NULL,
                                    palette = NULL,
                                    legend = FALSE,
                                    margins = TRUE,
                                    digits = 0,
                                    title = NULL,
                                    filename = NULL,
                                    ...) {

  values <- match.arg(values)
  shade <- match.arg(shade)
  if (shade == "pearson") shade <- "residuals"

  # Find the frequency column
  if (is.null(freq_col)) {
    if ("Freq" %in% names(x)) {
      freq_col <- "Freq"
    } else if ("count" %in% names(x)) {
      freq_col <- "count"
    } else {
      stop("Cannot find frequency column. Specify freq_col or use a column named 'Freq' or 'count'.")
    }
  }

  # Get factor columns (all columns except the frequency column)
  factor_cols <- setdiff(names(x), freq_col)

  if (length(factor_cols) < 2) {
    stop("Data frame must have at least 2 factor columns plus a frequency column.")
  }

  # Build xtabs formula
  if (is.null(formula)) {
    # Default: first factor as rows, second as columns
    xtabs_formula <- as.formula(paste(freq_col, "~", paste(factor_cols, collapse = " + ")))
  } else {
    # User provided formula - need to add freq_col to LHS
    xtabs_formula <- as.formula(paste(freq_col, "~", as.character(formula)[2]))
  }

  # Convert to table using xtabs
  x_table <- xtabs(xtabs_formula, data = x)

  # Now use the table method
  color_table.table(x_table,
                    formula = formula,
                    shade = shade,
                    model = model,
                    expected = expected,
                    palette = palette,
                    legend = legend,
                    margins = margins,
                    digits = digits,
                    values = values,
                    title = title,
                    filename = filename,
                    ...)
}

#' @describeIn color_table Method for matrix objects
#' @export
color_table.matrix <- function(x,
                                values = c("freq", "residuals"),
                                shade = c("residuals", "freq", "pearson", "deviance"),
                                model = NULL,
                                expected = NULL,
                                palette = NULL,
                                legend = FALSE,
                                margins = TRUE,
                                digits = 0,
                                title = NULL,
                                filename = NULL,
                                ...) {

  values <- match.arg(values)
  shade <- match.arg(shade)
  if (shade == "pearson") shade <- "residuals"

  .color_table_impl(x,
                    x_orig = NULL,
                    formula = NULL,
                    values = values,
                    shade = shade,
                    model = model,
                    expected = expected,
                    palette = palette,
                    legend = legend,
                    margins = margins,
                    digits = digits,
                    title = title,
                    filename = filename,
                    ...)
}

#' @describeIn color_table Default method
#' @export
color_table.default <- function(x, ...) {
  stop("color_table() does not know how to handle object of class ",
       paste(class(x), collapse = "/"),
       ". Supported classes: table, xtabs, ftable, structable, data.frame, matrix.")
}

# ###
# Internal implementation function
# ###

# Internal workhorse function for color_table
#
# This function takes a 2D matrix and builds the gt table with colored cells.
# All S3 methods convert their input to a matrix and call this function.
#
# @param x A 2D matrix with row and column names (the flattened display matrix)
# @param x_orig The original table (for multi-way tables, used to compute
#   complete independence residuals). NULL for 2D tables.
# @param formula The formula used for flattening (needed for residual mapping)
# @param shade Type of shading
# @param model Optional fitted model
# @param expected Optional expected frequencies
# @param palette Color palette
# @param legend Show legend?
# @param margins Show margins?
# @param values What to display in cells: "freq" or "residuals"
# @param digits Decimal places
# @param title Table title
# @param filename Optional file to save
# @param ... Additional arguments passed to gtsave
#
# @noRd
.color_table_impl <- function(x,
                               x_orig = NULL,
                               formula = NULL,
                               values = "freq",
                               shade = "residuals",
                               model = NULL,
                               expected = NULL,
                               palette = NULL,
                               legend = FALSE,
                               margins = TRUE,
                               digits = 0,
                               title = NULL,
                               filename = NULL,
                               ...) {


  # # DEBUG: Confirm function entry and parameters
  # cat("DEBUG: Entering .color_table_impl\n")
  # cat("DEBUG: shade =", shade, "\n")
  # cat("DEBUG: is.null(x_orig) =", is.null(x_orig), "\n")
  # if (!is.null(x_orig)) {
  #   cat("DEBUG: length(dim(x_orig)) =", length(dim(x_orig)), "\n")
  # }

  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Package 'gt' is required for color_table(). Please install it.")
  }

  # Ensure x is a matrix
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }

  dims <- dim(x)

  # Get dimension names
  dn <- dimnames(x)
  rnames <- dn[[1]]
  cnames <- dn[[2]]
  rvar <- names(dn)[1]
  cvar <- names(dn)[2]

  # Provide default names if missing
  if (is.null(rnames)) rnames <- paste0("R", seq_len(dims[1]))
  if (is.null(cnames)) cnames <- paste0("C", seq_len(dims[2]))
  if (is.null(rvar) || rvar == "") rvar <- "Row"
  if (is.null(cvar) || cvar == "") cvar <- "Col"

  # Ensure dimnames are set on x
  dimnames(x) <- list(rnames, cnames)
  names(dimnames(x)) <- c(rvar, cvar)

  # Validate values parameter
  if (values == "residuals" && shade == "freq") {
    stop("Cannot display residuals when shade = 'freq'. Use shade = 'residuals' or similar.")
  }

  # Initialize shading message for legend
  shading_message <- NULL

  # Compute shading values
  # cat("DEBUG: About to compute shading values\n")
  if (shade == "freq") {
    # cat("DEBUG: shade == 'freq', skipping residuals\n")
    shade_values <- x
    resid_mat <- NULL  # No residuals computed
    shading_message <- "Shading based on values of observed frequencies"
  } else {
    # cat("DEBUG: Computing residuals (shade != 'freq')\n")
    # Compute residuals
    if (!is.null(model)) {
      # Use provided model - extract and reshape residuals
      resid_arr <- residuals(model, type = "pearson")
      resid_mat <- matrix(as.vector(resid_arr), nrow = dims[1], ncol = dims[2])
      # Print model info
      if (inherits(model, "loglm")) {
        shading_message <- sprintf("Shading based on residuals from fitted model, X^2 = %.2f, df = %d, p = %.4g",
                    model$pearson, model$df, 1 - pchisq(model$pearson, model$df))
        cat(shading_message, "\n")
      }
    } else if (!is.null(expected)) {
      # Use provided expected values
      resid_mat <- (x - expected) / sqrt(expected)
      shading_message <- "Shading based on residuals from user-supplied expected frequencies"
      cat(shading_message, "\n")
    } else {
      # Fit independence model
      # For multi-way tables, use loglm for complete independence
      if (!is.null(x_orig) && length(dim(x_orig)) > 2) {
        # Multi-way table: fit complete independence model
        # cat("DEBUG: In multi-way table branch (>2 dimensions)\n")
        if (!requireNamespace("MASS", quietly = TRUE)) {
          stop("Package 'MASS' is required for residuals from multi-way tables.")
        }

        # Build formula for complete independence: ~ A + B + C + ...
        var_names <- names(dimnames(x_orig))
        indep_formula <- as.formula(paste("~", paste(var_names, collapse = " + ")))

        # Use do.call to avoid scoping issues with loglm's non-standard evaluation
        mod_indep <- do.call(MASS::loglm, list(formula = indep_formula, data = x_orig))

        # Get residuals from the model
        resid_arr <- residuals(mod_indep, type = "pearson")

        # Map residuals to the flattened matrix structure
        # The structable flattening preserves cell order, so we can reshape
        if (!is.null(formula)) {
          # Re-create the structable to get proper cell ordering
          resid_st <- vcd::structable(formula, data = resid_arr)
          resid_mat <- as.matrix(resid_st)
        } else {
          # No formula - use default structable flattening
          resid_st <- vcd::structable(resid_arr)
          resid_mat <- as.matrix(resid_st)
        }

        # Print model info
        shading_message <- sprintf("Shading based on residuals from model of complete independence, X^2 = %.2f, df = %d, p = %.4g",
                       mod_indep$pearson, mod_indep$df, 1 - pchisq(mod_indep$pearson, mod_indep$df))
        if (is.null(formula)) {
          shading_message <- paste0(shading_message, "\nUse formula for better control.")
        }
        cat(shading_message, "\n")

      } else {
        # 2-way table: use chisq.test (equivalent to independence)
        # cat("DEBUG: In 2-way table branch, about to run chisq.test\n")
        chi_result <- suppressWarnings(chisq.test(x))
        resid_mat <- chi_result$residuals
        # cat("DEBUG: chisq.test completed, X^2 =", unname(chi_result$statistic), "\n")

        # Print model info
        shading_message <- sprintf("Shading based on residuals from model of independence,\n X^2 = %.2f, df = %d, p = %.4g",
                    unname(chi_result$statistic),
                    unname(chi_result$parameter),
                    chi_result$p.value)
        cat(shading_message, "\n")
      }
    }

    if (shade == "deviance" && is.null(model)) {
      # Compute deviance residuals
      # For multi-way tables, need expected from loglm
      if (!is.null(x_orig) && length(dim(x_orig)) > 2) {
        var_names <- names(dimnames(x_orig))
        indep_formula <- as.formula(paste("~", paste(var_names, collapse = " + ")))
        mod_indep <- do.call(MASS::loglm, list(formula = indep_formula, data = x_orig))
        exp_arr <- fitted(mod_indep)
        if (!is.null(formula)) {
          exp_st <- vcd::structable(formula, data = exp_arr)
          exp_mat <- as.matrix(exp_st)
        } else {
          exp_st <- vcd::structable(exp_arr)
          exp_mat <- as.matrix(exp_st)
        }
      } else {
        chi_result <- suppressWarnings(chisq.test(x))
        exp_mat <- chi_result$expected
      }
      obs <- x
      resid_mat <- sign(obs - exp_mat) * sqrt(2 * ifelse(obs > 0, obs * log(obs / exp_mat), 0))
    }

    shade_values <- resid_mat
  }

  # Determine what values to display in cells
  if (values == "residuals") {
    display_mat <- resid_mat
    # Margins don't make sense for residuals
    show_margins <- FALSE
    # Use more decimal places for residuals if digits is 0
    if (digits == 0) digits <- 2
  } else {
    display_mat <- x
    show_margins <- margins
  }

  # Create data frame for gt
  x_mat <- matrix(as.vector(display_mat), nrow = nrow(display_mat), ncol = ncol(display_mat),
                  dimnames = list(rnames, cnames))
  df <- as.data.frame(x_mat, check.names = FALSE)

  # Add row totals if margins requested (only for frequencies)
  if (show_margins) {
    df$Total <- rowSums(x)  # Always use frequencies for totals
  }

  # Convert to character for display
  df_display <- df
  for (col in names(df_display)) {
    if (is.numeric(df_display[[col]])) {
      df_display[[col]] <- format(round(df_display[[col]], digits), nsmall = digits)
    }
  }

  # Add row names as first column
  df_display <- cbind(data.frame(row_var = rownames(df)), df_display)
  names(df_display)[1] <- rvar

  # Add column totals row if margins requested (only for frequencies)
  if (show_margins) {
    col_totals <- c("Total", as.character(colSums(x)),
                    as.character(sum(x)))
    df_display <- rbind(df_display, col_totals)
  }

  # Set up color palette
  if (is.null(palette)) {
    if (shade == "freq") {
      palette <- c("white", "firebrick")
    } else {
      palette <- c("#B2182B", "white", "#2166AC")
    }
  }

  # Determine domain for colors
  if (shade == "freq") {
    domain <- c(0, max(shade_values, na.rm = TRUE))
  } else {
    max_abs <- max(abs(shade_values), na.rm = TRUE)
    domain <- c(-max_abs, max_abs)
  }

  # Build gt table
  gt_tbl <- gt::gt(df_display, rowname_col = rvar)

  # Apply colors to data cells (not totals)
  n_row <- nrow(shade_values)
  n_col <- ncol(shade_values)

  # Color function for background
  color_fn <- scales::col_numeric(palette = palette, domain = domain)

  # Helper function to determine text color based on background
  # Uses colorspace::contrast_ratio if available, otherwise luminance-based fallback
  get_text_color <- function(bg_color) {
    if (requireNamespace("colorspace", quietly = TRUE)) {
      contrast_white <- colorspace::contrast_ratio(bg_color, "white")
      contrast_black <- colorspace::contrast_ratio(bg_color, "black")
      if (contrast_white > contrast_black) "white" else "black"
    } else {
      rgb_vals <- grDevices::col2rgb(bg_color) / 255
      luminance <- 0.2126 * rgb_vals[1] + 0.7152 * rgb_vals[2] + 0.0722 * rgb_vals[3]
      if (luminance < 0.5) "white" else "black"
    }
  }

  # Apply cell-by-cell coloring with appropriate text color
  for (i in 1:n_row) {
    for (j in 1:n_col) {
      col_name <- cnames[j]
      bg_color <- color_fn(shade_values[i, j])
      text_color <- get_text_color(bg_color)

      gt_tbl <- gt_tbl |>
        gt::tab_style(
          style = list(
            gt::cell_fill(color = bg_color),
            gt::cell_text(color = text_color)
          ),
          locations = gt::cells_body(columns = col_name, rows = i)
        )
    }
  }

  # Style the totals row and column
  if (show_margins) {
    gt_tbl <- gt_tbl |>
      gt::tab_style(
        style = gt::cell_fill(color = "#D1FFBD"),
        locations = gt::cells_body(columns = "Total")
      )

    gt_tbl <- gt_tbl |>
      gt::tab_style(
        style = gt::cell_fill(color = "#D1FFBD"),
        locations = gt::cells_body(rows = n_row + 1)
      ) |>
      gt::tab_style(
        style = gt::cell_fill(color = "#D1FFBD"),
        locations = gt::cells_stub(rows = n_row + 1)
      )
  }

  # Add column spanner for the column variable name
  gt_tbl <- gt_tbl |>
    gt::tab_spanner(
      label = cvar,
      columns = dplyr::all_of(cnames)
    )

  # Add title if provided
  if (!is.null(title)) {
    gt_tbl <- gt_tbl |> gt::tab_header(title = title)
  }

  # Add styling
  gt_tbl <- gt_tbl |>
    gt::tab_options(
      table.font.size = 14,
      column_labels.font.weight = "bold",
      row_group.font.weight = "bold"
    ) |>
    gt::cols_align(align = "center", columns = dplyr::everything())

  # Style row labels (stub) to match column label styling
  # Bold for category labels, italic for Total
  if (show_margins) {
    gt_tbl <- gt_tbl |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_stub(rows = seq_len(n_row))
      )

    gt_tbl <- gt_tbl |>
      gt::tab_style(
        style = gt::cell_text(style = "italic"),
        locations = gt::cells_body(columns = "Total")
      ) |>
      gt::tab_style(
        style = gt::cell_text(style = "italic"),
        locations = gt::cells_column_labels(columns = "Total")
      )

    gt_tbl <- gt_tbl |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold", style = "italic"),
        locations = gt::cells_stub(rows = n_row + 1)
      ) |>
      gt::tab_style(
        style = gt::cell_text(style = "italic"),
        locations = gt::cells_body(rows = n_row + 1)
      )
  } else {
    gt_tbl <- gt_tbl |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_stub()
      )
  }

  # Add source note legend if requested

  show_legend <- isTRUE(legend) || identical(legend, "note")
  if (show_legend && !is.null(shading_message)) {
    gt_tbl <- gt_tbl |> gt::tab_source_note(source_note = shading_message)
  }

  # Save to file if filename is provided
  if (!is.null(filename)) {
    gt::gtsave(gt_tbl, filename = filename, ...)
  }

  return(gt_tbl)
}
