# ✔️DONE: Initial implementation, using `gt` pkg for the table
#
# ✔️DONE: When the cell is colored a dark shade, use white text for readability.
#         Uses colorspace::contrast_ratio() to pick optimal text color.
# ✔️DONE: The default palette for diverging uses darker shades from RdBu (#2166AC, #B2182B)
# ✔️DONE: Suppress Warning `In chisq.test(x) : Chi-squared approximation may be incorrect`
# ✔️DONE: Test cases in dev/test-color_table.R using vcd::Suicide, vcd::PreSex, vcdExtra::Abortion
#
# ✔️DONE: Fixed bug in format(round(...)) - now checks is.numeric() before applying round()
#         The error occurred because as.data.frame() on a matrix can include factor columns.
#
# ‼ TODO: [HARD] When there are two (or more) variables for the row, these should appear as a nested
#         hierarchy similar
#         to what is shown in the table for the Titanic data in `dev/Titanic-residual-shading.png`. That is,
#         the Hair-Sex combinations that appear like "Black_Male" should be two columns for Hair and Sex, and
#         the rows for the other cases of black hair should have Sex empty. Not sure whether nested row groups
#         can be shown to look nested otherwise.
#
# ✔️DONE:  Make the display of the Total row and column optional. -- handled by `marginss`
#
# ✔️DONE: Row category labels (stub) now bold, matching column labels; Total row stub is italic.
#         Future enhancement: could extend `margins` to accept a list for custom styling.
#
# ‼ TODO: Should also allow the input argument, x, to be a dataset in frequency form.
#
# ‼ TODO: Consider use of patterned backgrounds using gt facilities-- e.g., opt_stylize()
#
# ‼ TODO: To handle  table, xtabs, ftable, or structable objects as the input, perhaps it would be better to
#         reorganize this as an S3 generic, with specific methods for table, xtabs, ftable, structable objects.
#
# ‼ TODO: Add filename arg, which if not NULL saves the `gt` result as an image. Needed because gt output is
#         hard to show in Rmd / qmd output. Most likely use `gt::gtsave()`. NB: `gt` documentation uses
#         `.svg` files in the README as: <img src="man/figures/gt_sp500_table.svg" width="800px">.
#
#         In man pages via roxygen it uses for examples:
#              \if{html}{\out{
#                 `r man_get_image_tag(file = "man_gt_1.png")`
#               }}
#


#' Display Frequency Table with Colored Cell Backgrounds
#'
#' Creates a formatted table display of frequency data with cell backgrounds
#' colored according to observed frequencies or their residuals from a loglinear model.
#'
#' @param x A table, xtabs, matrix, ftable, or structable object
#' @param formula Formula specifying row ~ col layout (passed to structable if needed)
#' @param shade What values determine cell shading: "residuals" (default), "freq",
#'   "pearson", or "deviance"
#' @param model A fitted model (loglm or glm) to compute residuals from.
#'   If NULL and shade involves residuals, uses independence model.
#' @param expected Expected frequencies (alternative to model)
#' @param palette Color palette function or vector. Default depends on shade type.
#' @param legend Logical, show color legend/scale?
#' @param margins Logical, include row/column totals?
#' @param digits Number of decimal places for displayed values
#' @param title Optional table title
#' @param ... Additional arguments (currently unused)
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
#' For cells with dark background colors, black text can be difficult to read.
#' This function automatically selects white or black text for each cell based
#' on which provides better contrast against the background color. If the
#' \pkg{colorspace} package is available, \code{\link[colorspace]{contrast_ratio}}
#' is used to determine the optimal text color according to WCAG 2.1 guidelines.
#' Otherwise, a fallback based on relative luminance (ITU-R BT.709) is used.
#'
#' @return A gt table object that can be further customized
#'
#' @examples
#' \dontrun{
#' # Basic usage - shade by residuals from independence
#' data(HairEyeColor)
#' HEC <- margin.table(HairEyeColor, 2:1)
#' color_table(HEC)
#'
#' # Shade by frequencies instead
#' color_table(HEC, shade = "freq")
#'
#' # 3-way table
#' color_table(HairEyeColor, formula = Eye ~ Hair + Sex)
#' }
#'
#' @importFrom stats chisq.test residuals
#' @importFrom grDevices col2rgb
#' @importFrom dplyr all_of everything
#' @export
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
                        ...) {


  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Package 'gt' is required for color_table(). Please install it.")
  }

  shade <- match.arg(shade)
  if (shade == "pearson") shade <- "residuals"


  # Store original for potential model fitting

  x_orig <- x

  # Handle structable input
  if (inherits(x, "structable")) {
    # Convert structable to matrix (flattened 2D form)
    x <- as.matrix(x)
  }

  # Handle ftable input
  if (inherits(x, "ftable")) {
    x <- as.matrix(x)
  }

  # For multi-way tables with formula, use structable to flatten
  dims <- dim(x)
  ndim <- length(dims)

  if (ndim > 2 && !is.null(formula)) {
    # Use structable to flatten according to formula
    if (requireNamespace("vcd", quietly = TRUE)) {
      st <- vcd::structable(formula, data = x)
      # Convert to matrix to get true 2D flattened form
      x <- as.matrix(st)
      dims <- dim(x)
      ndim <- 2
    }
  } else if (ndim > 2) {
    # Flatten to 2D by collapsing dimensions
    warning("Multi-way table without formula: collapsing to 2D. Use formula for better control.")
    x <- margin.table(x, 1:2)
    dims <- dim(x)
    ndim <- 2
  }

  # Ensure x is a matrix at this point
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }

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

  # Compute shading values
  if (shade == "freq") {
    shade_values <- x
  } else {
    # Compute residuals
    if (!is.null(model)) {
      # Use provided model
      resid_mat <- matrix(residuals(model, type = "pearson"),
                          nrow = dims[1], ncol = dims[2])
    } else if (!is.null(expected)) {
      # Use provided expected values
      resid_mat <- (x - expected) / sqrt(expected)
    } else {
      # Fit independence model on the (possibly flattened) 2D table
      # chisq.test works on matrices
      chi_result <- suppressWarnings(chisq.test(x))
      resid_mat <- chi_result$residuals
    }

    if (shade == "deviance" && is.null(model)) {
      # Compute deviance residuals
      obs <- x
      exp <- chi_result$expected
      resid_mat <- sign(obs - exp) * sqrt(2 * ifelse(obs > 0, obs * log(obs / exp), 0))
    }

    shade_values <- resid_mat
  }


  # Create data frame for gt
  # Convert to a plain matrix first to avoid table-specific behavior in as.data.frame()
  # (as.data.frame on a table gives long format with Freq column, which we don't want)
  x_mat <- matrix(as.vector(x), nrow = nrow(x), ncol = ncol(x),
                  dimnames = list(rnames, cnames))
  df <- as.data.frame(x_mat, check.names = FALSE)

  # Add row totals if margins requested
  if (margins) {
    df$Total <- rowSums(x)
  }

  # Convert to character for display (preserving integers)
  # Only apply round() to numeric columns - factors would cause an error
  df_display <- df
  for (col in names(df_display)) {
    if (is.numeric(df_display[[col]])) {
      df_display[[col]] <- format(round(df_display[[col]], digits), nsmall = digits)
    }
  }

  # Add row names as first column
 df_display <- cbind(data.frame(row_var = rownames(df)), df_display)
  names(df_display)[1] <- rvar

  # Add column totals row if margins requested
  if (margins) {
    col_totals <- c("Total", as.character(colSums(x)),
                    as.character(sum(x)))
    df_display <- rbind(df_display, col_totals)
  }

  # Set up color palette
  if (is.null(palette)) {
    if (shade == "freq") {
      # Sequential palette for frequencies
      palette <- c("white", "firebrick")
    } else {
      # Diverging palette for residuals
      palette <- c("#2166AC", "white", "#B2182B")  # darker blue/red from RdBu
    }
  }

  # Determine domain for colors
  if (shade == "freq") {
    domain <- c(0, max(shade_values, na.rm = TRUE))
  } else {
    # Symmetric domain for residuals
    max_abs <- max(abs(shade_values), na.rm = TRUE)
    domain <- c(-max_abs, max_abs)
  }

  # Build gt table
  gt_tbl <- gt::gt(df_display, rowname_col = rvar)

  # Apply colors to data cells (not totals)
  # Create a matrix of colors
  n_row <- nrow(shade_values)
  n_col <- ncol(shade_values)

  # Color function for background
  color_fn <- scales::col_numeric(palette = palette, domain = domain)

  # Helper function to determine text color based on background
  # Uses colorspace::contrast_ratio if available, otherwise luminance-based fallback
  get_text_color <- function(bg_color) {
    if (requireNamespace("colorspace", quietly = TRUE)) {
      # Use contrast_ratio to pick better text color
      contrast_white <- colorspace::contrast_ratio(bg_color, "white")
      contrast_black <- colorspace::contrast_ratio(bg_color, "black")
      if (contrast_white > contrast_black) "white" else "black"
    } else {
      # Fallback: use relative luminance calculation
      # Convert hex to RGB
      rgb_vals <- col2rgb(bg_color) / 255
      # Calculate relative luminance (ITU-R BT.709)
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

  # Style the totals row and column (light gray)
  if (margins) {
    # Total column
    gt_tbl <- gt_tbl |>
      gt::tab_style(
        style = gt::cell_fill(color = "#D1FFBD"),
        locations = gt::cells_body(columns = "Total")
      )

    # Total row (last row)
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
      columns = all_of(cnames)
    )

  # Add title if provided
  if (!is.null(title))
    gt_tbl <- gt_tbl |> gt::tab_header(title = title)

  # Add styling
gt_tbl <- gt_tbl |>
    gt::tab_options(
      table.font.size = 14,
      column_labels.font.weight = "bold",
      row_group.font.weight = "bold"
    ) |>
    gt::cols_align(align = "center", columns = everything())

  # Style row labels (stub) to match column label styling
  # Bold for category labels, italic for Total
  if (margins) {
    # Bold the row category labels (all rows except the Total row)
    gt_tbl <- gt_tbl |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_stub(rows = seq_len(n_row))
      )

    # Style "Total" column: italic for column label and all values in that column
    gt_tbl <- gt_tbl |>
      gt::tab_style(
        style = gt::cell_text(style = "italic"),
        locations = gt::cells_body(columns = "Total")
      ) |>
      gt::tab_style(
        style = gt::cell_text(style = "italic"),
        locations = gt::cells_column_labels(columns = "Total")
      )

    # Style "Total" row: bold italic for label, italic for all values
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
    # No margins - still bold the row category labels
    gt_tbl <- gt_tbl |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_stub()
      )
  }

  return(gt_tbl)
}
