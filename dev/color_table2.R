# color_table2.R - S3 refactoring prototype
#
# This is a prototype for refactoring color_table() as an S3 generic with
# specific methods for different input types:
#   - table (also handles xtabs)
#   - ftable
#   - structable
#   - data.frame (frequency form with Freq or count column)
#   - matrix
#
# Each method converts its input to a standardized 2D matrix form, then
# calls the internal .color_table_impl() function to build the gt table.

#' Display Frequency Table with Colored Cell Backgrounds
#'
#' Creates a formatted table display of frequency data with cell backgrounds
#' colored according to observed frequencies or their residuals from a loglinear model.
#'
#' @param x A table, xtabs, matrix, ftable, structable, or data.frame object
#' @param ... Additional arguments passed to methods
#'
#' @export
color_table <- function(x, ...) {
  UseMethod("color_table")
}

#' @describeIn color_table Method for table objects (including xtabs)
#' @param formula Formula specifying row ~ col layout (for multi-way tables)
#' @export
color_table.table <- function(x,
                               formula = NULL,
                               shade = c("residuals", "freq", "pearson", "deviance"),
                               model = NULL,
                               expected = NULL,
                               palette = NULL,
                               legend = TRUE,
                               margins = TRUE,
                               digits = 0,
                               title = NULL,
                               filename = NULL,
                               ...) {

  shade <- match.arg(shade)
  if (shade == "pearson") shade <- "residuals"

  dims <- dim(x)
  ndim <- length(dims)

  # For multi-way tables with formula, use structable to flatten

  if (ndim > 2 && !is.null(formula)) {
    if (requireNamespace("vcd", quietly = TRUE)) {
      st <- vcd::structable(formula, data = x)
      x_mat <- as.matrix(st)
    } else {
      stop("Package 'vcd' is required to use formula with multi-way tables.")
    }
  } else if (ndim > 2) {
    warning("Multi-way table without formula: collapsing to 2D. Use formula for better control.")
    x_mat <- as.matrix(margin.table(x, 1:2))
  } else {
    x_mat <- as.matrix(x)
  }

  .color_table_impl(x_mat,
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
                                shade = c("residuals", "freq", "pearson", "deviance"),
                                model = NULL,
                                expected = NULL,
                                palette = NULL,
                                legend = TRUE,
                                margins = TRUE,
                                digits = 0,
                                title = NULL,
                                filename = NULL,
                                ...) {

  shade <- match.arg(shade)
  if (shade == "pearson") shade <- "residuals"

  # Convert ftable to matrix
  x_mat <- as.matrix(x)

  .color_table_impl(x_mat,
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
                                    shade = c("residuals", "freq", "pearson", "deviance"),
                                    model = NULL,
                                    expected = NULL,
                                    palette = NULL,
                                    legend = TRUE,
                                    margins = TRUE,
                                    digits = 0,
                                    title = NULL,
                                    filename = NULL,
                                    ...) {

  shade <- match.arg(shade)
  if (shade == "pearson") shade <- "residuals"

  # Convert structable to matrix (flattened 2D form)
  x_mat <- as.matrix(x)

  .color_table_impl(x_mat,
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
                                    shade = c("residuals", "freq", "pearson", "deviance"),
                                    model = NULL,
                                    expected = NULL,
                                    palette = NULL,
                                    legend = TRUE,
                                    margins = TRUE,
                                    digits = 0,
                                    title = NULL,
                                    filename = NULL,
                                    ...) {

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
                    title = title,
                    filename = filename,
                    ...)
}

#' @describeIn color_table Method for matrix objects
#' @export
color_table.matrix <- function(x,
                                shade = c("residuals", "freq", "pearson", "deviance"),
                                model = NULL,
                                expected = NULL,
                                palette = NULL,
                                legend = TRUE,
                                margins = TRUE,
                                digits = 0,
                                title = NULL,
                                filename = NULL,
                                ...) {

  shade <- match.arg(shade)
  if (shade == "pearson") shade <- "residuals"

  .color_table_impl(x,
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


# =============================================================================
# Internal implementation function
# =============================================================================

#' Internal workhorse function for color_table
#'
#' This function takes a 2D matrix and builds the gt table with colored cells.
#' All S3 methods convert their input to a matrix and call this function.
#'
#' @param x A 2D matrix with row and column names
#' @param shade Type of shading
#' @param model Optional fitted model
#' @param expected Optional expected frequencies
#' @param palette Color palette
#' @param legend Show legend?
#' @param margins Show margins?
#' @param digits Decimal places
#' @param title Table title
#' @param filename Optional file to save
#' @param ... Additional arguments passed to gtsave
#'
#' @keywords internal
.color_table_impl <- function(x,
                               shade = "residuals",
                               model = NULL,
                               expected = NULL,
                               palette = NULL,
                               legend = TRUE,
                               margins = TRUE,
                               digits = 0,
                               title = NULL,
                               filename = NULL,
                               ...) {

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
      # Fit independence model on the 2D table
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
  x_mat <- matrix(as.vector(x), nrow = nrow(x), ncol = ncol(x),
                  dimnames = list(rnames, cnames))
  df <- as.data.frame(x_mat, check.names = FALSE)

  # Add row totals if margins requested
  if (margins) {
    df$Total <- rowSums(x)
  }

  # Convert to character for display (preserving integers)
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
      palette <- c("white", "firebrick")
    } else {
      palette <- c("#2166AC", "white", "#B2182B")
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
  if (margins) {
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

  # Style row labels (stub)
  if (margins) {
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

  # Save to file if filename is provided
  if (!is.null(filename)) {
    gt::gtsave(gt_tbl, filename = filename, ...)
  }

  return(gt_tbl)
}
