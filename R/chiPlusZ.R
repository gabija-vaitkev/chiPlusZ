#' chiPlusZ: Chi-Squared + SPSS-Style Column Proportion Z-Tests
#'
#' @description
#' Replicates SPSS's "Compare Column Proportions" workflow for two-way
#' contingency tables.  The main entry point is [crosstabs()].
#'
#' @keywords internal
"_PACKAGE"


# ── Internal helpers ──────────────────────────────────────────────────────────

#' Pairwise column-proportion Z-tests with Bonferroni correction
#'
#' @param tab An integer matrix (rows = variable1, cols = variable2).
#' @param alpha Numeric scalar. Significance threshold (default `0.05`).
#'
#' @return A list with elements:
#'   * `letters`  – character matrix of CLD letters (same dims as `tab`).
#'   * `p_table`  – data frame of every pairwise comparison with raw and
#'                  Bonferroni-adjusted p-values.
#'
#' @noRd
.compare_proportions <- function(tab, alpha = 0.05) {

  rows      <- rownames(tab)
  cols      <- colnames(tab)
  num_cols  <- length(cols)
  num_rows  <- length(rows)
  col_tots  <- colSums(tab)

  letter_mat <- matrix("", nrow = num_rows, ncol = num_cols,
                       dimnames = list(rows, cols))

  all_pairs <- data.frame(
    row_cat   = character(),
    col_pair  = character(),
    col_i     = character(),
    col_j     = character(),
    p1        = numeric(),
    p2        = numeric(),
    z_stat    = numeric(),
    p_raw     = numeric(),
    p_bonf    = numeric(),
    stringsAsFactors = FALSE
  )

  for (r in seq_len(num_rows)) {

    # Sanitize col names so they contain no dashes (fixes multcompLetters parsing)
    safe_cols        <- gsub("-", "_", cols, fixed = TRUE)
    names(safe_cols) <- cols   # safe_cols["50-64"] == "50_64"

    p_values   <- numeric(0)
    comp_names <- character(0)
    detail_rows <- list()

    for (i in seq_len(num_cols - 1)) {
      for (j in seq(i + 1, num_cols)) {

        x1 <- tab[r, i]
        x2 <- tab[r, j]
        n1 <- col_tots[i]
        n2 <- col_tots[j]

        # ── Safety checks ────────────────────────────────────────────────────
        skip_reason <- NULL
        if (x1 == 0 || x2 == 0) {
          skip_reason <- "observed count is zero"
        } else if (n1 < 5 || n2 < 5) {
          skip_reason <- "column total < 5"
        }

        comp_label <- paste0(safe_cols[cols[i]], "-", safe_cols[cols[j]])

        if (!is.null(skip_reason)) {
          message(sprintf(
            "  [chiPlusZ] Skipping %s | row '%s' vs cols '%s'/'%s': %s.",
            comp_label, rows[r], cols[i], cols[j], skip_reason
          ))
          p_values   <- c(p_values, 1)
          comp_names <- c(comp_names, comp_label)
          detail_rows[[length(detail_rows) + 1]] <- data.frame(
            row_cat  = rows[r], col_pair = comp_label,
            col_i = cols[i], col_j = cols[j],
            p1 = NA_real_, p2 = NA_real_,
            z_stat = NA_real_, p_raw = NA_real_, p_bonf = NA_real_,
            stringsAsFactors = FALSE
          )
          next
        }

        p1       <- x1 / n1
        p2       <- x2 / n2
        p_pool   <- (x1 + x2) / (n1 + n2)
        se       <- sqrt(p_pool * (1 - p_pool) * (1 / n1 + 1 / n2))

        if (!is.finite(se) || se == 0) {
          z_stat <- 0
          p_val  <- 1
        } else {
          z_stat <- (p1 - p2) / se
          p_val  <- 2 * (1 - stats::pnorm(abs(z_stat)))
          if (!is.finite(p_val)) p_val <- 1
        }

        p_values   <- c(p_values, p_val)
        comp_names <- c(comp_names, comp_label)
        detail_rows[[length(detail_rows) + 1]] <- data.frame(
          row_cat = rows[r], col_pair = comp_label,
          col_i = cols[i], col_j = cols[j],
          p1 = p1, p2 = p2,
          z_stat = z_stat, p_raw = p_val, p_bonf = NA_real_,
          stringsAsFactors = FALSE
        )
      }
    }

    # ── Bonferroni adjustment ────────────────────────────────────────────────
    num_comps <- length(p_values)
    p_adj     <- pmin(p_values * num_comps, 1.0)
    names(p_adj) <- comp_names

    for (k in seq_along(detail_rows)) {
      cp <- detail_rows[[k]]$col_pair
      detail_rows[[k]]$p_bonf <- p_adj[cp]
    }
    all_pairs <- rbind(all_pairs, do.call(rbind, detail_rows))

    # ── Compact letter display ───────────────────────────────────────────────
    sig_diff    <- p_adj < alpha
    letters_out <- multcompView::multcompLetters(sig_diff)$Letters
    # Map safe names back to original col names
    letter_mat[r, ] <- letters_out[safe_cols[cols]]
  }

  list(letters = letter_mat, p_table = all_pairs)
}


#' Compute Cramér's V from a Chi-squared result
#'
#' @param chi2_res Object returned by [stats::chisq.test()].
#' @param n Total sample size.
#' @param tab The original contingency table (to derive dimensions).
#'
#' @return Named numeric vector with elements `V`, `chi2`, `df`, `p`.
#' @noRd
.cramers_v <- function(chi2_res, n, tab) {
  k   <- min(dim(tab) - 1)
  V   <- sqrt(chi2_res$statistic / (n * k))
  c(V = unname(V),
    chi2 = unname(chi2_res$statistic),
    df   = unname(chi2_res$parameter),
    p    = chi2_res$p.value)
}


# ── p-value formatter (scientific for tiny values) ───────────────────────────
.fmt_p <- function(p) {
  ifelse(is.na(p), NA_character_,
    ifelse(p < 0.0001,
           formatC(p, format = "e", digits = 3),
           formatC(p, format = "f", digits = 4)))
}


# ── Main exported function ────────────────────────────────────────────────────

#' Run SPSS-style cross-tabulation with column proportion Z-tests
#'
#' @description
#' Builds a two-way contingency table from a long-format data frame, runs an
#' overall Chi-squared test, computes Cramér's V, performs pairwise
#' column-proportion Z-tests (Bonferroni-corrected), and produces a compact
#' letter display (CLD) - mirroring SPSS's **Compare Column Proportions**
#' output.
#'
#' @param data A data frame in long format.
#' @param row_var Character string naming the **row** variable (categories
#'   displayed as table rows, e.g. `"Commute"`).
#' @param col_var Character string naming the **column** variable (groups
#'   to compare, e.g. `"AgeGroup"`).
#' @param row_order Optional character vector giving the desired row order.
#'   Defaults to alphabetical.
#' @param col_order Optional character vector giving the desired column order.
#'   Defaults to alphabetical.
#' @param alpha Numeric. Significance level for Bonferroni-corrected tests
#'   (default `0.05`).
#' @param export_excel Logical. If `TRUE`, results are written to an Excel
#'   workbook (default `FALSE`).
#' @param excel_path Character. Path/filename for the Excel output
#'   (default `"crosstabs_output.xlsx"`).
#'
#' @return An S3 object of class `"crosstabs"` (invisibly), a named list
#'   containing:
#' \describe{
#'   \item{`counts`}{Integer matrix - the contingency table of raw counts.}
#'   \item{`row_pct`}{Numeric matrix - row percentages (sum to 100 across cols).}
#'   \item{`expected`}{Numeric matrix - expected counts under H0.}
#'   \item{`chi2`}{Named numeric vector: `chi2`, `df`, `p`.}
#'   \item{`cramers_v`}{Named numeric vector: `V`, `chi2`, `df`, `p`.}
#'   \item{`letters`}{Character matrix - CLD letters per cell.}
#'   \item{`display`}{Character matrix - combined `"Count (row%) <letter>"` strings.}
#'   \item{`p_table`}{Data frame - all pairwise raw and Bonferroni p-values.}
#'   \item{`low_expected_warning`}{Logical - `TRUE` if any expected count < 5.}
#' }
#'
#' @examples
# set.seed(123)
# n   <- 450
# df  <- data.frame(
#   Commute  = sample(
#     c("Bike", "Car", "Transit"),
#     n, replace = TRUE, prob = c(0.15, 0.55, 0.30)
#   ),
#   AgeGroup = sample(
#     c("Young", "Middle", "Senior"),
#     n, replace = TRUE, prob = c(0.50, 0.30, 0.20)
#   )
# )
# result_excel <- crosstabs(
#   data         = df,
#   row_var      = "Commute",
#   col_var      = "AgeGroup",
#   row_order    = c("Transit", "Bike", "Car"),
#   col_order    = c("Young", "Middle", "Senior"),
#   export_excel = FALSE,
#   excel_path   = "commute_by_age.xlsx"
# )
# print(result_excel)
#'
#' @importFrom stats chisq.test pnorm
#' @importFrom multcompView multcompLetters
#' @export
crosstabs <- function(data,
                      row_var,
                      col_var,
                      row_order    = NULL,
                      col_order    = NULL,
                      alpha        = 0.05,
                      export_excel = FALSE,
                      excel_path   = "crosstabs_output.xlsx") {

  # ── Input validation ─────────────────────────────────────────────────────
  if (!is.data.frame(data))
    stop("`data` must be a data frame.", call. = FALSE)
  if (!row_var %in% names(data))
    stop(sprintf("Column '%s' not found in `data`.", row_var), call. = FALSE)
  if (!col_var %in% names(data))
    stop(sprintf("Column '%s' not found in `data`.", col_var), call. = FALSE)

  # ── Build contingency table ──────────────────────────────────────────────
  raw_tab <- table(data[[row_var]], data[[col_var]])

  if (!is.null(row_order)) {
    missing_r <- setdiff(row_order, rownames(raw_tab))
    if (length(missing_r))
      warning("row_order contains levels not in data: ",
              paste(missing_r, collapse = ", "), call. = FALSE)
    row_order <- intersect(row_order, rownames(raw_tab))
    raw_tab   <- raw_tab[row_order, , drop = FALSE]
  }
  if (!is.null(col_order)) {
    missing_c <- setdiff(col_order, colnames(raw_tab))
    if (length(missing_c))
      warning("col_order contains levels not in data: ",
              paste(missing_c, collapse = ", "), call. = FALSE)
    col_order <- intersect(col_order, colnames(raw_tab))
    raw_tab   <- raw_tab[, col_order, drop = FALSE]
  }

  counts <- as.matrix(raw_tab)

  # ── Chi-squared & Cramér's V ──────────────────────────────────────────────
  chi2_res <- stats::chisq.test(counts)
  expected <- round(chi2_res$expected, 2)

  low_exp_warn <- any(expected < 5)
  if (low_exp_warn) {
    warning(
      "Some expected cell counts are < 5. Chi-squared approximation may be ",
      "unreliable. Consider collapsing categories or using Fisher's exact test.",
      call. = FALSE
    )
  }

  n        <- sum(counts)
  cv       <- .cramers_v(chi2_res, n, counts)

  # ── Post-hoc Z-tests ─────────────────────────────────────────────────────
  ph       <- .compare_proportions(counts, alpha = alpha)
  letters  <- ph$letters
  p_table  <- ph$p_table

  # ── Display matrix (count + row% [2 dp] + letter) ────────────────────────
  row_pct  <- round(prop.table(counts, margin = 1) * 100, 2)   # [1] 2 dp
  display  <- matrix(
    paste0(counts, " (", row_pct, "%) ", letters),
    nrow     = nrow(counts),
    dimnames = dimnames(counts)
  )

  # ── Assemble result object ────────────────────────────────────────────────
  result <- structure(
    list(
      counts               = counts,
      row_pct              = row_pct,
      expected             = expected,
      chi2                 = c(chi2   = unname(chi2_res$statistic),
                               df     = unname(chi2_res$parameter),
                               p      = chi2_res$p.value),
      cramers_v            = cv,
      letters              = letters,
      display              = display,
      p_table              = p_table,
      low_expected_warning = low_exp_warn,
      row_var              = row_var,
      col_var              = col_var,
      alpha                = alpha
    ),
    class = "crosstabs"
  )

  # ── Optional Excel export ─────────────────────────────────────────────────
  if (export_excel) {
    .export_excel(result, excel_path)
    message("chiPlusZ: Results exported to '", excel_path, "'.")
  }

  invisible(result)
}


# ── S3 print method ───────────────────────────────────────────────────────────

#' Print method for `crosstabs` objects
#'
#' @param x A `crosstabs` object returned by [crosstabs()].
#' @param ... Currently unused.
#'
#' @return `x`, invisibly.
#' @export
print.crosstabs <- function(x, ...) {

  sep  <- strrep("─", 60)
  sep2 <- strrep("═", 60)

  cat("\n", sep2, "\n", sep = "")
  cat(sprintf("  chiPlusZ  |  %s (rows)  ×  %s (cols)\n",
              x$row_var, x$col_var))
  cat(sep2, "\n\n", sep = "")

  # Overall Chi-Squared
  cat("── Overall Chi-Squared Test", sep, "\n", sep = "\n")
  cat(sprintf("  χ²(%d) = %.3f,  p = %s\n\n",
              x$chi2["df"], x$chi2["chi2"], .fmt_p(x$chi2["p"])))

  # Effect size
  cat("── Effect Size\n")
  cat(sprintf("  Cramér's V = %.3f\n\n", x$cramers_v["V"]))

  if (x$low_expected_warning)
    cat("  ⚠  Warning: some expected counts < 5.",
        "Interpret χ² with caution.\n\n")

  # Counts + Expected
  cat("── Contingency Table (Counts | Expected)\n")
  obs_exp <- matrix(
    paste0(x$counts, "  [", x$expected, "]"),
    nrow = nrow(x$counts), dimnames = dimnames(x$counts)
  )
  print(as.data.frame(obs_exp))
  cat("\n")

  # CLD display
  cat("── Column Proportion Z-Tests (Bonferroni)\n")
  cat(sprintf("   Significance level: α = %.3f\n", x$alpha))
  cat("   Cell format: Count (row%) <letter>\n")
  cat("   Columns sharing a letter are NOT significantly different.\n\n")
  print(as.data.frame(x$display))
  cat("\n")

  # p-value table (non-NA rows only)
  p_show <- x$p_table[!is.na(x$p_table$p_bonf), ]
  if (nrow(p_show) > 0) {
    cat("── Pairwise Bonferroni-Adjusted p-Values\n")
    p_show$p_raw  <- .fmt_p(p_show$p_raw)
    p_show$p_bonf <- .fmt_p(as.numeric(p_show$p_bonf))
    p_show$sig    <- ifelse(as.numeric(p_show$p_bonf) < x$alpha |
                              grepl("e-", p_show$p_bonf), "*", "")
    # Rename columns for console display
    p_show$Category <- p_show$row_cat
    print(p_show[, c("Category", "col_pair", "p_raw", "p_bonf", "sig")],
          row.names = FALSE)
    cat("  (* = significant after Bonferroni correction)\n")
  }

  cat("\n", sep2, "\n\n", sep = "")
  invisible(x)
}


# ── Excel export ──────────────────────────────────────────────────────────────

#' Write a `crosstabs` result to an Excel workbook
#'
#' @param x A `crosstabs` object.
#' @param path Character. Output file path.
#' @return `path`, invisibly.
#' @noRd
.export_excel <- function(x, path) {

  wb <- openxlsx::createWorkbook()

  # ── Shared label: Variable1 x Variable2  [3] unified style ───────────────
  analysis_label <- sprintf("%s x %s", x$row_var, x$col_var)

  # ── Styles ────────────────────────────────────────────────────────────────
  hdr_style <- openxlsx::createStyle(
    fontColour = "#FFFFFF", fgFill = "#1D2342",
    halign = "CENTER", textDecoration = "bold",
    border = "Bottom", borderColour = "#FFFFFF"
  )
  title_style <- openxlsx::createStyle(
    fontSize = 13, textDecoration = "bold"
  )
  warn_style <- openxlsx::createStyle(
    fontColour = "#C00000", textDecoration = "italic"
  )
  sig_style <- openxlsx::createStyle(
    fgFill = "#DDF0FF"
  )

  # ════════════════════════════════════════════════════════════════════════════
  # Sheet 1 : Summary
  # ════════════════════════════════════════════════════════════════════════════
  openxlsx::addWorksheet(wb, "Summary")

  row_cursor <- 1

  # Title  "Variable1 x Variable2"
  openxlsx::writeData(wb, "Summary",
    x = data.frame(V1 = sprintf("Cross-tabulation: %s", analysis_label)),
    startRow = row_cursor, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, "Summary", title_style,
                     rows = row_cursor, cols = 1)
  row_cursor <- row_cursor + 2

  # Chi-Squared block
  chi_df <- data.frame(
    Statistic = c("Chi-Squared (χ²)", "Degrees of Freedom", "p-value",
                  "Cramér's V"),
    Value     = c(round(x$chi2["chi2"], 4),
                  x$chi2["df"],
                  .fmt_p(x$chi2["p"]),
                  round(x$cramers_v["V"], 4))
  )
  openxlsx::writeData(wb, "Summary", x = chi_df,
                      startRow = row_cursor, startCol = 1,
                      headerStyle = hdr_style)
  row_cursor <- row_cursor + nrow(chi_df) + 2

  # Optional warning
  if (x$low_expected_warning) {
    openxlsx::writeData(wb, "Summary",
      x = data.frame(V1 = "⚠  Warning: some expected counts < 5. χ² may be unreliable."),
      startRow = row_cursor, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, "Summary", warn_style,
                       rows = row_cursor, cols = 1)
    row_cursor <- row_cursor + 2
  }

  # Contingency table title  [3] "Variable1 x Variable2"
  openxlsx::writeData(wb, "Summary",
    x = data.frame(V1 = sprintf("%s Crosstabulation", analysis_label)),
    startRow = row_cursor, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, "Summary", title_style,
                     rows = row_cursor, cols = 1)
  row_cursor <- row_cursor + 1

  # Stacked Count / Expected Count / % within row_var
  col_names   <- colnames(x$counts)
  row_names   <- rownames(x$counts)
  total_count <- colSums(x$counts)
  total_n     <- sum(x$counts)
  total_pct   <- round(total_count / total_n * 100, 2)
  row_totals  <- rowSums(x$counts)
  row_exp_tot <- rowSums(x$expected)

  stacked_rows <- list()
  for (r in row_names) {
    stacked_rows[[length(stacked_rows) + 1]] <- c(
      as.character(x$counts[r, ]), Total = as.character(row_totals[r])
    )
    stacked_rows[[length(stacked_rows) + 1]] <- c(
      as.character(x$expected[r, ]), Total = as.character(round(row_exp_tot[r], 2))
    )
    stacked_rows[[length(stacked_rows) + 1]] <- c(
      paste0(x$row_pct[r, ], "%"), Total = "100.00%"
    )
  }
  stacked_rows[[length(stacked_rows) + 1]] <- c(
    as.character(total_count), Total = as.character(total_n)
  )
  stacked_rows[[length(stacked_rows) + 1]] <- c(
    as.character(total_count), Total = as.character(total_n)
  )
  stacked_rows[[length(stacked_rows) + 1]] <- c(
    paste0(total_pct, "%"), Total = "100.00%"
  )

  stacked_df <- as.data.frame(do.call(rbind, stacked_rows),
                               stringsAsFactors = FALSE)
  names(stacked_df) <- c(col_names, "Total")

  # Build the two left-hand label columns
  n_rows_each <- 3
  row_var_col <- rep(c(row_names, "Total"), each = n_rows_each)
  stat_col    <- rep(c("Count", "Expected Count",
                       paste0("% within ", x$row_var)), length(row_names) + 1)

  stacked_df <- cbind(
    stats::setNames(data.frame(row_var_col, stat_col, stringsAsFactors = FALSE),
                    c(x$row_var, "Statistic")),
    stacked_df
  )

  openxlsx::writeData(wb, "Summary", x = stacked_df,
                      startRow = row_cursor, startCol = 1,
                      headerStyle = hdr_style)
  row_cursor <- row_cursor + nrow(stacked_df) + 2

  # Column widths: col A = 32, rest auto
  openxlsx::setColWidths(wb, "Summary", cols = 1,      widths = 32)
  openxlsx::setColWidths(wb, "Summary", cols = 2:20,   widths = "auto")

  # ════════════════════════════════════════════════════════════════════════════
  # Sheet 2 : PostHoc
  # ════════════════════════════════════════════════════════════════════════════
  openxlsx::addWorksheet(wb, "PostHoc")

  row_cursor <- 1

  # Analysis label at the top of PostHoc sheet, same style as Summary
  openxlsx::writeData(wb, "PostHoc",
    x = data.frame(V1 = sprintf("Cross-tabulation: %s", analysis_label)),
    startRow = row_cursor, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, "PostHoc", title_style,
                     rows = row_cursor, cols = 1)
  row_cursor <- row_cursor + 2

  # CLD table title
  openxlsx::writeData(wb, "PostHoc",
    x = data.frame(V1 = paste0(
      "Column Proportion Z-Tests with Bonferroni Correction  (α = ",
      x$alpha, ")")),
    startRow = row_cursor, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, "PostHoc", title_style,
                     rows = row_cursor, cols = 1)
  row_cursor <- row_cursor + 1

  openxlsx::writeData(wb, "PostHoc",
    x = data.frame(V1 = "Cell format: Count (row%) <letter>; shared letter = NOT significantly different"),
    startRow = row_cursor, startCol = 1, colNames = FALSE)
  row_cursor <- row_cursor + 2

  # CLD display table 
  disp_df <- as.data.frame(x$display)
  disp_df <- cbind(
    stats::setNames(data.frame(rownames(disp_df), stringsAsFactors = FALSE),
                    x$row_var),
    disp_df
  )
  openxlsx::writeData(wb, "PostHoc", x = disp_df,
                      startRow = row_cursor, startCol = 1,
                      headerStyle = hdr_style)
  row_cursor <- row_cursor + nrow(disp_df) + 2

  # Pairwise p-value table
  openxlsx::writeData(wb, "PostHoc",
    x = data.frame(V1 = "Pairwise Comparisons"),
    startRow = row_cursor, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, "PostHoc", title_style,
                     rows = row_cursor, cols = 1)
  row_cursor <- row_cursor + 1

  p_out <- x$p_table

  # Rename columns
  names(p_out)[names(p_out) == "row_cat"]  <- x$row_var
  names(p_out)[names(p_out) == "col_pair"] <- "pair"
  names(p_out)[names(p_out) == "col_i"]   <- "i"
  names(p_out)[names(p_out) == "col_j"]   <- "j"

  # is_significant column; raw (unrounded) p values formatted
  p_out$is_significant <- ifelse(!is.na(p_out$p_bonf) &
                                   p_out$p_bonf < x$alpha, "yes", "no")
  p_out$p_raw  <- .fmt_p(p_out$p_raw)
  p_out$p_bonf <- .fmt_p(as.numeric(p_out$p_bonf))

  openxlsx::writeData(wb, "PostHoc", x = p_out,
                      startRow = row_cursor, startCol = 1,
                      headerStyle = hdr_style)

  # Highlight significant rows
  sig_rows <- which(p_out$is_significant == "yes") + row_cursor
  if (length(sig_rows) > 0) {
    openxlsx::addStyle(wb, "PostHoc", sig_style,
                       rows = sig_rows, cols = seq_len(ncol(p_out)),
                       gridExpand = TRUE)
  }

  # Column widths: col A = 32, rest auto
  openxlsx::setColWidths(wb, "PostHoc", cols = 1,    widths = 32)
  openxlsx::setColWidths(wb, "PostHoc", cols = 2:20, widths = "auto")

  openxlsx::saveWorkbook(wb, file = path, overwrite = TRUE)
  invisible(path)
}
