# =============================================================================
#  chiPlusZ — Example script
#  Demonstrates every major feature of the package.
# =============================================================================

# ── 0. Install / load the package ─────────────────────────────────────────────
# First-time setup (run once from the repo root):
#   install.packages("devtools")
#   devtools::install_deps()          # installs multcompView + openxlsx
#   devtools::document()              # rebuilds NAMESPACE & man/ from roxygen2
#   devtools::install()               # installs chiPlusZ into your R library
#
# Then in any R session:
library(chiPlusZ)


# ── 1. Create a reproducible long-format data frame ───────────────────────────
set.seed(2025)
n   <- 450
df  <- data.frame(
  Commute  = sample(
    c("Bike", "Car", "Transit"),
    n, replace = TRUE, prob = c(0.15, 0.55, 0.30)
  ),
  AgeGroup = sample(
    c("Young", "Middle", "Senior"),
    n, replace = TRUE, prob = c(0.35, 0.40, 0.25)
  )
)


# ── 2. Basic run (console output only) ────────────────────────────────────────
result <- crosstabs(
  data    = df,
  row_var = "Commute",
  col_var = "AgeGroup"
)

# Typing `result` triggers print.crosstabs automatically:
result


# ── 3. Control row / column order to match SPSS layout ───────────────────────
result_ordered <- crosstabs(
  data      = df,
  row_var   = "Commute",
  col_var   = "AgeGroup",
  row_order = c("Bike", "Car", "Transit"),
  col_order = c("Middle", "Senior", "Young"),
  alpha     = 0.05
)
result_ordered


# ── 4. Access individual components programmatically ──────────────────────────

# Raw count matrix
result$counts

# Row-percentage matrix
result$row_pct

# Expected counts (from chi-square)
result$expected

# Overall chi-square summary
result$chi2        # chi2, df, p

# Cramér's V
result$cramers_v   # V, chi2, df, p

# Compact letter display matrix
result$letters

# Full display matrix (N<letter> (pct%))
result$display

# Bonferroni-corrected pairwise p-value table
result$p_table

# Was the low-expected-count warning triggered?
result$low_expected_warning


# ── 5. Export to Excel ────────────────────────────────────────────────────────
result_excel <- crosstabs(
  data         = df,
  row_var      = "Commute",
  col_var      = "AgeGroup",
  row_order    = c("Bike", "Car", "Transit"),
  col_order    = c("Middle", "Senior", "Young"),
  export_excel = TRUE,
  excel_path   = "commute_by_age.xlsx"
)
# File is written; open in Excel / LibreOffice Calc.


# ── 6. Low-count edge case ────────────────────────────────────────────────────
# Deliberately create sparse data to trigger safety checks.
df_sparse <- data.frame(
  Mode   = sample(c("A", "B", "C"), 30, replace = TRUE,
                  prob = c(0.70, 0.25, 0.05)),
  Group  = sample(c("G1", "G2"), 30, replace = TRUE)
)

# You may see messages like:
#   [chiPlusZ] Skipping G1-G2 | row 'C' vs cols 'G1'/'G2': observed count is zero.
result_sparse <- crosstabs(df_sparse, "Mode", "Group")
result_sparse


# ── 7. Devtools workflow reference ────────────────────────────────────────────
#
#  From the package ROOT directory (where DESCRIPTION lives):
#
#  devtools::load_all(".")          # fast in-memory load for development
#  devtools::document(".")          # regenerate NAMESPACE + man/*.Rd
#  devtools::check(".")             # full R CMD check (warnings / notes)
#  devtools::test(".")              # run testthat suite
#  devtools::install(".")           # permanent install
#
#  Useful one-liners during iteration:
#  usethis::use_package("rlang")    # add a new dependency to DESCRIPTION
#  usethis::use_test("crosstabs")   # scaffold a new test file
