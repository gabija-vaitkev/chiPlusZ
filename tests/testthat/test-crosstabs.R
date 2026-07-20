# Helper: a clean, reproducible long-format data frame with SIGNIFICANT differences
# Ensures that column order is ("Young", "Middle", "Senior") and row order is ("Transit", "Bike", "Car")
make_df <- function(seed = 123, n = 350) {
  set.seed(seed)
  
  # Sample age groups cleanly using requested factor ordering
  age_group <- sample(c("Young", "Middle", "Senior"), n, replace = TRUE, prob = c(0.40, 0.40, 0.20))
  
  # Generate dependent choices to ensure highly significant chi-squared and z-tests.
  # Keeps minimum probabilities high enough to completely eliminate 0-count cells.
  commute <- sapply(age_group, function(age) {
    if (age == "Young") {
      sample(c("Transit", "Bike", "Car"), 1, prob = c(0.45, 0.40, 0.15))
    } else if (age == "Middle") {
      sample(c("Transit", "Bike", "Car"), 1, prob = c(0.20, 0.15, 0.65))
    } else { # Senior
      sample(c("Transit", "Bike", "Car"), 1, prob = c(0.35, 0.15, 0.50))
    }
  })
  
  data.frame(
    Commute  = factor(commute, levels = c("Transit", "Bike", "Car")),
    AgeGroup = factor(age_group, levels = c("Young", "Middle", "Senior"))
  )
}

# Helper: data frame whose column names contain dashes
make_dash_df <- function(seed = 123, n = 450) {
  set.seed(seed)
  data.frame(
    Status   = sample(c("yes", "no"), n, replace = TRUE),
    AgeGroup = sample(c("<50", "50-64", "65-79", "80+"), n, replace = TRUE)
  )
}

# ── S3 class & structure ──────────────────────────────────────────────────────

test_that("crosstabs() returns an object of class 'crosstabs'", {
  res <- crosstabs(make_df(), "Commute", "AgeGroup")
  expect_s3_class(res, "crosstabs")
})

test_that("result contains all expected list elements", {
  res <- crosstabs(make_df(), "Commute", "AgeGroup")
  expected_names <- c("counts", "row_pct", "expected", "chi2", "cramers_v",
                      "letters", "display", "p_table",
                      "low_expected_warning", "row_var", "col_var", "alpha")
  expect_true(all(expected_names %in% names(res)))
})

# ── Dimensions ───────────────────────────────────────────────────────────────

test_that("counts matrix dimensions match variable levels", {
  res <- crosstabs(make_df(), "Commute", "AgeGroup")
  expect_equal(nrow(res$counts), 3)
  expect_equal(ncol(res$counts), 3)
})

test_that("row_order and col_order reorder the table correctly", {
  res <- crosstabs(make_df(), "Commute", "AgeGroup",
                   row_order = c("Car", "Bike", "Transit"),
                   col_order = c("Senior", "Young", "Middle"))
  expect_equal(rownames(res$counts)[1], "Car")
  expect_equal(colnames(res$counts)[1], "Senior")
})

# ── Percentages ───────────────────────────────────────────────────────────────

test_that("row percentages sum to 100 per row (within rounding tolerance)", {
  res <- crosstabs(make_df(), "Commute", "AgeGroup")
  expect_true(all(abs(rowSums(res$row_pct) - 100) < 0.1))
})

test_that("row percentages are rounded to 2 decimal places", {
  res <- crosstabs(make_df(), "Commute", "AgeGroup")
  expect_equal(res$row_pct, round(res$row_pct, 2))
})

# ── p_table column names & Significance ───────────────────────────────────────

test_that("p_table has correct column names after renaming", {
  res <- crosstabs(make_df(), "Commute", "AgeGroup")
  expect_true("row_cat"  %in% names(res$p_table))
  expect_true("col_pair" %in% names(res$p_table))
  expect_true("col_i"    %in% names(res$p_table))
  expect_true("col_j"    %in% names(res$p_table))
  expect_true("p_raw"    %in% names(res$p_table))
  expect_true("p_bonf"   %in% names(res$p_table))
})

test_that("p_table has correct number of pairwise rows (3 cols → 3 pairs per row)", {
  res <- crosstabs(make_df(), "Commute", "AgeGroup")
  expect_equal(nrow(res$p_table), 9)
})

test_that("Bonferroni p-values are always >= raw p-values", {
  res <- crosstabs(make_df(), "Commute", "AgeGroup")
  p   <- res$p_table[!is.na(res$p_table$p_raw), ]
  expect_true(all(p$p_bonf >= p$p_raw))
})

test_that("Bonferroni p-values are capped at 1", {
  res <- crosstabs(make_df(), "Commute", "AgeGroup")
  p   <- res$p_table[!is.na(res$p_table$p_bonf), ]
  expect_true(all(p$p_bonf <= 1))
})

test_that("crosstabs yields statistically significant results for make_df", {
  res <- crosstabs(make_df(), "Commute", "AgeGroup")
  # Fixed syntax: chi2 is a named numeric vector; the p-value element key is "p"
  expect_lt(as.numeric(res$chi2["p"]), 0.05)
})

# ── Dash-in-column-name fix ───────────────────────────────────────────────────

test_that("column names containing dashes do not crash multcompLetters", {
  expect_no_error(
    crosstabs(make_dash_df(), "Status", "AgeGroup",
              col_order = c("<50", "50-64", "65-79", "80+"))
  )
})

test_that("CLD letters are returned for all columns even with dashes in names", {
  res <- crosstabs(make_dash_df(), "Status", "AgeGroup",
                   col_order = c("<50", "50-64", "65-79", "80+"))
  expect_equal(rownames(res$letters), rownames(res$counts))
  expect_equal(colnames(res$letters), colnames(res$counts))
  expect_true(all(nchar(res$letters) > 0))
})

# ── Input validation ──────────────────────────────────────────────────────────

test_that("non-existent row_var raises an informative error", {
  expect_error(crosstabs(make_df(), "BADCOL", "AgeGroup"), "not found")
})

test_that("non-existent col_var raises an informative error", {
  expect_error(crosstabs(make_df(), "Commute", "BADCOL"), "not found")
})

test_that("non-data-frame input raises an error", {
  expect_error(crosstabs(list(a = 1), "a", "b"), "data frame")
})

test_that("unknown levels in row_order produce a warning, not an error", {
  expect_warning(
    crosstabs(make_df(), "Commute", "AgeGroup",
              row_order = c("Transit", "Bike", "Car", "Unicycle")),
    "not in data"
  )
})

# ── Low expected counts warning ───────────────────────────────────────────────

test_that("low expected counts trigger a warning and set the flag", {
  set.seed(1)
  df_sparse <- data.frame(
    Mode  = sample(c("A", "B", "C"), 20, replace = TRUE, prob = c(0.80, 0.15, 0.05)),
    Group = sample(c("G1", "G2"), 20, replace = TRUE)
  )
  res <- suppressWarnings(crosstabs(df_sparse, "Mode", "Group"))
  expect_true(res$low_expected_warning)
})

# ── Cramér's V ───────────────────────────────────────────────────────────────

test_that("Cramer's V is between 0 and 1", {
  res <- crosstabs(make_df(), "Commute", "AgeGroup")
  expect_gte(res$cramers_v["V"], 0)
  expect_lte(res$cramers_v["V"], 1)
})

# ── print method ─────────────────────────────────────────────────────────────

test_that("print.crosstabs runs without error and returns the object invisibly", {
  res <- crosstabs(make_df(), "Commute", "AgeGroup")
  expect_invisible(print(res))
})