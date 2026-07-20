# chiPlusZ

**Chi-Squared Tests with SPSS-Style Column Proportion Z-Tests**

`chiPlusZ` brings SPSS's *Compare Column Proportions* procedure into R. Given a long-format data frame, it builds a contingency table, runs a Chi-squared test of independence, computes Cramér's V, and performs pairwise column-proportion Z-tests with Bonferroni correction. Results are summarised with compact letter displays (CLD) and can be exported to a formatted Excel workbook.

---

## Installation

You can install the development version of `chiPlusZ` from [GitHub](https://github.com/) with:

```r
# Install devtools if you haven't already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install chiPlusZ from GitHub
devtools::install_github("gabija-vaitkev/chiPlusZ")
```

Alternatively, you can use the more lightweight remotes package:

```r
# install.packages("remotes")
remotes::install_github("gabija-vaitkev/chiPlusZ")
```
---

## Dependencies

| Package | Purpose |
|---|---|
| `multcompView` | Compact letter displays (CLD) |
| `openxlsx` | Excel export |

Both are installed automatically with the package.

---

## Quick start

```r
library(chiPlusZ)

result <- crosstabs(
  data    = my_data,
  row_var = "Outcome",    # dependent variable
  col_var = "Group"       # independent / grouping variable
)

print(result)
```

---

## Worked example

### The data

We use a fictional dataset of 450 individuals, recording their **commute method** (row variable, dependent) and **age group** (column variable, independent).

```r
set.seed(123)
n <- 450

df <- data.frame(
  Commute = sample(
    c("Bike", "Car", "Transit"),
    n, replace = TRUE, prob = c(0.15, 0.55, 0.30)
  ),
  AgeGroup = sample(
    c("Young", "Middle", "Senior"),
    n, replace = TRUE, prob = c(0.50, 0.30, 0.20)
  )
)
```

### Input format

Data must be in **long format** - one row per observation, with each variable in its own column. The two key arguments are:

- **`row_var`** - your **dependent** variable (the outcome or response category). CLD letters compare proportions *across the columns within each row*, so the dependent variable goes here.
- **`col_var`** - your **independent** variable (the grouping variable). The column-proportion Z-tests compare groups defined by this variable.

This mirrors SPSS's layout and ensures the CLD letters are calculated in the correct direction.

### Running the analysis

```r
result <- crosstabs(
  data      = df,
  row_var   = "Commute",
  col_var   = "AgeGroup",
  row_order = c("Transit", "Bike", "Car"),
  col_order = c("Young", "Middle", "Senior"),
  alpha     = 0.05
)

print(result)
```

### Console output

```
════════════════════════════════════════════════════════════
  chiPlusZ  |  Commute (rows)  ×  AgeGroup (cols)
════════════════════════════════════════════════════════════

── Overall Chi-Squared Test
────────────────────────────────────────────────────────────


  χ²(4) = 17.350,  p = 0.0017

── Effect Size
  Cramér's V = 0.139

── Contingency Table (Counts | Expected)
                Young      Middle      Senior
Transit   75  [61.39] 38  [37.78] 12  [25.83]
Bike       27  [33.4] 19  [20.55] 22  [14.05]
Car     119  [126.22] 79  [77.67] 59  [53.11]

── Column Proportion Z-Tests (Bonferroni)
   Significance level: α = 0.050
   Cell format: Count (row%) <letter>
   Columns sharing a letter are NOT significantly different.

                Young         Middle        Senior
Transit    75 (60%) a   38 (30.4%) a   12 (9.6%) b
Bike    27 (39.71%) a 19 (27.94%) ab 22 (32.35%) b
Car     119 (46.3%) a  79 (30.74%) a 59 (22.96%) a

── Pairwise Bonferroni-Adjusted p-Values
 Category      col_pair  p_raw p_bonf sig
  Transit  Young-Middle 0.2369 0.7107    
  Transit  Young-Senior 0.0001 0.0004   *
  Transit Middle-Senior 0.0068 0.0205   *
     Bike  Young-Middle 0.6311 1.0000    
     Bike  Young-Senior 0.0108 0.0323   *
     Bike Middle-Senior 0.0605 0.1814    
      Car  Young-Middle 0.4335 1.0000    
      Car  Young-Senior 0.1172 0.3516    
      Car Middle-Senior 0.4163 1.0000    
  (* = significant after Bonferroni correction)

════════════════════════════════════════════════════════════

```

### Interpreting the output

**Chi-squared test:** The overall test asks whether commute method and age group are independent. Here χ²(4) = 17.350, p = 0.0017. Because the p-value is well below our significance threshold (α = 0.05), we reject the null hypothesis of independence. The distribution of commuting methods varies significantly across the different age groups.

**Cramér's V:** Measures the strength of the association on a 0–1 scale, regardless of table size. V = 0.139 indicates a weak effect size. General benchmarks: < 0.10 negligible, 0.10–0.20 weak, 0.20–0.40 moderate, > 0.40 strong.

**Compact letter display (CLD):** Each row is tested separately. Within a row, columns that share a letter are *not* significantly different from each other in their proportions (after Bonferroni correction). Columns with *different* letters *are* significantly different. A cell showing `ab` overlaps with both the `a` group and the `b` group - it is not significantly different from either.

> **Note on all-same letters:** When all columns in a row share the same letter, it means the Bonferroni-corrected pairwise tests found no significant difference in proportions for that category across groups. This does not contradict a significant overall Chi-squared - the omnibus test is more sensitive than the conservative Bonferroni-corrected pairwise tests.

### Exporting to Excel

```r
result <- crosstabs(
  data         = df,
  row_var      = "Commute",
  col_var      = "AgeGroup",
  row_order    = c("Transit", "Bike", "Car"),
  col_order    = c("Young", "Middle", "Senior"),
  export_excel = TRUE,
  excel_path   = "commute_by_age.xlsx"
)
```

The workbook contains two sheets:

- **Summary** - Chi-squared statistics, Cramér's V, and a crosstabulation table with observed counts, expected counts, and row percentages.
- **PostHoc** - the CLD display table and a pairwise comparison table with raw and Bonferroni-adjusted p-values. Significant pairs are highlighted.

---

## Accessing results programmatically

The function returns an S3 object of class `crosstabs`. All components are accessible directly:

```r
result$counts        # raw count matrix
result$row_pct       # row percentages (2 decimal places)
result$expected      # expected counts under H0
result$chi2          # chi2 statistic, df, p-value
result$cramers_v     # Cramér's V, chi2, df, p
result$letters       # CLD letter matrix
result$display       # formatted display matrix (count + % + letter)
result$p_table       # full pairwise comparison data frame
result$low_expected_warning  # TRUE if any expected count < 5
```

---

## Notes and caveats

**Large samples:** With very large N, even trivially small differences in proportions will produce highly significant Z-tests (p shown in scientific notation, e.g. `1.3e-47`). In such cases, focus on the effect size (Cramér's V) rather than p-values alone to judge practical importance.

**Sparse cells:** If any observed cell count is 0 or a column total is less than 5, that specific pairwise comparison is skipped and treated as non-significant. A console message identifies which comparisons were skipped. If any *expected* count is less than 5, a warning is issued and the Chi-squared approximation should be interpreted with caution - consider collapsing categories or using Fisher's exact test.

**Column names with dashes:** Group names containing hyphens (e.g. `"50-64"`, `"65-79"`) are handled by substituting the hyphens (-) with underscores (_).

**Bonferroni correction:** The number of comparisons is counted *per row*, not across the whole table. This matches SPSS's behaviour.

---

## Citation

If you use `chiPlusZ` in your research or software documentation, please kindly cite it as follows:

Vaitkevičiūtė, G. (2026). chiPlusZ: Chi-Squared + Column Proportion Z-Tests. R package version 0.1.0. URL: https://github.com/gabija-vaitkev/chiPlusZ

You can also view this directly inside R by running:

```r
citation("chiPlusZ")
```
---

## Contacts

**Email:** [info@biomatika.lt](mailto:info@biomatika.lt)

**LinkedIn:** [www.linkedin.com/in/gabija-vaitkev](https://www.linkedin.com/in/gabija-vaitkev)

---

## License

MIT © Gabija Vaitkevičiūtė
