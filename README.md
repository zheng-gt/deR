---
output:
  html_document: default
  pdf_document: default
---

# deR

<!-- badges: start -->

<!-- badges: end -->

deR is an R package designed to simplify data description tasks, with functions aimed at helping you quickly generate summary tables and descriptive statistics.

# Installation

``` r
# Install remotes if you haven't already 
install.packages("remotes")
# Install deR from GitHub
remotes::install_github("zheng-gt/deR")
```

# tab_up Function

The tab_up function generates a frequency table for specified variables with options to include weighted counts, remove missing values, and control the sorting order. This function is flexible for both one-way and two-way tabulations. **For one-way tables, a Total row is included, showing the overall count and percentage.**

``` r
tab_up(x, ..., wt = NULL, na.rm = FALSE, sort = TRUE)
```

# Example Usage:

``` r
# Load necessary libraries
library(dplyr)
library(deR)

# Example data
df <- tibble(
  v1 = sample(c(NA, 1:5), 20, TRUE),
  v2 = sample(1:5, 20, TRUE)
)

# One-way tabulation
df %>% tab_up(v1)

# Weighted one-way tabulation
df %>% tab_up(v1, wt = v2)

# One-way tabulation with filtering and missing values removed
df %>% filter(v2 >= 3) %>% tab_up(v1, na.rm = TRUE)

# Two-way tabulation
df %>% tab_up(v1, v2)
```
