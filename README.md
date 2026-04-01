# atspR <img src="man/figures/logo.png" align="right" height="139" alt="" />

> **Automated Time-Series Preprocessing in R**

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview

`atspR` is an R package that provides an automated, modular preprocessing
framework for time-series data. It covers the full workflow from raw sensor
data to cross-validated, model-ready datasets.

### Pipeline Steps

| Step | Function | Description |
|------|----------|-------------|
| 0 | `standardize_na()` | Convert custom missing indicators to `NA` |
| 0 | `coerce_numeric()` | Auto-convert character columns to numeric |
| 0 | `combine_datetime()` | Merge separate date + time columns → `POSIXct` |
| 0 | `fill_time_gaps()` | Insert missing timestamps in time-series |
| 1 | `missing_analysis()` | Analyse missing values & draw boxplots |
| 2 | `handle_missing()` | Drop rows or impute (linear / KNN) |
| 3 | `visualize_data()` | Scatter plots per variable vs index |
| 4 | `split_data()` | Temporal train / test split |
| 5 | `scale_data()` | Feature scaling (Min-Max / Z-score / Robust) |
| 6 | `cross_validate()` | Walk-forward k-fold cross-validation |
| — | `ts_preprocess()` | Run all steps in one call |
| — | `export_pipeline()` | Export results to CSV |

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("example/Automated-Time-Series-Preprocessing-in-R")
```

## Quick Start

### Case 1: Date + Time in separate columns

```r
library(atspR)

# Step 1: Combine date + time
df <- combine_datetime(my_data,
                       date_col  = "Date",
                       time_col  = "Time",
                       new_col   = "datetime",
                       time_type = "string")

# Step 2: Fill missing timestamps
gap <- fill_time_gaps(df, time_col = "datetime", n = 1, unit = "hour")

# Step 3: Run full pipeline
result <- ts_preprocess(
  data         = gap$data,
  train_ratio  = 0.8,
  scale_method = "minmax",
  target_col   = "VPD",
  k_folds      = 5L
)

# Results
result$train_scaled   # scaled training set
result$test_scaled    # scaled test set
result$cv_summary     # cross-validation metrics
```

### Case 2: Datetime in one column

```r
df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%d %H:%M:%S")
gap <- fill_time_gaps(df, time_col = "datetime", n = 1, unit = "hour")
result <- ts_preprocess(data = gap$data, target_col = "temp")
```

### Case 3: Date only (daily data)

```r
gap <- fill_time_gaps(df, time_col = "date", n = 1, unit = "day")
result <- ts_preprocess(data = gap$data, target_col = "sales")
```

## Cross-Validation

`atspR` uses **walk-forward validation** — the correct approach for
time-series data. Each validation fold is always preceded only by past
data, so no future information leaks into training.

```
Seed(20%)  Fold1  Fold2  Fold3  Fold4  Fold5
[─────────][─────][─────][─────][─────][─────]

Fold 1: train = seed              → val = fold1
Fold 2: train = seed + fold1      → val = fold2
Fold 3: train = seed + fold1+2    → val = fold3
...
```

## Export Results

```r
ts_export(result, dir = "C:\Users\User\", prefix = "data")
```

## Citation

```
Yourname (2024). atspR: Automated Time-Series Preprocessing in R.
R package version 0.1.0.
https://github.com/example/Automated-Time-Series-Preprocessing-in-R
```

## License

MIT © 2024
