# atspR <img src="man/figures/logo.png" align="right" height="139" alt="" />

> **Automated Time-Series Preprocessing in R**

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview


`atspR` is an R package that provides an automated, modular preprocessing pipeline for time-series data — covering the full journey from raw sensor data to cross-validated, model-ready train and test sets.

The package is designed for students and domain practitioners (agronomists, environmental scientists, irrigation engineers) who work primarily in R and need a reliable, leakage-free preprocessing workflow without having to manage multiple packages or worry about the correct sequencing of steps.

### Why atspR?

| Problem | How atspR solves it |
|---|---|
| Scalers applied before splitting → **data leakage** | Scaler fitted on **TRAIN only**, applied to both sets |
| Random split invalidates time-series models | **Temporal split** — train always precedes test chronologically |
| Silent NA removal with no record | Full report of what was dropped or imputed, and why |
| Multi-package workflow, easy to sequence incorrectly | Single function `ts_preprocess()` runs all 7 steps in order |
| Error messages with no guidance | Every automated decision explained in plain-language output |

---


## Pipeline

```
Raw data
   │
   ├─ combine_datetime()            Merge date + time columns → POSIXct
   ├─ fill_time_gaps()              Insert placeholder rows for missing timestamps
   │
   └─ ts_preprocess()               ────────────────── 7 steps ──────────────────
   │    [1/7] standardize_na()      Convert sentinel values to NA
   │    [2/7] coerce_numeric()      Parse character columns to numeric
   │    [3/7] missing_analysis()    Summarise NAs; decide DROP or IMPUTE
   │    [4/7] handle_missing()      Drop rows or interpolate (linear/KNN)
   │    [5/7] visualize_data()      Scatter plots per variable vs time
   │    [6/7] split_data()          Temporal train/test split
   │    [7/7] scale_data()          MinMax / Z-score / Robust (auto)
   │        │
   │        └─ cross_validate()     Walk-forward k-fold CV (optional)
   │
   └─ ts_export()                   Write all outputs to CSV
```

---

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("SueppongSP/atspR")
```

## Quick Start

### Case 1: Date + Time in separate columns

```r
library(atspR)

df <- combine_datetime(my_data,
                       date_col  = "Date",
                       time_col  = "Time",
                       new_col   = "datetime",
                       time_type = "string")

gap <- fill_time_gaps(df, 
                      time_col = "datetime", 
                      n        = 1, 
                      unit     = "hour")

result <- ts_preprocess(data         = gap$data,
                        train_ratio  = 0.8,
                        scale_method = "minmax",
                        target_col   = "data_target",
                        k_folds      = 5)
```

### Case 2: Datetime in one column

```r
library(atspR)

df$datetime <- as.POSIXct(df$datetime, 
                          format = "%Y-%m-%d %H:%M:%S")
                          
gap <- fill_time_gaps(df, 
                      time_col = "datetime", 
                      n = 1, 
                      unit = "hour")
                      
result <- ts_preprocess(data         = gap$data,
                        train_ratio  = 0.8,
                        scale_method = "minmax",
                        target_col   = "data_target",
                        k_folds      = 5)
```

### Case 3: Date only (daily data)

```r
library(atspR)

gap <- fill_time_gaps(df, 
                      time_col = "date", 
                      n = 1, 
                      unit = "day")
                      
result <- ts_preprocess(data         = gap$data,
                        train_ratio  = 0.8,
                        scale_method = "minmax",
                        target_col   = "data_target",
                        k_folds      = 5)
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
ts_export(result, dir = "C:\Users\user\", prefix = "data")
```

## Citation

```
Yourname (2024). atspR: Automated Time-Series Preprocessing in R.
R package version 0.1.0.
https://github.com/example/Automated-Time-Series-Preprocessing-in-R
```

## License

MIT © 2024
