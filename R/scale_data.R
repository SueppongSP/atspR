# =============================================================================
# atspR/R/scale_data.R
# Step 5 - Feature scaling with auto-selection based on outlier detection
# =============================================================================

# -- Internal: detect outlier ratio per column using IQR method ----------------
.outlier_ratio <- function(x) {
  x   <- x[!is.na(x)]
  if (length(x) < 4) return(0)
  q1  <- stats::quantile(x, 0.25)
  q3  <- stats::quantile(x, 0.75)
  iqr <- q3 - q1
  if (iqr == 0) return(0)
  sum(x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)) / length(x)
}

# -- Internal: normality test (Shapiro-Wilk, sample <= 5000) ------------------
.is_normal <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3)   return(TRUE)
  if (n > 5000) x <- sample(x, 5000)
  tryCatch(
    stats::shapiro.test(x)$p.value > 0.05,
    error = function(e) FALSE
  )
}

# -- Internal: auto-select scaling method -------------------------------------
.auto_select_method <- function(df, outlier_threshold = 0.05) {
  ratios   <- vapply(df, .outlier_ratio, numeric(1))
  avg_ratio <- mean(ratios)

  if (avg_ratio >= outlier_threshold) {
    list(method = "robust",
         reason = sprintf("outliers detected (avg %.1f%% per column) -> robust",
                          avg_ratio * 100))
  } else {
    normals <- vapply(df, .is_normal, logical(1))
    if (mean(normals) >= 0.5) {
      list(method = "zscore",
           reason = "no outliers + data approximately normal -> zscore")
    } else {
      list(method = "minmax",
           reason = "no outliers + data not normal -> minmax")
    }
  }
}

#' Scale Numeric Features (Auto-Select Method)
#'
#' Fits a scaler on the **training** set and applies the same parameters
#' to both train and test sets, preventing data leakage.
#'
#' When `method = "auto"` (default), the scaling method is chosen
#' automatically based on outlier detection (IQR method) and a normality
#' test (Shapiro-Wilk):
#' \itemize{
#'   \item **robust**  -- outliers detected in >= 5% of values per column
#'   \item **zscore**  -- no outliers + data approximately normal
#'   \item **minmax**  -- no outliers + data not normal
#' }
#'
#' @param split_result The list returned by [split_data()].
#' @param method Character. One of `"auto"` (default), `"minmax"`,
#'   `"zscore"`, or `"robust"`. Use `"auto"` to let the function choose.
#' @param outlier_threshold Numeric in (0, 1). Fraction of outliers per
#'   column above which `"robust"` is selected. Default `0.05` (5%).
#' @param cols Character vector. Names of numeric columns to scale.
#'   `NULL` (default) scales all numeric columns.
#' @param verbose Logical (default `TRUE`).
#'
#' @section Methods:
#' \describe{
#'   \item{`minmax`}{Scales each feature to \eqn{[0, 1]}.}
#'   \item{`zscore`}{Standardises to zero mean and unit variance.}
#'   \item{`robust`}{Uses median and IQR, robust to outliers.}
#' }
#'
#' @return An invisible list with elements:
#' \describe{
#'   \item{`train_scaled`}{Scaled training `data.frame`.}
#'   \item{`test_scaled`}{Scaled test `data.frame`.}
#'   \item{`params`}{Per-column scaling parameters (fitted on train only).}
#'   \item{`method`}{The scaling method used.}
#'   \item{`method_reason`}{Why the method was selected (auto mode only).}
#'   \item{`outlier_ratio`}{Named numeric vector of outlier ratios per column.}
#'   \item{`cols`}{Character vector of scaled column names.}
#' }
#'
#' @examples
#' data(airquality)
#' clean <- airquality[complete.cases(airquality), ]
#' sp    <- split_data(clean, verbose = FALSE)
#' sc    <- scale_data(sp)
#' sc$method
#' sc$method_reason
#'
#' @export
scale_data <- function(split_result,
                       method             = "auto",
                       outlier_threshold  = 0.05,
                       cols               = NULL,
                       verbose            = TRUE) {

  train <- split_result$train
  test  <- split_result$test

  .check_df(train, "split_result$train")
  .check_df(test,  "split_result$test")

  # -- Determine columns to scale --------------------------------------------
  all_num <- .numeric_cols(train)
  if (is.null(cols)) cols <- all_num
  cols <- intersect(cols, all_num)

  if (length(cols) == 0)
    rlang::abort("No numeric columns found to scale.")

  train_num <- train[, cols, drop = FALSE]

  # -- Auto-select method ----------------------------------------------------
  method_reason <- NULL
  outlier_ratio <- vapply(train_num, .outlier_ratio, numeric(1))

  if (method == "auto") {
    auto          <- .auto_select_method(train_num, outlier_threshold)
    method        <- auto$method
    method_reason <- auto$reason
  }

  # -- Fit scaler on TRAIN ---------------------------------------------------
  params <- .fit_scaler(train_num, method)

  # -- Apply to train and test -----------------------------------------------
  train_scaled <- train
  test_scaled  <- test

  train_scaled[, cols] <- .apply_scaler(train_num, params, method)
  test_scaled[, cols]  <- .apply_scaler(test[, cols, drop = FALSE], params, method)

  # -- Console output --------------------------------------------------------
  if (verbose) {
    has_outlier <- outlier_ratio[outlier_ratio > 0]
    avg_outlier <- mean(outlier_ratio) * 100

    .header(paste0("STEP 7/7 : Feature Scaling  [", toupper(method), "]"))
    if (!is.null(method_reason))
      cat(sprintf("  Auto-selected : %s\n", method_reason))
    cat(sprintf("  Columns : %d  (%s)\n",
                length(cols),
                if (length(cols) <= 4) paste(cols, collapse = ", ")
                else paste(c(cols[1:3], "..."), collapse = ", ")))
    cat(sprintf("  Fitted on TRAIN only  |  Avg outlier: %.1f%%\n\n", avg_outlier))

    if (length(has_outlier) > 0) {
      out_df <- data.frame(
        column      = names(outlier_ratio),
        outlier_pct = outlier_ratio * 100,
        stringsAsFactors = FALSE
      )
      out_df <- out_df[order(-out_df$outlier_pct), ]
      out_df$outlier_pct <- paste0(sprintf("%.2f", out_df$outlier_pct), "%")
      show_df <- out_df[out_df$outlier_pct != "0.00%", , drop = FALSE]
      .subheader("Outlier ratio per column  (IQR)")
      print(show_df, row.names = FALSE)
      high_out_cols <- names(outlier_ratio[outlier_ratio >= 0.10])
      if (length(high_out_cols) > 0 && method != "robust")
        cat(sprintf("\n  [!] %s >= 10%% outliers -- consider scale_method = \"robust\"\n",
                    paste(high_out_cols, collapse = ", ")))
      cat("\n")
    } else {
      cat("  [OK] No outliers detected\n\n")
    }
  }

  invisible(list(
    train_scaled  = train_scaled,
    test_scaled   = test_scaled,
    params        = params,
    method        = method,
    method_reason = method_reason,
    outlier_ratio = outlier_ratio,
    cols          = cols
  ))
}

# -- Internal: fit scaler parameters ------------------------------------------
.fit_scaler <- function(df, method) {
  lapply(df, function(x) {
    switch(method,
           minmax = list(min    = min(x,              na.rm = TRUE),
                         max    = max(x,              na.rm = TRUE)),
           zscore = list(mean   = mean(x,             na.rm = TRUE),
                         sd     = stats::sd(x,        na.rm = TRUE)),
           robust = list(median = stats::median(x,    na.rm = TRUE),
                         iqr    = stats::IQR(x,       na.rm = TRUE))
    )
  })
}

# -- Internal: apply pre-fitted scaler ----------------------------------------
.apply_scaler <- function(df, params, method) {
  for (col in names(df)) {
    p <- params[[col]]
    df[[col]] <- switch(method,
                        minmax = {
                          rng <- p$max - p$min
                          if (rng == 0) rep(0, length(df[[col]])) else (df[[col]] - p$min) / rng
                        },
                        zscore = {
                          if (p$sd == 0) rep(0, length(df[[col]])) else (df[[col]] - p$mean) / p$sd
                        },
                        robust = {
                          if (p$iqr == 0) rep(0, length(df[[col]])) else (df[[col]] - p$median) / p$iqr
                        }
    )
  }
  df
}

#' Inverse-Transform Scaled Data
#'
#' Reverses the scaling applied by [scale_data()] to recover original units.
#'
#' @param scaled_data A `data.frame` of scaled values.
#' @param scale_result The list returned by [scale_data()].
#'
#' @return A `data.frame` with the scaled columns back in their original units.
#'
#' @examples
#' data(airquality)
#' clean <- airquality[complete.cases(airquality), ]
#' sp    <- split_data(clean, verbose = FALSE)
#' sc    <- scale_data(sp, verbose = FALSE)
#' original_train <- inverse_scale(sc$train_scaled, sc)
#'
#' @export
inverse_scale <- function(scaled_data, scale_result) {
  .check_df(scaled_data)

  params <- scale_result$params
  method <- scale_result$method
  cols   <- scale_result$cols
  out    <- scaled_data

  for (col in cols) {
    p <- params[[col]]
    out[[col]] <- switch(method,
                         minmax = out[[col]] * (p$max - p$min) + p$min,
                         zscore = out[[col]] * p$sd + p$mean,
                         robust = out[[col]] * p$iqr + p$median
    )
  }
  out
}
