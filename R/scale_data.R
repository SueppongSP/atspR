# =============================================================================
# atspR/R/scale_data.R
# Step 5 – Feature scaling (Min-Max / Z-score / Robust)
# =============================================================================

#' Scale Numeric Features
#'
#' Fits a scaler on the **training** set and applies the same parameters
#' to both train and test sets, preventing data leakage.
#'
#' @param split_result The list returned by [split_data()].
#' @param method Character.  One of `"minmax"` (default), `"zscore"`, or
#'   `"robust"`.
#' @param cols Character vector.  Names of numeric columns to scale.
#'   `NULL` (default) scales all numeric columns.
#' @param verbose Logical (default `TRUE`).
#'
#' @section Methods:
#' \describe{
#'   \item{`minmax`}{Scales each feature to \eqn{[0, 1]} using
#'     \eqn{(x - \min) / (\max - \min)}.}
#'   \item{`zscore`}{Standardises to zero mean and unit variance.}
#'   \item{`robust`}{Uses median and IQR, robust to outliers:
#'     \eqn{(x - \text{median}) / \text{IQR}}.}
#' }
#'
#' @return An invisible list with elements:
#' \describe{
#'   \item{`train_scaled`}{Scaled training `data.frame`.}
#'   \item{`test_scaled`}{Scaled test `data.frame`.}
#'   \item{`params`}{A named list of per-column scaling parameters
#'     (fitted on train only).}
#'   \item{`method`}{The scaling method used.}
#'   \item{`cols`}{Character vector of scaled column names.}
#' }
#'
#' @examples
#' data(airquality)
#' clean <- airquality[complete.cases(airquality), ]
#' sp    <- split_data(clean, verbose = FALSE)
#' sc    <- scale_data(sp, method = "minmax")
#' summary(sc$train_scaled)
#'
#' @export
scale_data <- function(split_result,
                       method  = c("minmax", "zscore", "robust"),
                       cols    = NULL,
                       verbose = TRUE) {

  method <- match.arg(method)

  train <- split_result$train
  test  <- split_result$test

  .check_df(train, "split_result$train")
  .check_df(test,  "split_result$test")

  # Determine columns to scale
  all_num <- .numeric_cols(train)
  if (is.null(cols)) cols <- all_num
  cols <- intersect(cols, all_num)          # silently ignore non-numeric cols

  if (length(cols) == 0)
    rlang::abort("No numeric columns found to scale.")

  # ── Fit scaler on TRAIN ──────────────────────────────────────────────────
  params <- .fit_scaler(train[, cols, drop = FALSE], method)

  # ── Apply to train and test ───────────────────────────────────────────────
  train_scaled <- train
  test_scaled  <- test

  train_scaled[, cols] <- .apply_scaler(train[, cols, drop = FALSE], params, method)
  test_scaled[, cols]  <- .apply_scaler(test[, cols,  drop = FALSE], params, method)

  if (verbose) {
    .header(paste0("TIME SERIES FEATURE SCALING  [", toupper(method), "]"))
    cat(sprintf("  Scaled columns : %d  (%s)\n", length(cols), paste(cols, collapse = ", ")))
    cat(sprintf("  Fitted on      : TRAIN set only (no data leakage)\n\n"))
  }

  invisible(list(
    train_scaled = train_scaled,
    test_scaled  = test_scaled,
    params       = params,
    method       = method,
    cols         = cols
  ))
}

# ── Internal: fit scaler parameters from training data ───────────────────────
.fit_scaler <- function(df, method) {
  lapply(df, function(x) {
    switch(method,
           minmax = list(min = min(x, na.rm = TRUE),
                         max = max(x, na.rm = TRUE)),
           zscore = list(mean = mean(x, na.rm = TRUE),
                         sd   = stats::sd(x,   na.rm = TRUE)),
           robust = list(median = stats::median(x, na.rm = TRUE),
                         iqr    = stats::IQR(x,    na.rm = TRUE))
    )
  })
}

# ── Internal: apply pre-fitted scaler ────────────────────────────────────────
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
