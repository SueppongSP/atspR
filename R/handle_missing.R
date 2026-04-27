# =============================================================================
# atspR/R/handle_missing.R
# Step 2 - Impute or drop missing values
# =============================================================================

#' Handle Missing Values
#'
#' Automatically chooses between **imputation** and **row deletion** based on
#' two criteria: dataset size (`n`) and overall missing rate.
#'
#' Decision rules:
#' \itemize{
#'   \item `n <= 50`                      -> always impute (data too small to lose rows)
#'   \item `n > 50` and `missing > 5%`   -> impute
#'   \item `n > 50` and `missing <= 5%`  -> drop rows with NA
#' }
#'
#' When imputing, linear interpolation is the default;
#' KNN imputation is available as an alternative.
#'
#' @param analysis_result The list returned by [atspR::missing_analysis()].
#'   Alternatively, pass a plain `data.frame` to `data`.
#' @param data A `data.frame`.  Used only when `analysis_result` is `NULL`.
#' @param method Character.  Imputation method:
#'   `"linear"` (default) or `"knn"`.
#' @param knn_k Integer.  Number of neighbours for KNN (default 5).
#' @param verbose Logical.  Print progress messages (default `TRUE`).
#'
#' @return An invisible list with elements:
#' \describe{
#'   \item{`data_clean`}{The cleaned `data.frame`.}
#'   \item{`action`}{Character: `"drop"` or `"impute"`.}
#'   \item{`method`}{Imputation method used (or `"none"`).}
#'   \item{`imputed_mask`}{Logical `data.frame` marking imputed cells
#'     (same dimensions as `data_clean`).  All `FALSE` when action is `"drop"`.}
#'   \item{`n_imputed`}{Integer.  Number of cells that were imputed.}
#'   \item{`report`}{A short character string summarising the action taken.}
#' }
#'
#' @examples
#' data(airquality)
#' ana  <- missing_analysis(airquality, plot = FALSE)
#' clean <- handle_missing(ana)
#' clean$report
#'
#' @export
handle_missing <- function(analysis_result = NULL,
                           data            = NULL,
                           method          = c("linear", "knn"),
                           knn_k           = 5L,
                           verbose         = TRUE) {

  method <- match.arg(method)

  # -- Resolve inputs --------------------------------------------------------
  if (!is.null(analysis_result)) {
    .check_df(analysis_result$data, "analysis_result$data")
    raw_data    <- analysis_result$data
    overall_pct <- analysis_result$overall_pct
  } else if (!is.null(data)) {
    .check_df(data)
    raw_data    <- data
    overall_pct <- sum(is.na(data)) / (nrow(data) * ncol(data))
  } else {
    rlang::abort("Supply either `analysis_result` (from missing_analysis()) or `data`.")
  }

  imputed_mask <- as.data.frame(
    matrix(FALSE, nrow = nrow(raw_data), ncol = ncol(raw_data),
           dimnames = list(NULL, names(raw_data)))
  )

  n_rows <- nrow(raw_data)

  # -- Decision rule ---------------------------------------------------------
  # n <= 50                              -> always impute (too small to lose rows)
  # n > 50  and missing > 5%            -> impute
  # 50 < n < 1000 and missing <= 5%     -> drop
  # n >= 1000 and missing <= 5%         -> drop
  do_impute <- (n_rows <= 50) || (overall_pct > 0.05)

  # -- Reason string for verbose output --------------------------------------
  decision_reason <- if (n_rows <= 50) {
    sprintf("n = %d <= 50 -> always impute regardless of missing rate", n_rows)
  } else if (overall_pct > 0.05) {
    sprintf("n = %d > 50 and missing = %.2f%% > 5%% -> impute",
            n_rows, overall_pct * 100)
  } else if (n_rows < 1000) {
    sprintf("50 < n = %d < 1000 and missing = %.2f%% <= 5%% -> drop rows",
            n_rows, overall_pct * 100)
  } else {
    sprintf("n = %d >= 1000 and missing = %.2f%% <= 5%% -> drop rows",
            n_rows, overall_pct * 100)
  }

  # -- Branch: impute --------------------------------------------------------
  if (do_impute) {

    if (verbose) {
      .header("STEP 4/7 : Handle Missing Values  [IMPUTE]")
      cat(sprintf("  Method  : %s  |  Missing: %.2f%%\n", toupper(method), overall_pct * 100))
      cat(sprintf("  Reason  : %s\n\n", decision_reason))
    }

    na_before  <- is.na(raw_data)
    data_clean <- raw_data

    if (method == "linear") {
      data_clean <- .impute_linear(data_clean, verbose)
    } else {
      data_clean <- .impute_knn(data_clean, knn_k, verbose)
    }

    imputed_mask <- as.data.frame(na_before & !is.na(data_clean))
    n_imputed    <- sum(as.matrix(imputed_mask))
    action       <- "impute"
    used_method  <- method
    report <- sprintf(
      "Action: IMPUTATION (%s)  |  %d cells imputed  |  %s",
      method, n_imputed, decision_reason
    )

    if (verbose) {
      .subheader("Imputation result")
      cat(sprintf("  Cells filled : %d\n", n_imputed))
      imp_cols <- colSums(as.matrix(imputed_mask))
      imp_cols <- imp_cols[imp_cols > 0]
      if (length(imp_cols) > 0) {
        imp_df <- data.frame(
          column  = names(imp_cols),
          imputed = as.integer(imp_cols),
          stringsAsFactors = FALSE
        )
        print(imp_df[order(-imp_df$imputed), ], row.names = FALSE)
      }
      cat("\n")
    }

    # -- Branch: drop ----------------------------------------------------------
  } else {

    rows_before <- n_rows
    data_clean  <- raw_data[stats::complete.cases(raw_data), ]
    n_dropped   <- rows_before - nrow(data_clean)

    action      <- "drop"
    used_method <- "none"
    n_imputed   <- 0L
    report <- sprintf(
      "Action: ROW DELETION  |  Removed %d rows  |  %s",
      n_dropped, decision_reason
    )

    if (verbose) {
      .header("STEP 4/7 : Handle Missing Values  [DROP]")
      cat(sprintf("  Missing : %.2f%%  |  Reason: %s\n\n", overall_pct * 100, decision_reason))
      cat(sprintf("  Rows before : %d  ->  after: %d  (-%d removed)\n\n",
                  rows_before, nrow(data_clean), n_dropped))
    }
  }

  invisible(list(
    data_clean   = data_clean,
    action       = action,
    method       = used_method,
    imputed_mask = imputed_mask,
    n_imputed    = n_imputed,
    report       = report
  ))
}

# -- Internal: linear interpolation ------------------------------------------
.impute_linear <- function(data, verbose = TRUE) {
  num_cols <- .numeric_cols(data)
  for (col in names(data)) {
    if (any(is.na(data[[col]]))) {
      if (col %in% num_cols) {
        data[[col]] <- zoo::na.approx(data[[col]], na.rm = FALSE)
        # Fill leading / trailing NAs with nearest valid value
        data[[col]] <- zoo::na.locf(data[[col]], na.rm = FALSE)
        data[[col]] <- zoo::na.locf(data[[col]], fromLast = TRUE, na.rm = FALSE)
      } else {
        # Categorical: forward-fill
        data[[col]] <- zoo::na.locf(data[[col]], na.rm = FALSE)
        data[[col]] <- zoo::na.locf(data[[col]], fromLast = TRUE, na.rm = FALSE)
      }
      if (verbose) cat(sprintf("    [linear] imputed column: %s\n", col))
    }
  }
  data
}

# -- Internal: KNN imputation ------------------------------------------------
.impute_knn <- function(data, k = 5, verbose = TRUE) {
  if (!requireNamespace("VIM", quietly = TRUE)) {
    rlang::abort(
      "Package 'VIM' is required for KNN imputation. Install it with: install.packages('VIM')"
    )
  }
  if (verbose) cat(sprintf("    [KNN] k = %d\n", k))
  imputed <- VIM::kNN(data, k = k, imp_var = FALSE)
  as.data.frame(imputed)
}
