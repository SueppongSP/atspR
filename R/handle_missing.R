# =============================================================================
# atspR/R/handle_missing.R
# Step 2 - Impute or drop missing values
# =============================================================================

#' Handle Missing Values
#'
#' Automatically chooses between **row deletion** (overall missing < 5%) and
#' **imputation** (>= 5%).  When imputing, linear interpolation is the default;
#' KNN imputation is available as an alternative.
#'
#' @param analysis_result The list returned by [missing_analysis()].
#'   Alternatively, pass a plain `data.frame` to `data`.
#' @param data A `data.frame`.  Used only when `analysis_result` is `NULL`.
#' @param method Character.  Imputation method when missing >= 5%:
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

  # -- Branch: drop vs impute ------------------------------------------------
  if (overall_pct < 0.05) {

    # -- Drop rows ------------------------------------------------------------
    rows_before <- nrow(raw_data)
    data_clean  <- raw_data[stats::complete.cases(raw_data), ]
    n_dropped   <- rows_before - nrow(data_clean)

    action      <- "drop"
    used_method <- "none"
    n_imputed   <- 0L
    report <- sprintf(
      "Action: ROW DELETION  |  Removed %d rows (overall missing %.2f%% < 5%%)",
      n_dropped, overall_pct * 100
    )

    if (verbose) {
      .header("HANDLING MISSING VALUES")
      cat(sprintf("  Overall missing = %.2f%% < 5%%\n", overall_pct * 100))
      cat(sprintf("  Strategy        : DROP rows with any NA\n"))
      cat(sprintf("  Rows removed    : %d  (%d -> %d)\n\n",
                  n_dropped, rows_before, nrow(data_clean)))
    }

  } else {

    # -- Impute ---------------------------------------------------------------
    if (verbose) {
      .header("HANDLING MISSING VALUES")
      cat(sprintf("  Overall missing = %.2f%% >= 5%%\n", overall_pct * 100))
      cat(sprintf("  Strategy        : IMPUTATION (%s)\n\n", method))
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
      "Action: IMPUTATION (%s)  |  %d cells imputed (overall missing %.2f%% >= 5%%)",
      method, n_imputed, overall_pct * 100
    )

    if (verbose) {
      cat(sprintf("  Action : IMPUTE (%s)  |  %d cells\n\n", used_method, n_imputed))
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
