# =============================================================================
# atspR/R/ts_preprocess.R
# Master orchestrator
# =============================================================================

#' Run the Full Preprocessing Pipeline
#'
#' Executes all pipeline steps in order:
#' \enumerate{
#'   \item [standardize_na()]  - convert custom missing indicators to NA
#'   \item [missing_analysis()] - assess missing values & draw boxplots
#'   \item [handle_missing()]   - drop rows or impute
#'   \item [visualize_data()]   - scatter plots per variable vs index
#'   \item [split_data()]       - temporal train/test split
#'   \item [scale_data()]       - feature scaling (no leakage)
#'   \item [cross_validate()]   - k-fold CV on training set (optional)
#' }
#'
#' @param data A `data.frame` with rows ordered by time.
#' @param na_strings Character vector. Extra strings to treat as `NA`,
#'   e.g. `c("-", "Missing", "N/A")`. Built-in defaults already cover
#'   common patterns. Default `character(0)`.
#' @param na_numbers Numeric vector. Numeric sentinel values to treat as `NA`,
#'   e.g. `c(-999, 9999)`. Default `NULL`.
#' @param min_success_rate Numeric in (0, 1]. Minimum fraction of non-NA values
#'   that must parse as numbers for a character column to be converted to numeric.
#'   Default `0.8`.
#' @param train_ratio Numeric in (0, 1). Default `0.8`.
#' @param impute_method `"linear"` (default) or `"knn"`.
#' @param scale_method `"auto"` (default) auto-selects based on outlier detection,
#'   or one of `"minmax"`, `"zscore"`, `"robust"`.
#' @param target_col Character. Response column for cross-validation.
#'   If `NULL` (default), CV is skipped.
#' @param k_folds Integer. Number of CV folds. Default `5`.
#' @param model_fn Optional custom model function for CV.
#' @param verbose Logical (default `TRUE`).
#'
#' @return An invisible list with elements:
#' \describe{
#'   \item{`data_clean`}{Cleaned data.frame.}
#'   \item{`train`}{Unscaled training set.}
#'   \item{`test`}{Unscaled test set.}
#'   \item{`train_scaled`}{Scaled training set -- ready for modelling.}
#'   \item{`test_scaled`}{Scaled test set.}
#'   \item{`scale_params`}{Scaling parameters (fitted on train only).}
#'   \item{`scale_method`}{Scaling method used.}
#'   \item{`missing_report`}{Per-variable NA summary table.}
#'   \item{`imputation_report`}{String describing the action taken.}
#'   \item{`cv_summary`}{CV summary data.frame, or NULL if skipped.}
#'   \item{`cv_folds`}{Per-fold CV results, or NULL if skipped.}
#'   \item{`plots`}{Named list: boxplot, scatter.}
#'   \item{`before_after`}{Before/after imputation sample, or NULL.}
#' }
#'
#' @examples
#' data(airquality)
#'
#' # Basic usage
#' result <- ts_preprocess(
#'   data         = airquality,
#'   train_ratio  = 0.8,
#'   scale_method = "minmax"
#' )
#' head(result$train_scaled)
#'
#' # With custom NA indicators
#' result <- ts_preprocess(
#'   data       = airquality,
#'   na_strings = c("-", "Missing"),
#'   na_numbers = c(-999),
#'   scale_method = "minmax"
#' )
#'
#' @export
ts_preprocess <- function(data,
                          na_strings    = character(0),
                          na_numbers    = NULL,
                          min_success_rate = 0.8,
                          train_ratio   = 0.8,
                          impute_method = "linear",
                          scale_method  = "minmax",
                          target_col    = NULL,
                          k_folds       = 5L,
                          model_fn      = NULL,
                          verbose       = TRUE) {

  .check_df(data)

  if (verbose) {
    .header("atspR  |  Automated Time Series Preprocessing")
    cat(sprintf("  Input : %d rows, %d cols  |  Train/Test: %.0f%%/%.0f%%  |  Impute: %s  |  Scale: %s\n",
                nrow(data), ncol(data),
                train_ratio * 100, (1 - train_ratio) * 100,
                impute_method, scale_method))
    cat(sprintf("  CV    : target = %s,  k = %d\n",
                if (!is.null(target_col)) target_col else "none (skipped)", k_folds))
    cat(strrep("-", 60), "\n", sep = "")
  }

  # Step 1: Standardise missing indicators
  data <- standardize_na(data,
                         na_strings = na_strings,
                         na_numbers = na_numbers,
                         verbose    = FALSE)


  # Step 2: Coerce character columns to numeric
  data <- coerce_numeric(data,
                         min_success_rate = min_success_rate,
                         verbose          = FALSE)


  # Step 3: Missing analysis
  ana <- missing_analysis(data, plot = TRUE, verbose = FALSE)

  # Step 4: Handle missing
  clean <- handle_missing(ana, method = impute_method, verbose = FALSE)

  # Step 5: Visualise
  viz <- visualize_data(clean, raw_data = data, verbose = FALSE)

  # Step 6: Split
  sp <- split_data(clean$data_clean, train_ratio = train_ratio, verbose = FALSE)

  # Step 7: Scale
  sc <- scale_data(sp, method = scale_method, verbose = FALSE)

  # -- Print unified pipeline summary ----------------------------------------
  if (verbose) {
    miss_cols  <- sum(ana$summary$n_missing > 0)
    n_orig     <- nrow(ana$data)
    n_clean    <- nrow(clean$data_clean)

    # Step 1: Missing Analysis
    miss_reason <- if (n_orig <= 50) {
      sprintf("n = %d <= 50, always impute", n_orig)
    } else if (ana$overall_pct > 0.05) {
      sprintf("missing = %.2f%% > 5%% threshold", ana$overall_pct * 100)
    } else {
      sprintf("missing = %.2f%% <= 5%% threshold, safe to drop", ana$overall_pct * 100)
    }
    miss_action <- if (clean$action == "drop") "DROP" else paste0("IMPUTE (", toupper(clean$method), ")")

    cat(sprintf("  [1/7] Standardise NA indicators  -- done\n\n"))

    cat(sprintf("  [2/7] Coerce character cols to numeric  -- done\n\n"))

    cat(sprintf("  [3/7] Missing Value Analysis\n"))
    cat(sprintf("        %.2f%%  |  %d / %d cols  |  %s  -- %s\n\n",
                ana$overall_pct * 100, miss_cols, ncol(data), miss_action, miss_reason))

    cat(sprintf("  [4/7] Handle Missing Values\n"))
    if (clean$action == "drop") {
      cat(sprintf("        %d -> %d rows  (-%d dropped)\n\n",
                  n_orig, n_clean, n_orig - n_clean))
    } else {
      cat(sprintf("        %d cells imputed", clean$n_imputed))
      imp_cols <- colSums(as.matrix(clean$imputed_mask))
      imp_cols <- sort(imp_cols[imp_cols > 0], decreasing = TRUE)
      if (length(imp_cols) > 0)
        cat(sprintf("  [%s]",
                    paste(sprintf("%s:%d", names(imp_cols), imp_cols), collapse = "  ")))
      cat("\n\n")
    }

    cat(sprintf("  [5/7] Visualise\n"))
    cat(sprintf("        %d scatter page(s) generated\n\n", length(viz$scatter_plots)))

    cat(sprintf("  [6/7] Train / Test Split\n"))
    cat(sprintf("        Train: %d rows (%.0f%%)  |  Test: %d rows (%.0f%%)\n\n",
                nrow(sp$train), nrow(sp$train) / n_clean * 100,
                nrow(sp$test),  nrow(sp$test)  / n_clean * 100))

    avg_out  <- mean(sc$outlier_ratio) * 100
    out_cols <- sc$outlier_ratio[sc$outlier_ratio > 0]
    cat(sprintf("  [7/7] Feature Scaling  [%s]\n", toupper(sc$method)))
    if (!is.null(sc$method_reason))
      cat(sprintf("        %s\n", sc$method_reason))
    if (length(out_cols) > 0) {
      out_sorted <- sort(out_cols, decreasing = TRUE)
      cat(sprintf("        Outliers: %s\n",
                  paste(sprintf("%s(%.1f%%)", names(out_sorted), out_sorted * 100),
                        collapse = "  ")))
      high_out <- out_sorted[out_sorted >= 0.10]
      if (length(high_out) > 0 && toupper(sc$method) != "ROBUST")
        cat(sprintf("        [!] %s >= 10%% -- consider scale_method = \"robust\"\n",
                    paste(names(high_out), collapse = ", ")))
    }
    cat("\n")
    cat(strrep("-", 60), "\n", sep = "")
  }

  # Step 8: Cross-validate (optional)
  cv_summary <- NULL
  cv_folds   <- NULL

  if (!is.null(target_col)) {
    cv         <- cross_validate(sc,
                                 target_col = target_col,
                                 model_fn   = model_fn,
                                 k          = k_folds,
                                 verbose    = verbose)
    cv_summary <- cv$summary
    cv_folds   <- cv$fold_results
  }



  invisible(list(
    data_clean        = clean$data_clean,
    train             = sp$train,
    test              = sp$test,
    train_scaled      = sc$train_scaled,
    test_scaled       = sc$test_scaled,
    scale_params      = sc$params,
    scale_method      = sc$method,
    missing_report    = ana$summary,
    imputation_report = clean$report,
    cv_summary        = cv_summary,
    cv_folds          = cv_folds,
    plots = list(
      boxplot = ana$plot,
      scatter = viz$scatter_plots
    ),
    before_after = viz$before_after
  ))
}
