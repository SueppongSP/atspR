# =============================================================================
# atspR/R/cross_validate.R
# Step 6 - K-fold cross-validation on the training set
# =============================================================================

#' K-Fold Cross-Validation on the Training Set
#'
#' Performs walk-forward (time-ordered) k-fold cross-validation using a
#' user-supplied model function. Each validation fold is always preceded
#' only by past data, so no future information leaks into training.
#'
#' A seed training set of size `min_train_size` is reserved from the start of
#' the data before folding, guaranteeing every fold (including fold 1) has
#' enough data to train on.
#'
#' @param scale_result The list returned by [scale_data()]; provides
#'   `train_scaled`.  Alternatively, supply a plain `data.frame` to `train`.
#' @param train A `data.frame`.  Used only when `scale_result` is `NULL`.
#' @param target_col Character.  Name of the response column.
#' @param model_fn Function with signature `function(train_fold, val_fold)`.
#'   Must return a named numeric vector / list that includes at least one
#'   performance metric (e.g. `c(RMSE = ..., MAE = ..., R2 = ...)`).
#'   A simple linear-regression default is provided when `NULL`.
#' @param k Integer.  Number of folds (default 5).
#' @param min_train_size Numeric in (0, 1).  Fraction of rows reserved as seed
#'   training data before folding begins.  Default `0.2` (20 %).
#'   This ensures fold 1 always has training data available.
#' @param verbose Logical (default `TRUE`).
#'
#' @return An invisible list with elements:
#' \describe{
#'   \item{`fold_results`}{A `data.frame` with one row per fold.}
#'   \item{`summary`}{A `data.frame` with mean and SD of each metric.}
#'   \item{`k`}{Number of folds requested.}
#'   \item{`k_evaluated`}{Number of folds actually evaluated.}
#'   \item{`min_train_size`}{Seed fraction used.}
#'   \item{`target_col`}{Response column name.}
#' }
#'
#' @examples
#' data(airquality)
#' clean <- airquality[complete.cases(airquality), ]
#' sp    <- split_data(clean, verbose = FALSE)
#' sc    <- scale_data(sp, verbose = FALSE)
#' cv    <- cross_validate(sc, target_col = "Ozone", k = 5)
#' cv$summary
#'
#' @export
cross_validate <- function(scale_result   = NULL,
                           train          = NULL,
                           target_col,
                           model_fn       = NULL,
                           k              = 5L,
                           min_train_size = 0.2,
                           verbose        = TRUE) {

  # -- Force k to integer ----------------------------------------------------
  k <- as.integer(k)

  # -- Validate min_train_size -----------------------------------------------
  if (!is.numeric(min_train_size) || min_train_size <= 0 || min_train_size >= 1)
    rlang::abort("`min_train_size` must be a number strictly between 0 and 1.")

  # -- Resolve training data -------------------------------------------------
  if (!is.null(scale_result)) {
    train_data <- scale_result$train_scaled
  } else if (!is.null(train)) {
    train_data <- train
  } else {
    rlang::abort("Supply either `scale_result` or `train`.")
  }

  .check_df(train_data)

  if (!target_col %in% names(train_data))
    rlang::abort(sprintf("`target_col` '%s' not found in training data.", target_col))

  if (k < 2L || k > nrow(train_data))
    rlang::abort(sprintf("`k` must be between 2 and nrow(train) = %d.", nrow(train_data)))

  # -- Split seed train vs fold region ------------------------------------
  n         <- nrow(train_data)
  seed_n    <- max(1L, floor(n * min_train_size))
  seed_data <- train_data[seq_len(seed_n), , drop = FALSE]
  fold_data <- train_data[seq(seed_n + 1L, n), , drop = FALSE]
  n_fold    <- nrow(fold_data)

  if (n_fold < k)
    rlang::abort(sprintf(
      "After reserving seed train (%d rows), only %d rows remain for %d folds.\n  Reduce `k` or decrease `min_train_size`.",
      seed_n, n_fold, k
    ))

  fold_id <- cut(seq_len(n_fold), breaks = k, labels = FALSE)

  # -- Track whether model is default ---------------------------------------
  is_default_model <- is.null(model_fn)

  # -- Default model: OLS linear regression ---------------------------------
  if (is_default_model) {
    model_fn <- function(train_fold, val_fold) {
      fmla <- stats::as.formula(paste(target_col, "~ ."))

      tryCatch({
        fit     <- stats::lm(fmla, data = train_fold)
        preds   <- stats::predict(fit, newdata = val_fold)
        actuals <- val_fold[[target_col]]
        resid   <- actuals - preds
        ss_res  <- sum(resid^2, na.rm = TRUE)
        ss_tot  <- sum((actuals - mean(actuals, na.rm = TRUE))^2, na.rm = TRUE)

        c(RMSE = sqrt(mean(resid^2, na.rm = TRUE)),
          MAE  = mean(abs(resid),   na.rm = TRUE),
          R2   = if (ss_tot == 0) NA_real_ else 1 - ss_res / ss_tot)
      }, error = function(e) {
        warning("Fold failed: ", conditionMessage(e))
        c(RMSE = NA_real_, MAE = NA_real_, R2 = NA_real_)
      })
    }
  }

  if (verbose) {
    .header(paste0("STEP : Walk-Forward Cross-Validation  (k = ", k, ")"))
    cat(sprintf("  Target : %s  |  Seed: %d rows (%.0f%%)  |  Folds: %d (~%d rows each)\n\n",
                target_col, seed_n, min_train_size * 100, k, floor(n_fold / k)))
    .subheader("Results per fold")
    cat("  RMSE / MAE = error (lower is better)  |  R2 = fit (closer to 1.0 is better)\n\n")
    cat(sprintf("  %-6s  %-10s  %-8s  %-8s  %-8s  %-8s\n",
                "Fold", "n_train", "n_val", "RMSE", "MAE", "R2"))
    cat(sprintf("  %s\n", strrep("-", 56)))
  }

  fold_results <- vector("list", k)

  for (i in seq_len(k)) {

    # -- Walk-forward: seed + all prior folds ----------------------------
    prior_fold_idx <- which(fold_id < i)
    val_idx        <- which(fold_id == i)

    if (length(prior_fold_idx) > 0) {
      prior_rows <- fold_data[prior_fold_idx, , drop = FALSE]
      fold_train <- rbind(seed_data, prior_rows)
    } else {
      fold_train <- seed_data   # fold 1: train = seed only
    }

    fold_val <- fold_data[val_idx, , drop = FALSE]

    metrics <- tryCatch(
      as.list(model_fn(fold_train, fold_val)),
      error = function(e) {
        warning(sprintf("Fold %d failed: %s", i, conditionMessage(e)))
        list(RMSE = NA_real_, MAE = NA_real_, R2 = NA_real_)
      }
    )

    fold_results[[i]] <- c(fold    = i,
                           n_train = nrow(fold_train),
                           n_val   = length(val_idx),
                           metrics)

    if (verbose) {
      m <- as.list(metrics)
      cat(sprintf("  %-6d  %-10d  %-8d  %-8s  %-8s  %-8s\n",
                  i,
                  nrow(fold_train),
                  length(val_idx),
                  if (!is.null(m$RMSE) && !is.na(m$RMSE)) sprintf("%.4f", m$RMSE) else "  NA  ",
                  if (!is.null(m$MAE)  && !is.na(m$MAE))  sprintf("%.4f", m$MAE)  else "  NA  ",
                  if (!is.null(m$R2)   && !is.na(m$R2))   sprintf("%.4f", m$R2)   else "  NA  "))
    }

  }  # end for loop

  # -- Remove NULL entries ---------------------------------------------------
  fold_results <- Filter(Negate(is.null), fold_results)

  fold_df <- as.data.frame(do.call(rbind, lapply(fold_results, function(x)
    as.data.frame(x, stringsAsFactors = FALSE))))

  metric_cols <- setdiff(names(fold_df), c("fold", "n_train", "n_val"))
  fold_df[metric_cols] <- lapply(fold_df[metric_cols], as.numeric)

  k_evaluated <- nrow(fold_df)

  # -- Summary statistics ----------------------------------------------------
  summary_df <- do.call(rbind, lapply(metric_cols, function(m) {
    vals <- fold_df[[m]]
    data.frame(metric = m,
               mean   = round(mean(vals, na.rm = TRUE), 4),
               sd     = round(stats::sd(vals, na.rm = TRUE), 4),
               min    = round(min(vals,  na.rm = TRUE), 4),
               max    = round(max(vals,  na.rm = TRUE), 4),
               stringsAsFactors = FALSE)
  }))

  # -- Build fold_df with Mean row appended --------------------------------
  # Keep fold + metric columns only (drop n_train, n_val)
  show_cols <- c("fold", metric_cols)

  fold_df_show <- fold_df[, show_cols, drop = FALSE]
  fold_df_show[metric_cols] <- lapply(fold_df_show[metric_cols], function(x)
    sprintf("%.4f", round(as.numeric(x), 4))
  )
  fold_df_show[["fold"]] <- as.character(fold_df_show[["fold"]])

  mean_row <- as.data.frame(
    c(list(fold = "Mean"),
      stats::setNames(
        lapply(metric_cols, function(m)
          sprintf("%.4f", round(mean(fold_df[[m]], na.rm = TRUE), 4))
        ),
        metric_cols
      )
    ),
    stringsAsFactors = FALSE
  )

  fold_df_display <- rbind(fold_df_show, mean_row)

  if (verbose) {
    cat("\n")
    .subheader(paste0("Summary  (", k_evaluated, "/", k, " folds)"))
    cat("  mean = avg across folds  |  sd = consistency (lower = more stable)\n\n")
    cat(sprintf("  %-8s  %-8s  %-8s  %-8s  %-8s\n", "metric", "mean", "sd", "min", "max"))
    cat(sprintf("  %s\n", strrep("-", 46)))
    for (i in seq_len(nrow(summary_df))) {
      r <- summary_df[i, ]
      cat(sprintf("  %-8s  %-8s  %-8s  %-8s  %-8s\n",
                  r$metric,
                  sprintf("%.4f", r$mean),
                  sprintf("%.4f", r$sd),
                  sprintf("%.4f", r$min),
                  sprintf("%.4f", r$max)))
    }
    cat("\n")
  }

  invisible(list(
    fold_results   = fold_df,
    summary        = summary_df,
    k              = k,
    k_evaluated    = k_evaluated,
    min_train_size = min_train_size,
    target_col     = target_col
  ))
}
