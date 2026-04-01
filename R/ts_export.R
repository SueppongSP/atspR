# =============================================================================
# atspR/R/ts_export.R
# Utility - Export ts_preprocess() results to CSV files
# =============================================================================

#' Export Preprocessing Results to CSV
#'
#' Saves the scaled train and test sets (and optionally the cleaned data and
#' summary report) produced by [ts_preprocess()] to CSV files in a specified
#' directory.
#'
#' @param result The list returned by [ts_preprocess()].
#' @param dir Character. Directory to save files. Created if it does not exist.
#'   Default `"atspR_output"`.
#' @param prefix Character. Filename prefix. Default `"atspR"`.
#' @param export_clean Logical. Also export `data_clean`. Default `TRUE`.
#' @param export_report Logical. Also export `missing_report` and
#'   `scale_params` as CSV. Default `TRUE`.
#' @param verbose Logical (default `TRUE`).
#'
#' @return Invisibly returns a named character vector of exported file paths.
#'
#' @examples
#' \dontrun{
#' data(airquality)
#' result <- ts_preprocess(airquality, verbose = FALSE)
#' ts_export(result, dir = "output", prefix = "airquality")
#' }
#'
#' @export
ts_export <- function(result,
                      dir           = "atspR_output",
                      prefix        = "atspR",
                      export_clean  = TRUE,
                      export_report = TRUE,
                      verbose       = TRUE) {

  # -- Create directory ------------------------------------------------------
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    if (verbose) cat(sprintf("  Created directory: %s\n\n", dir))
  }

  exported <- character(0)

  # -- Helper: save + record -------------------------------------------------
  .save <- function(df, name) {
    path <- file.path(dir, paste0(prefix, "_", name, ".csv"))
    utils::write.csv(df, path, row.names = FALSE)
    exported <<- c(exported, stats::setNames(path, name))
    if (verbose) cat(sprintf("  %-25s -> %s\n", name, path))
    invisible(path)
  }

  if (verbose) {
    .header("EXPORT PREPROCESSING RESULTS")
    cat(sprintf("  Directory : %s\n", normalizePath(dir, mustWork = FALSE)))
    cat(sprintf("  Prefix    : %s\n\n", prefix))
  }

  # -- Core outputs ----------------------------------------------------------
  .save(result$train_scaled, "train_scaled")
  .save(result$test_scaled,  "test_scaled")

  # -- Optional: clean data --------------------------------------------------
  if (export_clean) {
    .save(result$data_clean, "data_clean")
  }

  # -- Optional: reports -----------------------------------------------------
  if (export_report) {
    .save(result$missing_report, "missing_report")

    # Flatten scale_params to data.frame
    param_df <- do.call(rbind, lapply(names(result$scale_params), function(col) {
      p <- result$scale_params[[col]]
      data.frame(column      = col,
                 method      = result$scale_method,
                 param1_name = names(p)[1],
                 param1_val  = round(p[[1]], 6),
                 param2_name = names(p)[2],
                 param2_val  = round(p[[2]], 6),
                 stringsAsFactors = FALSE)
    }))
    .save(param_df, "scale_params")

    # CV summary if available
    if (!is.null(result$cv_summary)) {
      .save(result$cv_summary, "cv_summary")
    }
    if (!is.null(result$cv_folds)) {
      # Drop n_train and n_val -- internal fold sizing info, not needed in export
      cv_folds_export <- result$cv_folds[,
                                         setdiff(names(result$cv_folds), c("n_train", "n_val")),
                                         drop = FALSE
      ]
      .save(cv_folds_export, "cv_folds")
    }
  }

  if (verbose) {
    cat(sprintf("\n  Total files exported : %d\n", length(exported)))
    cat(strrep("=", 60), "\n\n", sep = "")
  }

  invisible(exported)
}
