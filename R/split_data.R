# =============================================================================
# atspR/R/split_data.R
# Step 4 – Temporal train / test split
# =============================================================================

#' Split Data into Temporal Train and Test Sets
#'
#' Performs a **time-ordered** (non-random) train/test split so that the
#' training set always precedes the test set chronologically – the correct
#' approach for time-series and any dataset where row order encodes time.
#'
#' @param data A `data.frame` (rows assumed ordered by time).
#' @param train_ratio Numeric in (0, 1).  Fraction of rows for training.
#'   Default `0.8` (80 / 20 split).
#' @param verbose Logical (default `TRUE`).
#'
#' @return An invisible list with elements:
#' \describe{
#'   \item{`train`}{Training `data.frame`.}
#'   \item{`test`}{Test `data.frame`.}
#'   \item{`train_idx`}{Integer vector of training row indices.}
#'   \item{`test_idx`}{Integer vector of test row indices.}
#'   \item{`ratio`}{Named numeric `c(train = ..., test = ...)`.}
#' }
#'
#' @examples
#' data(airquality)
#' clean <- airquality[complete.cases(airquality), ]
#' sp    <- split_data(clean, train_ratio = 0.7)
#' nrow(sp$train); nrow(sp$test)
#'
#' @export
split_data <- function(data, train_ratio = 0.8, verbose = TRUE) {

  .check_df(data)

  if (!is.numeric(train_ratio) || train_ratio <= 0 || train_ratio >= 1)
    rlang::abort("`train_ratio` must be a number strictly between 0 and 1.")

  n          <- nrow(data)
  train_n    <- floor(n * train_ratio)
  train_idx  <- seq_len(train_n)
  test_idx   <- seq(train_n + 1L, n)

  train <- data[train_idx, , drop = FALSE]
  test  <- data[test_idx,  , drop = FALSE]

  ratio <- c(train = train_n / n, test = (n - train_n) / n)

  if (verbose) {
    .header("TRAIN / TEST SPLIT  (temporal)")
    cat(sprintf("  Total rows  : %d\n",   n))
    cat(sprintf("  Train ratio : %.0f%%  (%d rows)\n",
                ratio["train"] * 100, nrow(train)))
    cat(sprintf("  Test  ratio : %.0f%%  (%d rows)\n\n",
                ratio["test"]  * 100, nrow(test)))
  }

  invisible(list(
    train     = train,
    test      = test,
    train_idx = train_idx,
    test_idx  = test_idx,
    ratio     = ratio
  ))
}
