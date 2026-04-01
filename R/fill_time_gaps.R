# =============================================================================
# atspR/R/fill_time_gaps.R
# Utility - Fill missing timestamps in time-series data
# =============================================================================

#' Fill Missing Timestamps in Time-Series Data
#'
#' Handles two types of missingness common in time-series:
#' \enumerate{
#'   \item **Missing rows** - timestamps that are absent entirely.
#'     These rows are inserted with `NA` for all value columns.
#'   \item **Missing values** - rows that exist but have `NA` in some columns.
#' }
#' After filling gaps the result is ready to pass into `missing_analysis()`
#' and [handle_missing()].
#'
#' @param data A `data.frame` with a timestamp column, sorted ascending.
#' @param time_col Character. Name of the timestamp column
#'   (must be `Date` or `POSIXct`). Use [combine_datetime()] first if date
#'   and time are stored in separate columns.
#' @param n Numeric. Number of units per step. e.g. `1`, `15`, `30`.
#' @param unit Character. One of `"sec"`, `"min"`, `"hour"`, `"day"`, `"year"`.
#' @param verbose Logical (default `TRUE`).
#'
#' @return An invisible list with elements:
#' \describe{
#'   \item{`data`}{Complete `data.frame` with no missing timestamps.
#'     Inserted rows have `NA` for all value columns.}
#'   \item{`n_gaps`}{Number of timestamp rows inserted.}
#'   \item{`n_na_before`}{Total NA cells before gap-filling.}
#'   \item{`n_na_after`}{Total NA cells after gap-filling
#'     (includes inserted rows).}
#'   \item{`gap_timestamps`}{Vector of timestamps that were inserted.}
#'   \item{`time_col`}{Name of the timestamp column.}
#'   \item{`freq`}{Frequency string used internally by `seq()`.}
#' }
#'
#' @examples
#' df <- data.frame(
#'   datetime = as.POSIXct(c("2024-01-01 08:00:00",
#'                            "2024-01-01 09:00:00",
#'                            "2024-01-01 11:00:00")),
#'   temp  = c(25.1, 25.4, 26.2),
#'   humid = c(80, NA, 85)
#' )
#'
#' # Fill missing 10:00 row
#' result <- fill_time_gaps(df, time_col = "datetime", n = 1, unit = "hour")
#' result$data
#' result$n_gaps
#'
#' # Every 15 minutes
#' result2 <- fill_time_gaps(df, time_col = "datetime", n = 15, unit = "min")
#'
#' @export
fill_time_gaps <- function(data,
                           time_col,
                           n    = 1,
                           unit = c("sec", "min", "hour", "day", "year"),
                           verbose = TRUE) {

  .check_df(data)
  unit <- match.arg(unit)

  if (!time_col %in% names(data))
    rlang::abort(sprintf("`time_col` '%s' not found in data.", time_col))

  if (!is.numeric(n) || n <= 0)
    rlang::abort("`n` must be a positive number.")

  ts <- data[[time_col]]

  # ── Validate timestamp class ──────────────────────────────────────────────
  if (!inherits(ts, c("Date", "POSIXct", "POSIXlt"))) {
    rlang::abort(sprintf(paste0(
      "Column '%s' must be Date or POSIXct/POSIXlt, not %s.\n",
      "  Use combine_datetime() first, or:\n",
      "  data$%s <- as.POSIXct(data$%s)"
    ), time_col, class(ts)[1], time_col, time_col))
  }

  # ── Build freq string for seq() ───────────────────────────────────────────
  freq_str <- paste(n, switch(unit,
                              sec  = "secs",
                              min  = "mins",
                              hour = "hours",
                              day  = "days",
                              year = "years"
  ))

  # ── Generate full expected sequence ──────────────────────────────────────
  full_seq <- seq(
    from = min(ts, na.rm = TRUE),
    to   = max(ts, na.rm = TRUE),
    by   = freq_str
  )

  # ── Find missing timestamps ───────────────────────────────────────────────
  val_cols      <- setdiff(names(data), time_col)
  n_na_before   <- sum(is.na(data[, val_cols, drop = FALSE]))

  existing_num  <- as.numeric(ts)
  full_seq_num  <- as.numeric(full_seq)
  missing_num   <- setdiff(full_seq_num, existing_num)
  gap_timestamps <- full_seq[full_seq_num %in% missing_num]
  n_gaps         <- length(gap_timestamps)

  # ── Insert missing rows ───────────────────────────────────────────────────
  if (n_gaps > 0) {
    gap_df <- data.frame(
      matrix(NA_real_, nrow = n_gaps, ncol = length(val_cols),
             dimnames = list(NULL, val_cols)),
      stringsAsFactors = FALSE
    )
    # preserve column types from original
    for (col in val_cols) {
      storage.mode(gap_df[[col]]) <- storage.mode(data[[col]])
    }
    gap_df[[time_col]]  <- gap_timestamps
    gap_df              <- gap_df[, names(data), drop = FALSE]

    data_full           <- rbind(data, gap_df)
    data_full           <- data_full[order(data_full[[time_col]]), ]
    rownames(data_full) <- NULL
  } else {
    data_full <- data
  }

  n_na_after <- sum(is.na(data_full[, val_cols, drop = FALSE]))

  # ── Console output ────────────────────────────────────────────────────────
  if (verbose) {
    unit_label <- switch(unit,
                         sec  = "second(s)",
                         min  = "minute(s)",
                         hour = "hour(s)",
                         day  = "day(s)",
                         year = "year(s)"
    )

    .header("TIME GAP ANALYSIS & FILL")
    cat(sprintf("  Timestamp column : %s\n",          time_col))
    cat(sprintf("  Frequency        : every %g %s\n", n, unit_label))
    cat(sprintf("  Expected steps   : %d\n",          length(full_seq)))
    cat(sprintf("  Actual rows      : %d\n",          nrow(data)))
    cat(sprintf("  Gaps inserted    : %d rows\n",     n_gaps))
    cat(sprintf("  NA cells before  : %d\n",          n_na_before))
    cat(sprintf("  NA cells after   : %d  (includes inserted rows)\n\n",
                n_na_after))

    if (n_gaps > 0) {
      .subheader("Inserted Timestamps (first 10)")
      print(utils::head(gap_timestamps, 10))
      cat("\n")
    } else {
      cat("  [OK] No missing timestamps found.\n\n")
    }
  }

  invisible(list(
    data           = data_full,
    n_gaps         = n_gaps,
    n_na_before    = n_na_before,
    n_na_after     = n_na_after,
    gap_timestamps = gap_timestamps,
    time_col       = time_col,
    freq           = freq_str
  ))
}
