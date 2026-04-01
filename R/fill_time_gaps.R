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
#' After filling gaps the result is ready to pass into
#' [atspR::missing_analysis()] and [atspR::handle_missing()].
#'
#' @param data A `data.frame` with a timestamp column, sorted ascending.
#' @param time_col Character. Name of the timestamp column
#'   (must be `Date` or `POSIXct`). Use [atspR::combine_datetime()] first if
#'   date and time are stored in separate columns.
#' @param n Numeric. Number of units per step. e.g. `1`, `3`, `15`.
#' @param unit Character. One of `"sec"`, `"min"`, `"hour"`, `"day"`,
#'   `"month"`, `"quarter"`, `"year"`.
#'   \itemize{
#'     \item `"month"`   - calendar months (1, 2, 3, ...)
#'     \item `"quarter"` - calendar quarters of 3 months each (Q1, Q2, Q3, Q4)
#'   }
#'   Note: `"month"` and `"quarter"` require the timestamp column to be `Date`
#'   and each value to fall on the first day of the month
#'   (e.g. `2024-01-01`, `2024-04-01`).
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
#'   \item{`freq`}{Frequency string describing the step used.}
#' }
#'
#' @examples
#' # Hourly data with a missing row
#' df <- data.frame(
#'   datetime = as.POSIXct(c("2024-01-01 08:00:00",
#'                            "2024-01-01 09:00:00",
#'                            "2024-01-01 11:00:00")),
#'   temp  = c(25.1, 25.4, 26.2),
#'   humid = c(80, NA, 85)
#' )
#' result <- fill_time_gaps(df, time_col = "datetime", n = 1, unit = "hour")
#' result$data
#'
#' # Monthly data - missing February
#' df_m <- data.frame(
#'   date  = as.Date(c("2024-01-01", "2024-03-01", "2024-04-01")),
#'   sales = c(100, 130, 150)
#' )
#' result_m <- fill_time_gaps(df_m, time_col = "date", n = 1, unit = "month")
#'
#' # Quarterly data - missing Q2
#' df_q <- data.frame(
#'   date  = as.Date(c("2024-01-01", "2024-07-01", "2024-10-01")),
#'   sales = c(300, 420, 390)
#' )
#' result_q <- fill_time_gaps(df_q, time_col = "date", n = 1, unit = "quarter")
#'
#' @export
fill_time_gaps <- function(data,
                           time_col,
                           n    = 1,
                           unit = c("sec", "min", "hour", "day",
                                    "month", "quarter", "year"),
                           verbose = TRUE) {

  .check_df(data)
  unit <- match.arg(unit)

  if (!time_col %in% names(data))
    rlang::abort(sprintf("`time_col` '%s' not found in data.", time_col))

  if (!is.numeric(n) || n <= 0)
    rlang::abort("`n` must be a positive number.")

  ts <- data[[time_col]]

  # -- Validate timestamp class ----------------------------------------------
  if (!inherits(ts, c("Date", "POSIXct", "POSIXlt"))) {
    rlang::abort(sprintf(paste0(
      "Column '%s' must be Date or POSIXct/POSIXlt, not %s.\n",
      "  Use combine_datetime() first, or:\n",
      "  data$%s <- as.POSIXct(data$%s)"
    ), time_col, class(ts)[1], time_col, time_col))
  }

  # -- month/quarter require Date (not POSIXct) ------------------------------
  if (unit %in% c("month", "quarter") && !inherits(ts, "Date")) {
    rlang::abort(paste0(
      "unit = '", unit, "' requires a Date column, not POSIXct.\n",
      "  Convert first: data$", time_col, " <- as.Date(data$", time_col, ")"
    ))
  }

  # -- Build full expected sequence ------------------------------------------
  is_calendar <- unit %in% c("month", "quarter")

  if (is_calendar) {
    # quarter = 3 months per step
    months_per_step <- if (unit == "quarter") as.integer(n) * 3L else as.integer(n)
    full_seq <- seq.Date(
      from = min(ts, na.rm = TRUE),
      to   = max(ts, na.rm = TRUE),
      by   = paste(months_per_step, "months")
    )
    freq_str <- if (unit == "quarter") {
      sprintf("every %g quarter(s) [%d months]", n, months_per_step)
    } else {
      sprintf("every %g month(s)", n)
    }
  } else {
    freq_str <- paste(n, switch(unit,
                                sec  = "secs",
                                min  = "mins",
                                hour = "hours",
                                day  = "days",
                                year = "years"
    ))
    full_seq <- seq(
      from = min(ts, na.rm = TRUE),
      to   = max(ts, na.rm = TRUE),
      by   = freq_str
    )
  }

  # -- Find missing timestamps -----------------------------------------------
  val_cols     <- setdiff(names(data), time_col)
  n_na_before  <- sum(is.na(data[, val_cols, drop = FALSE]))

  existing_num  <- as.numeric(ts)
  full_seq_num  <- as.numeric(full_seq)
  missing_num   <- setdiff(full_seq_num, existing_num)
  gap_timestamps <- full_seq[full_seq_num %in% missing_num]
  n_gaps         <- length(gap_timestamps)

  # -- Insert missing rows ---------------------------------------------------
  if (n_gaps > 0) {

    # Build gap_df respecting each column's original type
    gap_df <- as.data.frame(
      lapply(val_cols, function(col) {
        x <- data[[col]]
        if (is.integer(x))   return(rep(NA_integer_,  n_gaps))
        if (is.numeric(x))   return(rep(NA_real_,     n_gaps))
        if (is.character(x)) return(rep(NA_character_, n_gaps))
        if (is.logical(x))   return(rep(NA,            n_gaps))
        if (is.factor(x))    return(factor(rep(NA, n_gaps), levels = levels(x)))
        rep(NA, n_gaps)
      }),
      stringsAsFactors = FALSE
    )
    names(gap_df) <- val_cols

    gap_df[[time_col]]  <- gap_timestamps
    gap_df              <- gap_df[, names(data), drop = FALSE]

    data_full           <- rbind(data, gap_df)
    data_full           <- data_full[order(data_full[[time_col]]), ]
    rownames(data_full) <- NULL
  } else {
    data_full <- data
  }

  n_na_after <- sum(is.na(data_full[, val_cols, drop = FALSE]))

  # -- Console output --------------------------------------------------------
  if (verbose) {
    unit_label <- switch(unit,
                         sec     = "second(s)",
                         min     = "minute(s)",
                         hour    = "hour(s)",
                         day     = "day(s)",
                         month   = "month(s)",
                         quarter = "quarter(s)",
                         year    = "year(s)"
    )

    .header("TIME GAP ANALYSIS & FILL")
    cat(sprintf("  Timestamp column : %s\n",          time_col))
    cat(sprintf("  Frequency        : every %g %s\n", n, unit_label))
    if (unit == "quarter")
      cat(sprintf("  (1 quarter = 3 months, step = %d months)\n",
                  months_per_step))
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
