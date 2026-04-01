# =============================================================================
# atspR/R/combine_datetime.R
# Utility - Combine separate date and time columns into one POSIXct column
# =============================================================================

# ── Internal: auto-detect date format ────────────────────────────────────────
.detect_date_format <- function(x) {
  formats <- c(
    "%Y-%m-%d",    # 2024-01-31
    "%Y/%m/%d",    # 2024/01/31
    "%d-%m-%Y",    # 31-01-2024
    "%d/%m/%Y",    # 31/01/2024
    "%m-%d-%Y",    # 01-31-2024
    "%m/%d/%Y",    # 01/31/2024
    "%d %b %Y",    # 31 Jan 2024
    "%d %B %Y",    # 31 January 2024
    "%b %d, %Y",   # Jan 31, 2024
    "%B %d, %Y",   # January 31, 2024
    "%Y%m%d",      # 20240131
    "%d.%m.%Y",    # 31.01.2024
    "%Y.%m.%d"     # 2024.01.31
  )

  sample_val <- as.character(stats::na.omit(x))[1]
  if (is.na(sample_val)) return(NA_character_)

  for (fmt in formats) {
    parsed <- suppressWarnings(as.Date(sample_val, format = fmt))
    if (!is.na(parsed)) return(fmt)
  }
  return(NA_character_)
}

#' Combine Separate Date and Time Columns into One Timestamp
#'
#' Many datasets store date and time in separate columns. This function
#' merges them into a single `POSIXct` column ready for [fill_time_gaps()].
#' The date format is detected automatically.
#'
#' @param data A `data.frame`.
#' @param date_col Character. Name of the date column. Supports many formats
#'   automatically:
#'   \itemize{
#'     \item `"2024-01-31"` or `"2024/01/31"`
#'     \item `"31-01-2024"` or `"31/01/2024"`
#'     \item `"01-31-2024"` or `"01/31/2024"`
#'     \item `"31.01.2024"` or `"2024.01.31"`
#'     \item `"31 Jan 2024"` or `"January 31, 2024"`
#'     \item `"20240131"` (compact)
#'   }
#' @param time_col Character. Name of the time/hour column.
#' @param new_col Character. Name of the new combined column.
#'   Default `"datetime"`.
#' @param date_format Character. Override auto-detection with a `strptime`
#'   format string. Leave `NULL` (default) to auto-detect.
#' @param time_type Character. How the time column is stored:
#'   \itemize{
#'     \item `"hour"`   - integer 0-23, e.g. `8`, `14`
#'     \item `"hhmm"`   - integer HHMM, e.g. `830`, `1430`
#'     \item `"string"` - character `"HH:MM"` or `"HH:MM:SS"`
#'   }
#' @param tz Character. Timezone. Default `"UTC"`.
#' @param drop_cols Logical. If `TRUE` (default), remove `date_col` and
#'   `time_col` after combining.
#' @param verbose Logical (default `TRUE`).
#'
#' @return The original `data.frame` with a new `POSIXct` column prepended
#'   (and optionally the source columns removed).
#'
#' @examples
#' # Hour stored as integer
#' df <- data.frame(
#'   date = c("2024-01-01", "2024-01-01", "2024-01-01"),
#'   hour = c(8L, 9L, 11L),
#'   temp = c(25.1, 25.4, 26.2)
#' )
#' df2 <- combine_datetime(df, date_col = "date", time_col = "hour",
#'                         time_type = "hour")
#' df2$datetime
#'
#' # Time stored as "HH:MM" string
#' df3 <- data.frame(
#'   date = c("01/01/2024", "01/01/2024"),
#'   time = c("08:30", "09:00"),
#'   val  = c(10, 12)
#' )
#' df4 <- combine_datetime(df3, date_col = "date", time_col = "time",
#'                         time_type = "string")
#'
#' @export
combine_datetime <- function(data,
                             date_col    = "date",
                             time_col    = "time",
                             new_col     = "datetime",
                             date_format = NULL,
                             time_type   = c("hour", "hhmm", "string"),
                             tz          = "UTC",
                             drop_cols   = TRUE,
                             verbose     = TRUE) {

  .check_df(data)
  time_type <- match.arg(time_type)

  if (!date_col %in% names(data))
    rlang::abort(sprintf("`date_col` '%s' not found in data.", date_col))
  if (!time_col %in% names(data))
    rlang::abort(sprintf("`time_col` '%s' not found in data.", time_col))

  # ── Auto-detect date format ───────────────────────────────────────────────
  if (is.null(date_format)) {
    date_format   <- .detect_date_format(data[[date_col]])
    detected_auto <- TRUE
    if (is.na(date_format))
      rlang::abort(paste0(
        "Could not auto-detect date format from column '", date_col, "'.\n",
        "  Please supply `date_format` manually, e.g. date_format = '%d-%m-%Y'."
      ))
  } else {
    detected_auto <- FALSE
  }

  date_vec <- as.Date(data[[date_col]], format = date_format)

  n_fail <- sum(is.na(date_vec) & !is.na(data[[date_col]]))
  if (n_fail > 0)
    rlang::abort(sprintf(
      "%d date values could not be parsed with format '%s'.\n  Check column '%s'.",
      n_fail, date_format, date_col
    ))

  # ── Build time string ─────────────────────────────────────────────────────
  time_str <- switch(time_type,

    hour = {
      h <- as.integer(data[[time_col]])
      if (any(h < 0 | h > 23, na.rm = TRUE))
        rlang::abort("Hour values must be between 0 and 23.")
      sprintf("%02d:00:00", h)
    },

    hhmm = {
      v <- as.integer(data[[time_col]])
      h <- v %/% 100
      m <- v %%  100
      if (any(h < 0 | h > 23, na.rm = TRUE))
        rlang::abort("Hour part of HHMM must be between 0 and 23.")
      if (any(m < 0 | m > 59, na.rm = TRUE))
        rlang::abort("Minute part of HHMM must be between 0 and 59.")
      sprintf("%02d:%02d:00", h, m)
    },

    string = {
      t <- trimws(as.character(data[[time_col]]))
      ifelse(nchar(t) == 5, paste0(t, ":00"), t)
    }
  )

  # ── Combine → POSIXct ─────────────────────────────────────────────────────
  datetime_str    <- paste(as.character(date_vec), time_str)
  data[[new_col]] <- as.POSIXct(datetime_str,
                                format = "%Y-%m-%d %H:%M:%S",
                                tz     = tz)

  # ── Reorder columns: new_col first ────────────────────────────────────────
  other_cols <- setdiff(names(data), c(new_col, date_col, time_col))
  if (drop_cols) {
    data <- data[, c(new_col, other_cols), drop = FALSE]
  } else {
    data <- data[, c(new_col, date_col, time_col, other_cols), drop = FALSE]
  }

  # ── Console output ────────────────────────────────────────────────────────
  if (verbose) {
    .header("COMBINE DATE + TIME COLUMNS")
    cat(sprintf("  Date column : %s\n", date_col))
    cat(sprintf("  Date format : %s  %s\n",
                date_format,
                ifelse(detected_auto, "(auto-detected)", "(user-supplied)")))
    cat(sprintf("  Time column : %s  (type: %s)\n", time_col, time_type))
    cat(sprintf("  New column  : %s  (tz: %s)\n",   new_col,  tz))
    cat(sprintf("  Drop originals : %s\n\n",
                ifelse(drop_cols, "yes", "no")))
    cat("  Sample output:\n")
    print(utils::head(data[[new_col]], 5))
    cat("\n")
  }

  data
}
