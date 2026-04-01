# =============================================================================
# atspR/R/coerce_numeric.R
# Utility - Auto-convert character/factor columns to numeric where possible
# =============================================================================

#' Auto-Convert Columns to Numeric
#'
#' Scans all character and factor columns and attempts to convert them to
#' numeric. A column is converted only when the proportion of values that
#' can be parsed as numbers meets the `min_success_rate` threshold.
#' Values that cannot be parsed become `NA`.
#'
#' This step should run **after** [standardize_na()] so that custom missing
#' indicators (e.g. `"-"`, `"Missing"`) are already `NA` and do not count
#' against the success rate.
#'
#' @param data A `data.frame`.
#' @param cols Character vector. Columns to attempt conversion on.
#'   `NULL` (default) tries all character and factor columns.
#' @param min_success_rate Numeric in (0, 1]. Minimum fraction of non-NA
#'   values that must parse successfully for the column to be converted.
#'   Default `0.8` (80%).
#' @param verbose Logical (default `TRUE`).
#'
#' @return The `data.frame` with eligible columns converted to numeric.
#'
#' @examples
#' df <- data.frame(
#'   date  = c("2024-01-01", "2024-01-02", "2024-01-03"),
#'   temp  = c("25.1", "26.3", "NA"),
#'   humid = c("80", "85", "90"),
#'   label = c("A", "B", "C"),
#'   stringsAsFactors = FALSE
#' )
#' # temp and humid will be converted; date and label will stay as character
#' result <- coerce_numeric(df)
#' str(result)
#'
#' @export
coerce_numeric <- function(data,
                           cols              = NULL,
                           min_success_rate  = 0.8,
                           verbose           = TRUE) {

  .check_df(data)

  # target only character and factor columns
  candidate_cols <- names(data)[vapply(data, function(x) {
    is.character(x) || is.factor(x)
  }, logical(1))]

  if (!is.null(cols))
    candidate_cols <- intersect(cols, candidate_cols)

  if (length(candidate_cols) == 0) {
    if (verbose) {
      .header("COERCE TO NUMERIC")
      cat("  No character/factor columns found to convert.\n\n")
    }
    return(data)
  }

  converted  <- character(0)
  skipped    <- character(0)
  result_tbl <- vector("list", length(candidate_cols))
  names(result_tbl) <- candidate_cols

  for (col in candidate_cols) {
    x     <- as.character(data[[col]])
    x_num <- suppressWarnings(as.numeric(x))

    non_na_orig <- sum(!is.na(x))
    non_na_num  <- sum(!is.na(x_num))

    # success rate = how many non-NA originals parsed successfully
    rate <- if (non_na_orig == 0) 0 else non_na_num / non_na_orig

    result_tbl[[col]] <- list(
      rate      = rate,
      converted = rate >= min_success_rate,
      n_coerced_na = non_na_orig - non_na_num   # values lost to NA on parse
    )

    if (rate >= min_success_rate) {
      data[[col]] <- x_num
      converted   <- c(converted, col)
    } else {
      skipped <- c(skipped, col)
    }
  }

  # ── Console output ──────────────────────────────────────────────────────
  if (verbose) {
    .header("COERCE TO NUMERIC")
    cat(sprintf("  Min success rate : %.0f%%\n\n", min_success_rate * 100))

    summary_df <- do.call(rbind, lapply(candidate_cols, function(col) {
      r <- result_tbl[[col]]
      data.frame(
        column    = col,
        success   = paste0(round(r$rate * 100, 1), "%"),
        converted = ifelse(r$converted, "YES", "no"),
        na_added  = r$n_coerced_na,
        stringsAsFactors = FALSE
      )
    }))
    print(summary_df, row.names = FALSE)
    cat("\n")

    cat(sprintf("  Converted : %d column(s)  [%s]\n",
                length(converted),
                if (length(converted)) paste(converted, collapse = ", ") else "-"))
    cat(sprintf("  Skipped   : %d column(s)  [%s]\n\n",
                length(skipped),
                if (length(skipped)) paste(skipped, collapse = ", ") else "-"))
  }

  data
}
