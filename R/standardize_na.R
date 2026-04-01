# =============================================================================
# atspR/R/standardize_na.R
# Utility - Convert custom missing value indicators to NA
# =============================================================================

# ── Internal: default patterns treated as NA ──────────────────────────────────
.default_na_strings <- c(
  # Dashes / blanks
  "-", "--", "---", "N/A", "n/a", "NA", "na", "N.A.", "n.a.",
  # Words
  "Missing", "missing", "MISSING",
  "None", "none", "NONE",
  "Null", "null", "NULL",
  "Unknown", "unknown", "UNKNOWN",
  "Undefined", "undefined",
  "NaN", "nan",
  # Empty-ish
  "", " ", "  ",
  # Thai
  "\u0e44\u0e21\u0e48\u0e21\u0e35\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25",   # ไม่มีข้อมูล
  "\u0e44\u0e21\u0e48\u0e23\u0e30\u0e1a\u0e38",                           # ไม่ระบุ
  "\u0e44\u0e21\u0e48\u0e17\u0e23\u0e32\u0e1a"                            # ไม่ทราบ
)

#' Standardise Missing Value Indicators to NA
#'
#' Converts custom "missing" representations such as `"-"`, `"Missing"`,
#' `"N/A"`, `"none"`, `"null"`, `""`, or any user-defined string/number
#' into proper `NA` values so that the rest of the pipeline can detect and
#' handle them correctly.
#'
#' @param data A `data.frame`.
#' @param na_strings Character vector. Additional strings to treat as `NA`.
#'   These are combined with the built-in defaults. Set to `character(0)` to
#'   use only the defaults.
#' @param na_numbers Numeric vector. Numeric sentinel values to treat as `NA`,
#'   e.g. `-999`, `9999`, `0`. Default `NULL` (none).
#' @param cols Character vector. Column names to process. `NULL` (default)
#'   processes all columns.
#' @param trim Logical. If `TRUE` (default), trim leading/trailing whitespace
#'   from character columns before matching.
#' @param verbose Logical (default `TRUE`).
#'
#' @return The `data.frame` with all matched values replaced by `NA`.
#'
#' @examples
#' df <- data.frame(
#'   date  = c("2024-01-01", "2024-01-02", "2024-01-03"),
#'   temp  = c(25.1, -999, 26.2),
#'   humid = c("80", "-", "Missing"),
#'   wind  = c("N/A", "12", "")
#' )
#'
#' # Use defaults + custom
#' clean <- standardize_na(df,
#'   na_strings = c("N/A"),
#'   na_numbers = c(-999)
#' )
#' clean
#'
#' @export
standardize_na <- function(data,
                           na_strings = character(0),
                           na_numbers = NULL,
                           cols       = NULL,
                           trim       = TRUE,
                           verbose    = TRUE) {

  .check_df(data)

  # columns to process
  target_cols <- if (is.null(cols)) names(data) else intersect(cols, names(data))

  # combine default + user strings
  all_na_strings <- unique(c(.default_na_strings, na_strings))

  n_before <- sum(is.na(data[, target_cols, drop = FALSE]))
  counts   <- integer(length(target_cols))
  names(counts) <- target_cols

  for (col in target_cols) {
    x       <- data[[col]]
    changed <- 0L

    # ── Character / factor columns ──────────────────────────────────────────
    if (is.character(x) || is.factor(x)) {
      x_chr <- as.character(x)
      if (trim) x_chr <- trimws(x_chr)
      hit           <- x_chr %in% all_na_strings
      changed       <- sum(hit & !is.na(x), na.rm = TRUE)
      x_chr[hit]    <- NA_character_
      data[[col]]   <- x_chr
    }

    # ── Numeric columns ─────────────────────────────────────────────────────
    if (is.numeric(x) && !is.null(na_numbers)) {
      hit         <- x %in% na_numbers
      changed     <- changed + sum(hit & !is.na(x), na.rm = TRUE)
      x[hit]      <- NA_real_
      data[[col]] <- x
    }

    counts[col] <- changed
  }

  n_after   <- sum(is.na(data[, target_cols, drop = FALSE]))
  n_convert <- n_after - n_before

  # ── Console output ──────────────────────────────────────────────────────
  if (verbose) {
    .header("STANDARDISE MISSING VALUES")

    cat(sprintf("  Built-in NA strings : %d patterns\n",
                length(.default_na_strings)))
    if (length(na_strings) > 0)
      cat(sprintf("  User NA strings     : %s\n",
                  paste(na_strings, collapse = ", ")))
    if (!is.null(na_numbers))
      cat(sprintf("  NA numbers          : %s\n",
                  paste(na_numbers, collapse = ", ")))
    cat(sprintf("  Cells converted     : %d\n\n", n_convert))

    changed_cols <- counts[counts > 0]
    if (length(changed_cols) > 0) {
      .subheader("Conversions per Column")
      conv_df <- data.frame(
        column    = names(changed_cols),
        converted = as.integer(changed_cols),
        stringsAsFactors = FALSE
      )
      print(conv_df, row.names = FALSE)
      cat("\n")
    } else {
      cat("  [OK] No custom missing values found.\n\n")
    }
  }

  data
}
