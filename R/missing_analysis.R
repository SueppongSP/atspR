# =============================================================================
# atspR/R/missing_analysis.R
# Step 1 - Analyse missing values and produce boxplots
# =============================================================================

#' Analyse Missing Values in a Time-Series Dataset
#'
#' Computes a per-variable missing-value summary table and draws boxplots
#' for every numeric column. Results are returned invisibly as a list so
#' they can be passed directly into `handle_missing()`.
#'
#' @param data A `data.frame` or tibble.
#' @param plot Logical. If `TRUE` (default) boxplots are drawn.
#' @param verbose Logical. If `TRUE` (default) the summary table is printed.
#'
#' @return An invisible list with elements:
#' \describe{
#'   \item{`summary`}{A `data.frame` with columns `variable`, `n_missing`,
#'     `pct_missing`, and `pct_missing_num`.}
#'   \item{`overall_pct`}{Overall fraction of missing cells (numeric 0-1).}
#'   \item{`plot`}{A `ggplot` object (boxplots), or `NULL`.}
#'   \item{`data`}{The original `data` passed in (unchanged).}
#' }
#'
#' @examples
#' data(airquality)
#' result <- missing_analysis(airquality)
#' result$summary
#'
#' @export
missing_analysis <- function(data, plot = TRUE, verbose = TRUE) {

  .check_df(data)

  n_rows  <- nrow(data)
  n_cells <- n_rows * ncol(data)

  # ── Per-variable summary ──────────────────────────────────────────────────
  miss_tbl <- data.frame(
    variable  = names(data),
    n_missing = vapply(data, function(x) sum(is.na(x)), integer(1)),
    stringsAsFactors = FALSE
  )
  miss_tbl$pct_missing_num <- miss_tbl$n_missing / n_rows
  miss_tbl$pct_missing     <- .pct(miss_tbl$pct_missing_num)
  miss_tbl <- miss_tbl[order(-miss_tbl$n_missing), ]

  overall_pct <- sum(miss_tbl$n_missing) / n_cells

  # ── Console output ────────────────────────────────────────────────────────
  if (verbose) {
    .header("TIME SERIES MISSING VALUE ANALYSIS")
    miss_cols <- sum(miss_tbl$n_missing > 0)
    cat(sprintf("  Overall missing : %s  (%d of %d columns affected)\n\n",
                .pct(overall_pct), miss_cols, ncol(data)))

    if (miss_cols > 0) {
      .subheader("Per-Variable Summary")
      print(miss_tbl[miss_tbl$n_missing > 0,
                     c("variable", "n_missing", "pct_missing")],
            row.names = FALSE)
      cat("\n")
    }
  }

  # ── Boxplots ─────────────────────────────────────────────────────────────
  bp_plot  <- NULL
  num_cols <- .numeric_cols(data)

  if (plot && length(num_cols) > 0) {

    long_list <- lapply(num_cols, function(col) {
      vals <- data[[col]]
      vals <- vals[!is.na(vals)]
      data.frame(variable = col, value = vals, stringsAsFactors = FALSE)
    })
    long_df <- do.call(rbind, long_list)

    bp_plot <- ggplot2::ggplot(long_df,
                               ggplot2::aes(x = variable, y = value, fill = variable)) +
      ggplot2::geom_boxplot(colour = "grey30", alpha = 0.75,
                            outlier.colour = .pp_palette["highlight"],
                            outlier.size   = 1.5) +
      ggplot2::stat_summary(fun = mean, geom = "point",
                            shape = 23, size = 3,
                            fill = "white", colour = "grey20") +
      ggplot2::scale_fill_viridis_d(option = "plasma", guide = "none") +
      ggplot2::labs(
        title    = "Boxplots - Numeric Variables",
        subtitle = paste0("Overall missing: ", .pct(overall_pct)),
        x        = NULL,
        y        = "Value"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title    = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(colour = "grey40"),
        axis.text.x   = ggplot2::element_text(angle = 35, hjust = 1)
      )

    print(bp_plot)
  }

  invisible(list(
    summary     = miss_tbl,
    overall_pct = overall_pct,
    plot        = bp_plot,
    data        = data
  ))
}
