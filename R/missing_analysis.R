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

  # -- Per-variable summary --------------------------------------------------
  miss_tbl <- data.frame(
    variable  = names(data),
    n_missing = vapply(data, function(x) sum(is.na(x)), integer(1)),
    stringsAsFactors = FALSE
  )
  miss_tbl$pct_missing_num <- miss_tbl$n_missing / n_rows
  miss_tbl$pct_missing     <- .pct(miss_tbl$pct_missing_num)
  miss_tbl <- miss_tbl[order(-miss_tbl$n_missing), ]

  overall_pct <- sum(miss_tbl$n_missing) / n_cells

  # -- Console output --------------------------------------------------------
  if (verbose) {
    miss_cols   <- sum(miss_tbl$n_missing > 0)
    do_impute   <- (n_rows <= 50) || (overall_pct > 0.05)
    miss_action <- if (do_impute) "IMPUTE" else "DROP"
    miss_reason <- if (n_rows <= 50) {
      sprintf("n = %d <= 50, too few rows to lose any", n_rows)
    } else if (overall_pct > 0.05) {
      sprintf("missing = %.2f%% > 5%% threshold", overall_pct * 100)
    } else {
      sprintf("missing = %.2f%% <= 5%% threshold, safe to remove rows", overall_pct * 100)
    }

    .header("STEP 3/7 : Missing Value Analysis")
    cat(sprintf("  Rows    : %d  |  Cols: %d  |  Total cells: %d\n",
                n_rows, ncol(data), n_cells))
    cat(sprintf("  Missing : %s  (%d cells)  |  %d / %d cols affected\n",
                .pct(overall_pct), sum(miss_tbl$n_missing), miss_cols, ncol(data)))
    cat(sprintf("  Action  : %s  -- %s\n\n", miss_action, miss_reason))

    if (miss_cols > 0) {
      .subheader("Missing per column")
      show_tbl <- miss_tbl[miss_tbl$n_missing > 0,
                           c("variable", "n_missing", "pct_missing"),
                           drop = FALSE]
      print(show_tbl, row.names = FALSE)
      cat("\n")
    } else {
      cat("  [OK] No missing values\n\n")
    }
  }

  # -- Boxplots -------------------------------------------------------------
  bp_plot  <- NULL
  num_cols <- .numeric_cols(data)

  if (plot && length(num_cols) > 0) {

    cols_per_page <- 4L
    n_cols  <- length(num_cols)
    n_pages <- ceiling(n_cols / cols_per_page)

    bp_plot <- vector("list", n_pages)

    for (page in seq_len(n_pages)) {

      idx_start <- (page - 1) * cols_per_page + 1
      idx_end   <- min(page * cols_per_page, n_cols)
      page_cols <- num_cols[idx_start:idx_end]

      long_df <- do.call(rbind, lapply(page_cols, function(col) {
        vals <- data[[col]]
        vals <- vals[!is.na(vals)]
        data.frame(variable = col, value = vals, stringsAsFactors = FALSE)
      }))

      p <- ggplot2::ggplot(long_df, ggplot2::aes(x = "", y = value, fill = variable)) +
        ggplot2::geom_boxplot(
          alpha = 0.8,
          outlier.colour = .pp_palette["highlight"]
        ) +
        ggplot2::stat_summary(
          fun = mean, geom = "point",
          shape = 23, size = 3,
          fill = "white", colour = "black"
        ) +
        ggplot2::facet_wrap(~ variable, scales = "free_y", ncol = 2) +
        ggplot2::scale_fill_viridis_d(option = "plasma", guide = "none") +
        ggplot2::labs(
          title = sprintf("Boxplots - Numeric Variables (page %d / %d)", page, n_pages),
          subtitle = paste0("Overall missing: ", .pct(overall_pct)),
          x = NULL,
          y = "Value"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          strip.text = ggplot2::element_text(face = "bold"),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        )

      print(p)
      bp_plot[[page]] <- p
    }
  }

  invisible(list(
    summary     = miss_tbl,
    overall_pct = overall_pct,
    plot        = bp_plot,
    data        = data
  ))
}
