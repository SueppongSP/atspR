# =============================================================================
# atspR/R/visualize_data.R
# Step 5 - Scatter plots: each numeric column vs index, max 4 per page
# =============================================================================

#' Visualise Data After Missing-Value Handling
#'
#' Produces scatter plots of each numeric column vs row index (time order).
#' At most 4 subplots are shown per page -- if there are more than 4 numeric
#' columns, multiple plots are printed sequentially.
#' When imputation was performed, imputed points are highlighted in red.
#'
#' @param handle_result The list returned by [handle_missing()].
#' @param raw_data `data.frame`. The original (pre-cleaning) dataset.
#'   Required when `handle_result$action == "impute"`.
#' @param cols_per_page Integer. Maximum subplots per page. Default `4`.
#' @param n_sample Integer. Rows to show in before/after console table.
#'   Default `10`.
#' @param verbose Logical (default `TRUE`).
#' @param time_col Character. Name of the datetime column to use as x-axis.
#'   If `NULL` (default), row index is used instead.
#' @param x_col,y_col Ignored. Kept for backward compatibility.
#'
#' @return An invisible list with elements `scatter_plots` (a list of ggplot
#'   objects, one per page) and `before_after` (data.frame or NULL).
#'
#' @examples
#' data(airquality)
#' ana   <- missing_analysis(airquality, plot = FALSE, verbose = FALSE)
#' clean <- handle_missing(ana, verbose = FALSE)
#' viz   <- visualize_data(clean, raw_data = airquality)
#'
#' @export
visualize_data <- function(handle_result,
                           raw_data      = NULL,
                           cols_per_page = 4L,
                           n_sample      = 10L,
                           verbose       = TRUE,
                           time_col      = NULL,
                           x_col         = NULL,
                           y_col         = NULL) {

  data_clean   <- handle_result$data_clean
  imputed_mask <- handle_result$imputed_mask
  action       <- handle_result$action

  num_cols <- .numeric_cols(data_clean)
  if (length(num_cols) == 0)
    rlang::abort("No numeric columns found for plotting.")

  n         <- nrow(data_clean)
  n_cols    <- length(num_cols)
  n_pages   <- ceiling(n_cols / cols_per_page)

  colours <- c(Original = unname(.pp_palette["original"]),
               Imputed  = unname(.pp_palette["imputed"]))
  shapes  <- c(Original = 16L, Imputed = 17L)
  sizes   <- c(Original = 1.2, Imputed = 2.8)

  has_imputed <- action == "impute" &&
    any(as.matrix(imputed_mask[, num_cols, drop = FALSE]), na.rm = TRUE)

  subtitle_txt <- if (has_imputed) {
    paste0("Imputed points highlighted  |  method: ",
           handle_result$method,
           "  |  ", handle_result$n_imputed, " cells imputed")
  } else {
    "No imputation - rows with NA were dropped"
  }

  # ── Build one plot per page ───────────────────────────────────────────────
  scatter_plots <- vector("list", n_pages)

  for (page in seq_len(n_pages)) {

    idx_start <- (page - 1L) * cols_per_page + 1L
    idx_end   <- min(page * cols_per_page, n_cols)
    page_cols <- num_cols[idx_start:idx_end]

    # Auto-detect datetime column if time_col not supplied
    if (is.null(time_col)) {
      time_col <- names(data_clean)[vapply(data_clean, function(x)
        inherits(x, c("Date", "POSIXct", "POSIXlt")), logical(1))][1]
    }
    x_vals  <- if (!is.na(time_col) && !is.null(time_col) && time_col %in% names(data_clean)) {
      data_clean[[time_col]]
    } else {
      seq_len(n)
    }
    x_label <- if (!is.na(time_col) && !is.null(time_col) && time_col %in% names(data_clean)) time_col else "Index"

    long_df <- do.call(rbind, lapply(page_cols, function(col) {
      is_imp <- if (action == "impute") imputed_mask[[col]] else rep(FALSE, n)
      data.frame(
        index      = x_vals,
        value      = data_clean[[col]],
        variable   = col,
        point_type = factor(
          ifelse(is_imp, "Imputed", "Original"),
          levels = c("Original", "Imputed")
        ),
        stringsAsFactors = FALSE
      )
    }))

    page_title <- if (n_pages > 1) {
      sprintf("Each Variable vs Index  (page %d / %d)", page, n_pages)
    } else {
      "Each Variable vs Index (Time Order)"
    }

    p <- ggplot2::ggplot(
      long_df,
      ggplot2::aes(x = index, y = value,
                   colour = point_type,
                   shape  = point_type,
                   size   = point_type)
    ) +
      ggplot2::geom_line(
        ggplot2::aes(group = 1),
        colour = "grey70", linewidth = 0.4, alpha = 0.7
      ) +
      ggplot2::geom_point(alpha = 0.85) +
      ggplot2::facet_wrap(
        ~ variable,
        scales = "free_y",
        ncol   = min(2L, length(page_cols))
      ) +
      ggplot2::scale_colour_manual(values = colours, name = NULL) +
      ggplot2::scale_shape_manual (values = shapes,  name = NULL) +
      ggplot2::scale_size_manual  (values = sizes,   name = NULL) +
      ggplot2::labs(
        title    = page_title,
        subtitle = subtitle_txt,
        x        = x_label,
        y        = "Value"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        plot.title      = ggplot2::element_text(face = "bold"),
        plot.subtitle   = ggplot2::element_text(colour = "grey40", size = 9),
        legend.position = "bottom",
        strip.text      = ggplot2::element_text(face = "bold", size = 10),
        panel.spacing   = ggplot2::unit(1, "lines")
      )

    print(p)
    scatter_plots[[page]] <- p

    if (verbose && n_pages > 1)
      cat(sprintf("  [Plot %d/%d] columns: %s\n",
                  page, n_pages, paste(page_cols, collapse = ", ")))
  }

  if (verbose && n_pages > 1) cat("\n")

  # ── Before / after console table ─────────────────────────────────────────
  before_after <- NULL

  if (action == "impute" && !is.null(raw_data)) {
    imp_rows <- which(
      rowSums(as.matrix(imputed_mask[, num_cols, drop = FALSE])) > 0
    )

    if (length(imp_rows) > 0) {
      show_rows    <- utils::head(imp_rows, n_sample)
      before_df    <- raw_data[show_rows,   num_cols, drop = FALSE]
      after_df     <- data_clean[show_rows, num_cols, drop = FALSE]

      before_after <- data.frame(
        row = show_rows,
        stats::setNames(before_df, paste0(names(before_df), "_before")),
        stats::setNames(after_df,  paste0(names(after_df),  "_after")),
        stringsAsFactors = FALSE
      )

      if (verbose) {
        .header("IMPUTATION - BEFORE vs AFTER (sample)")
        print(before_after, row.names = FALSE)
        cat("\n")
      }
    }
  }

  invisible(list(scatter_plots = scatter_plots, before_after = before_after))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
