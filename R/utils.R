# =============================================================================
# atspR/R/utils.R
# Internal utility functions (not exported)
# =============================================================================

# -- Declare global variables used in ggplot2 aes() -------------------------
# Suppresses "no visible binding for global variable" NOTE in R CMD check
utils::globalVariables(c("variable", "value", "index", "point_type"))

# -- Colour palette used across all plots ------------------------------------
.pp_palette <- c(
  original  = "#2C7BB6",
  imputed   = "#D7191C",
  train     = "#1A9641",
  test      = "#FDAE61",
  highlight = "#762A83",
  neutral   = "#636363"
)

# -- Check that input is a data.frame (or tibble) ----------------------------
.check_df <- function(data, arg = "data") {
  if (!is.data.frame(data)) {
    rlang::abort(
      paste0("`", arg, "` must be a data.frame or tibble, not ",
             class(data)[1], "."),
      call = rlang::caller_env()
    )
  }
  invisible(data)
}

# -- Select only numeric columns ---------------------------------------------
.numeric_cols <- function(data) {
  names(Filter(is.numeric, data))
}

# -- Pretty percentage string ------------------------------------------------
.pct <- function(x, digits = 2) {
  paste0(round(x * 100, digits), "%")
}

# -- Console section header --------------------------------------------------
.header <- function(title, width = 60) {
  line <- strrep("=", width)
  cat("\n", line, "\n", sep = "")
  cat("  ", title, "\n", sep = "")
  cat(line, "\n\n", sep = "")
}

# -- Console sub-header ------------------------------------------------------
.subheader <- function(title, width = 60) {
  line <- strrep("-", width)
  cat(line, "\n", sep = "")
  cat("  ", title, "\n", sep = "")
  cat(line, "\n", sep = "")
}
