test_that("coerce_numeric converts pure-numeric character columns", {
  df <- data.frame(
    x = c("1.1", "2.2", "3.3"),
    stringsAsFactors = FALSE
  )
  result <- coerce_numeric(df, verbose = FALSE)
  expect_true(is.numeric(result$x))
  expect_equal(result$x, c(1.1, 2.2, 3.3))
})

test_that("coerce_numeric skips columns below min_success_rate", {
  df <- data.frame(
    x = c("A", "B", "C"),   # 0% numeric
    stringsAsFactors = FALSE
  )
  result <- coerce_numeric(df, verbose = FALSE)
  expect_true(is.character(result$x))
})

test_that("coerce_numeric respects min_success_rate threshold", {
  df <- data.frame(
    x = c("1", "2", "abc"),  # 67% success
    stringsAsFactors = FALSE
  )
  # ต่ำกว่า 80% → ไม่ convert
  result_strict <- coerce_numeric(df, min_success_rate = 0.8, verbose = FALSE)
  expect_true(is.character(result_strict$x))

  # ต่ำกว่า 60% → convert ได้
  result_loose <- coerce_numeric(df, min_success_rate = 0.6, verbose = FALSE)
  expect_true(is.numeric(result_loose$x))
})

test_that("coerce_numeric only targets specified cols", {
  df <- data.frame(
    a = c("1", "2", "3"),
    b = c("4", "5", "6"),
    stringsAsFactors = FALSE
  )
  result <- coerce_numeric(df, cols = "a", verbose = FALSE)
  expect_true(is.numeric(result$a))
  expect_true(is.character(result$b))
})
