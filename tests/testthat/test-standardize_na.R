test_that("standardize_na converts default missing strings to NA", {
  df <- data.frame(
    x = c("1", "-", "N/A", "Missing", "none", ""),
    stringsAsFactors = FALSE
  )
  result <- standardize_na(df, verbose = FALSE)
  expect_equal(sum(is.na(result$x)), 5)
  expect_equal(result$x[1], "1")
})

test_that("standardize_na converts user-defined na_strings", {
  df <- data.frame(x = c("valid", "INVALID", "ok"), stringsAsFactors = FALSE)
  result <- standardize_na(df, na_strings = c("INVALID"), verbose = FALSE)
  expect_true(is.na(result$x[2]))
  expect_false(is.na(result$x[1]))
})

test_that("standardize_na converts na_numbers", {
  df <- data.frame(x = c(1.0, -999, 3.0, 9999))
  result <- standardize_na(df, na_numbers = c(-999, 9999), verbose = FALSE)
  expect_true(is.na(result$x[2]))
  expect_true(is.na(result$x[4]))
  expect_false(is.na(result$x[1]))
})

test_that("standardize_na handles cols argument", {
  df <- data.frame(
    a = c("Missing", "ok"),
    b = c("Missing", "ok"),
    stringsAsFactors = FALSE
  )
  result <- standardize_na(df, cols = "a", verbose = FALSE)
  expect_true(is.na(result$a[1]))
  expect_false(is.na(result$b[1]))  # b ไม่โดนแตะ
})

test_that("standardize_na errors on non-data.frame input", {
  expect_error(standardize_na(list(x = 1), verbose = FALSE))
})
