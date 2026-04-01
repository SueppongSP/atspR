test_that("missing_analysis returns correct summary structure", {
  data(airquality)
  result <- missing_analysis(airquality, plot = FALSE, verbose = FALSE)

  expect_named(result, c("summary", "overall_pct", "plot", "data"))
  expect_true(is.data.frame(result$summary))
  expect_true("n_missing" %in% names(result$summary))
  expect_true("pct_missing" %in% names(result$summary))
  expect_true(is.numeric(result$overall_pct))
  expect_true(result$overall_pct >= 0 && result$overall_pct <= 1)
})

test_that("missing_analysis overall_pct matches manual calculation", {
  data(airquality)
  result <- missing_analysis(airquality, plot = FALSE, verbose = FALSE)
  expected <- sum(is.na(airquality)) / (nrow(airquality) * ncol(airquality))
  expect_equal(result$overall_pct, expected)
})

test_that("handle_missing drops rows when missing < 5%", {
  # สร้างข้อมูลที่ missing น้อยกว่า 5%
  df <- data.frame(x = c(1:99, NA), y = 1:100)
  ana <- missing_analysis(df, plot = FALSE, verbose = FALSE)
  result <- handle_missing(ana, verbose = FALSE)

  expect_equal(result$action, "drop")
  expect_equal(nrow(result$data_clean), 99)
  expect_equal(sum(is.na(result$data_clean)), 0)
})

test_that("handle_missing imputes when missing >= 5%", {
  # สร้างข้อมูลที่ missing มากกว่า 5%
  df <- data.frame(
    x = c(1, NA, 3, NA, 5, NA, 7, NA, 9, 10),
    y = 1:10
  )
  ana <- missing_analysis(df, plot = FALSE, verbose = FALSE)
  result <- handle_missing(ana, method = "linear", verbose = FALSE)

  expect_equal(result$action, "impute")
  expect_equal(sum(is.na(result$data_clean)), 0)
  expect_true(result$n_imputed > 0)
})

test_that("handle_missing returns imputed_mask with correct dimensions", {
  df <- data.frame(x = c(1, NA, 3, NA, 5, NA, 7, NA, 9, 10), y = 1:10)
  ana <- missing_analysis(df, plot = FALSE, verbose = FALSE)
  result <- handle_missing(ana, method = "linear", verbose = FALSE)

  expect_equal(dim(result$imputed_mask), dim(result$data_clean))
  expect_true(is.logical(as.matrix(result$imputed_mask)))
})
