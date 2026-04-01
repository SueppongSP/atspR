test_that("combine_datetime with time_type='hour' produces POSIXct", {
  df <- data.frame(
    date = c("2024-01-01", "2024-01-01", "2024-01-01"),
    hour = c(8L, 9L, 10L),
    val  = c(1, 2, 3)
  )
  result <- combine_datetime(df, date_col = "date", time_col = "hour",
                             time_type = "hour", verbose = FALSE)
  expect_true(inherits(result$datetime, "POSIXct"))
  expect_equal(format(result$datetime[1], "%H:%M"), "08:00")
})

test_that("combine_datetime with time_type='string' parses HH:MM", {
  df <- data.frame(
    date = c("2024-01-01", "2024-01-01"),
    time = c("08:30", "14:00"),
    val  = c(1, 2)
  )
  result <- combine_datetime(df, date_col = "date", time_col = "time",
                             time_type = "string", verbose = FALSE)
  expect_true(inherits(result$datetime, "POSIXct"))
  expect_equal(format(result$datetime[1], "%H:%M"), "08:30")
})

test_that("combine_datetime with time_type='hhmm' parses 830 as 08:30", {
  df <- data.frame(
    date = c("2024-01-01", "2024-01-01"),
    time = c(830L, 1430L),
    val  = c(1, 2)
  )
  result <- combine_datetime(df, date_col = "date", time_col = "time",
                             time_type = "hhmm", verbose = FALSE)
  expect_equal(format(result$datetime[1], "%H:%M"), "08:30")
  expect_equal(format(result$datetime[2], "%H:%M"), "14:30")
})

test_that("combine_datetime drops source columns when drop_cols=TRUE", {
  df <- data.frame(date = "2024-01-01", hour = 8L, val = 1)
  result <- combine_datetime(df, date_col = "date", time_col = "hour",
                             time_type = "hour", drop_cols = TRUE, verbose = FALSE)
  expect_false("date" %in% names(result))
  expect_false("hour" %in% names(result))
  expect_true("datetime" %in% names(result))
})

test_that("combine_datetime keeps source columns when drop_cols=FALSE", {
  df <- data.frame(date = "2024-01-01", hour = 8L, val = 1)
  result <- combine_datetime(df, date_col = "date", time_col = "hour",
                             time_type = "hour", drop_cols = FALSE, verbose = FALSE)
  expect_true("date" %in% names(result))
  expect_true("hour" %in% names(result))
})

test_that("combine_datetime errors on missing date_col", {
  df <- data.frame(x = "2024-01-01", hour = 8L)
  expect_error(combine_datetime(df, date_col = "date", time_col = "hour",
                                time_type = "hour", verbose = FALSE))
})
