test_that("fill_time_gaps inserts missing hourly timestamps", {
  df <- data.frame(
    datetime = as.POSIXct(c("2024-01-01 08:00:00",
                             "2024-01-01 09:00:00",
                             "2024-01-01 11:00:00"), tz = "UTC"),
    temp  = c(25.1, 25.4, 26.2),
    humid = c(80, NA, 85)
  )
  result <- fill_time_gaps(df, time_col = "datetime", n = 1,
                           unit = "hour", verbose = FALSE)
  expect_equal(result$n_gaps, 1)
  expect_equal(nrow(result$data), 4)
  expect_true(as.POSIXct("2024-01-01 10:00:00", tz = "UTC") %in% result$data$datetime)
})

test_that("fill_time_gaps returns 0 gaps when timestamps are complete", {
  df <- data.frame(
    datetime = as.POSIXct(c("2024-01-01 08:00:00",
                             "2024-01-01 09:00:00",
                             "2024-01-01 10:00:00"), tz = "UTC"),
    temp = c(25.1, 25.4, 26.2)
  )
  result <- fill_time_gaps(df, time_col = "datetime", n = 1,
                           unit = "hour", verbose = FALSE)
  expect_equal(result$n_gaps, 0)
  expect_equal(nrow(result$data), 3)
})

test_that("fill_time_gaps handles daily gaps", {
  df <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-04")),
    val  = c(1, 2, 4)
  )
  result <- fill_time_gaps(df, time_col = "date", n = 1,
                           unit = "day", verbose = FALSE)
  expect_equal(result$n_gaps, 1)
  expect_true(as.Date("2024-01-03") %in% result$data$date)
})

test_that("fill_time_gaps preserves character column types", {
  df <- data.frame(
    datetime = as.POSIXct(c("2024-01-01 08:00:00",
                             "2024-01-01 10:00:00"), tz = "UTC"),
    label = c("A", "B"),
    val   = c(1.0, 2.0),
    stringsAsFactors = FALSE
  )
  result <- fill_time_gaps(df, time_col = "datetime", n = 1,
                           unit = "hour", verbose = FALSE)
  expect_true(is.character(result$data$label))
  expect_true(is.numeric(result$data$val))
})

test_that("fill_time_gaps errors on non-datetime column", {
  df <- data.frame(datetime = c("2024-01-01", "2024-01-02"), val = c(1, 2),
                   stringsAsFactors = FALSE)
  expect_error(
    fill_time_gaps(df, time_col = "datetime", n = 1, unit = "day", verbose = FALSE)
  )
})
