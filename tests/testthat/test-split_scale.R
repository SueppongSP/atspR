test_that("split_data splits correctly at 80/20", {
  df <- data.frame(x = 1:100, y = rnorm(100))
  result <- split_data(df, train_ratio = 0.8, verbose = FALSE)

  expect_equal(nrow(result$train), 80)
  expect_equal(nrow(result$test), 20)
  expect_equal(result$train_idx, 1:80)
  expect_equal(result$test_idx, 81:100)
})

test_that("split_data preserves temporal order", {
  df <- data.frame(x = 1:10)
  result <- split_data(df, train_ratio = 0.7, verbose = FALSE)

  # train ต้องมาก่อน test เสมอ
  expect_true(max(result$train_idx) < min(result$test_idx))
})

test_that("split_data errors on invalid train_ratio", {
  df <- data.frame(x = 1:10)
  expect_error(split_data(df, train_ratio = 0,   verbose = FALSE))
  expect_error(split_data(df, train_ratio = 1,   verbose = FALSE))
  expect_error(split_data(df, train_ratio = 1.5, verbose = FALSE))
})

# ── scale_data ────────────────────────────────────────────────────────────────

test_that("scale_data minmax produces values in [0, 1]", {
  df <- data.frame(x = 1:100, y = rnorm(100))
  sp <- split_data(df, verbose = FALSE)
  sc <- scale_data(sp, method = "minmax", verbose = FALSE)

  expect_true(all(sc$train_scaled$x >= 0 & sc$train_scaled$x <= 1))
  expect_true(all(sc$train_scaled$y >= 0 & sc$train_scaled$y <= 1))
})

test_that("scale_data zscore produces mean~0 and sd~1 on train", {
  set.seed(42)
  df <- data.frame(x = rnorm(100, mean = 50, sd = 10))
  sp <- split_data(df, verbose = FALSE)
  sc <- scale_data(sp, method = "zscore", verbose = FALSE)

  expect_lt(abs(mean(sc$train_scaled$x)), 1e-10)
  expect_lt(abs(sd(sc$train_scaled$x) - 1), 1e-10)
})

test_that("scale_data fits on train only — no leakage", {
  df <- data.frame(x = 1:100)
  sp <- split_data(df, verbose = FALSE)
  sc <- scale_data(sp, method = "minmax", verbose = FALSE)

  # params ต้องมาจาก train เท่านั้น
  expect_equal(sc$params$x$min, min(sp$train$x))
  expect_equal(sc$params$x$max, max(sp$train$x))
})

test_that("inverse_scale recovers original values", {
  set.seed(1)
  df <- data.frame(x = rnorm(100), y = rnorm(100, 10, 2))
  sp <- split_data(df, verbose = FALSE)
  sc <- scale_data(sp, method = "minmax", verbose = FALSE)

  recovered <- inverse_scale(sc$train_scaled, sc)
  expect_equal(round(recovered$x, 8), round(sp$train$x, 8))
})
