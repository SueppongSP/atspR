test_that("cross_validate returns correct structure", {
  data(airquality)
  clean <- airquality[complete.cases(airquality), ]
  sp    <- split_data(clean, verbose = FALSE)
  sc    <- scale_data(sp, verbose = FALSE)
  cv    <- cross_validate(sc, target_col = "Ozone", k = 3L, verbose = FALSE)

  expect_named(cv, c("fold_results", "summary", "k", "k_evaluated",
                     "min_train_size", "target_col"))
  expect_equal(cv$k, 3L)
  expect_equal(cv$target_col, "Ozone")
  expect_true(is.data.frame(cv$fold_results))
  expect_true(is.data.frame(cv$summary))
})

test_that("cross_validate evaluates all k folds with min_train_size", {
  data(airquality)
  clean <- airquality[complete.cases(airquality), ]
  sp    <- split_data(clean, verbose = FALSE)
  sc    <- scale_data(sp, verbose = FALSE)
  cv    <- cross_validate(sc, target_col = "Ozone", k = 4L,
                          min_train_size = 0.2, verbose = FALSE)

  # ด้วย min_train_size ทุก fold ควรมีค่า ไม่มี skip
  expect_equal(cv$k_evaluated, 4L)
})

test_that("cross_validate summary has correct metric columns", {
  data(airquality)
  clean <- airquality[complete.cases(airquality), ]
  sp    <- split_data(clean, verbose = FALSE)
  sc    <- scale_data(sp, verbose = FALSE)
  cv    <- cross_validate(sc, target_col = "Ozone", k = 3L, verbose = FALSE)

  expect_true(all(c("RMSE", "MAE", "R2") %in% cv$summary$metric))
  expect_true(all(c("mean", "sd", "min", "max") %in% names(cv$summary)))
})

test_that("cross_validate train always precedes val (walk-forward)", {
  data(airquality)
  clean <- airquality[complete.cases(airquality), ]
  sp    <- split_data(clean, verbose = FALSE)
  sc    <- scale_data(sp, verbose = FALSE)
  cv    <- cross_validate(sc, target_col = "Ozone", k = 4L,
                          min_train_size = 0.2, verbose = FALSE)

  # n_train ต้องเพิ่มขึ้นในแต่ละ fold
  n_train_vals <- cv$fold_results$n_train
  expect_true(all(diff(n_train_vals) > 0))
})

test_that("cross_validate errors on missing target_col", {
  data(airquality)
  clean <- airquality[complete.cases(airquality), ]
  sp    <- split_data(clean, verbose = FALSE)
  sc    <- scale_data(sp, verbose = FALSE)

  expect_error(cross_validate(sc, target_col = "NonExistent", verbose = FALSE))
})

test_that("cross_validate accepts custom model_fn", {
  data(airquality)
  clean <- airquality[complete.cases(airquality), ]
  sp    <- split_data(clean, verbose = FALSE)
  sc    <- scale_data(sp, verbose = FALSE)

  my_fn <- function(train_fold, val_fold) {
    preds   <- rep(mean(train_fold$Ozone, na.rm = TRUE), nrow(val_fold))
    actuals <- val_fold$Ozone
    resid   <- actuals - preds
    c(RMSE = sqrt(mean(resid^2, na.rm = TRUE)),
      MAE  = mean(abs(resid), na.rm = TRUE))
  }

  cv <- cross_validate(sc, target_col = "Ozone", model_fn = my_fn,
                       k = 3L, verbose = FALSE)
  expect_true(all(c("RMSE", "MAE") %in% cv$summary$metric))
})
