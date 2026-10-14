test_that("fit_height_dbh recovers known parameters", {
  set.seed(42)
  dbh <- runif(200, 0.1, 0.8)                      # metres
  h   <- exp(1.1) * (dbh * 100)^0.8 * exp(rnorm(200, sd = 0.03))
  fit <- fit_height_dbh(dbh, h)

  expect_false(is.null(fit))
  expect_equal(fit$b, 0.8, tolerance = 0.05)
  expect_equal(fit$a, exp(1.1), tolerance = 0.15)
  expect_gt(fit$r2, 0.95)
  expect_lt(fit$rmse, 1)
  expect_equal(fit$n, 200)
  expect_equal(fit$fun(0.3), fit$a * 30^fit$b)
})

test_that("fit_height_dbh ignores invalid trees and fails gracefully", {
  expect_null(fit_height_dbh(c(0.2, 0.3), c(10, 12)))
  expect_null(fit_height_dbh(rep(0.3, 10), seq(10, 19)))
  fit <- fit_height_dbh(c(0.1, 0.2, 0.3, 0.4, NA, 0), c(5, 8, 10, 12, 9, 3))
  expect_equal(fit$n, 4)
})

test_that("fit_volume_dbh recovers known parameters", {
  set.seed(42)
  dbh <- runif(200, 0.1, 0.8)
  v   <- 2e-4 * (dbh * 100)^2.4 * exp(rnorm(200, sd = 0.03))
  fit <- fit_volume_dbh(dbh, v)

  expect_equal(fit$b, 2.4, tolerance = 0.05)
  expect_equal(fit$a, 2e-4, tolerance = 0.2)
  expect_gt(fit$r2, 0.95)
  expect_equal(fit$fun(0.3), fit$a * 30^fit$b)
  expect_null(fit_volume_dbh(c(0.2, 0.3), c(1, 2)))
})
