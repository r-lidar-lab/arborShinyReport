test_that("fit_height_dbh recovers known parameters", {
  set.seed(42)
  dbh <- runif(200, 0.1, 0.8)                      # metres
  h   <- exp(1.1) * (dbh * 100)^0.8 * exp(rnorm(200, sd = 0.03))
  fit <- fit_height_dbh(dbh, h)

  expect_false(is.null(fit))
  expect_equal(fit$b1, 0.8, tolerance = 0.05)
  expect_equal(fit$b0, 1.1, tolerance = 0.15)
  expect_gt(fit$r2, 0.95)
  expect_lt(fit$rmse, 1)
  expect_equal(fit$n, 200)
  expect_equal(fit$fun(0.3), exp(fit$b0) * 30^fit$b1)
})

test_that("fit_height_dbh ignores invalid trees and fails gracefully", {
  expect_null(fit_height_dbh(c(0.2, 0.3), c(10, 12)))
  expect_null(fit_height_dbh(rep(0.3, 10), seq(10, 19)))
  fit <- fit_height_dbh(c(0.1, 0.2, 0.3, 0.4, NA, 0), c(5, 8, 10, 12, 9, 3))
  expect_equal(fit$n, 4)
})
