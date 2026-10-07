#' Fit a height-diameter allometry
#'
#' Fits \eqn{H = \exp(b_0) \cdot (DBH \times 100)^{b_1}} where `DBH` is given in
#' metres (so the allometry is expressed with the diameter in centimetres) and
#' `H` is in metres.
#'
#' The model is fitted by non-linear least squares (`stats::nls()`), starting
#' from the log-log linear regression. If `nls()` does not converge, the
#' back-transformed log-log regression is used instead (`method` reports which).
#' Both goodness-of-fit metrics are computed on the original height scale:
#' `r2 = 1 - SSE/SST` and `rmse = sqrt(mean(residuals^2))`.
#'
#' @param dbh numeric. Diameters at breast height, in **metres**.
#' @param height numeric. Tree heights, in metres.
#'
#' @return `NULL` if fewer than 3 valid trees (finite, strictly positive) or
#'   fewer than 2 distinct diameters are available. Otherwise a list with
#'   `b0`, `b1`, `r2`, `rmse`, `n`, `method`, `range_cm` (range of the fitted
#'   diameters, in cm) and `fun`, a function of `dbh` (in metres) returning the
#'   predicted height.
#' @examples
#' set.seed(1)
#' dbh <- runif(60, 0.1, 0.7)
#' h <- exp(1.2) * (dbh * 100)^0.75 * exp(rnorm(60, sd = 0.05))
#' fit <- fit_height_dbh(dbh, h)
#' fit$b0; fit$b1; fit$r2; fit$rmse
#' fit$fun(0.3)
#' @noRd
fit_height_dbh <- function(dbh, height) {
  ok <- is.finite(dbh) & is.finite(height) & dbh > 0 & height > 0
  d  <- dbh[ok] * 100
  h  <- height[ok]
  n  <- length(h)
  if (n < 3 || length(unique(d)) < 2) return(NULL)

  dat <- data.frame(d = d, h = h)

  # Starting values / fallback: log-log OLS
  ols <- stats::lm(log(h) ~ log(d), data = dat)
  b   <- unname(stats::coef(ols))
  method <- "log-log OLS"

  fit <- tryCatch(
    stats::nls(h ~ exp(b0) * d^b1, data = dat,
               start = list(b0 = b[1], b1 = b[2]),
               control = stats::nls.control(maxiter = 200)),
    error = function(e) NULL)
  if (!is.null(fit)) {
    b <- unname(stats::coef(fit))
    method <- "nls"
  }

  fun  <- function(dbh) exp(b[1]) * (dbh * 100)^b[2]
  pred <- fun(d / 100)
  sse  <- sum((h - pred)^2)
  sst  <- sum((h - mean(h))^2)

  list(b0 = b[1], b1 = b[2],
       r2   = if (sst > 0) 1 - sse / sst else NA_real_,
       rmse = sqrt(sse / n),
       n = n, method = method, range_cm = range(d), fun = fun)
}
