### transformations.R


## internal function
gen.func <- function(y)
{
  period <- as.integer(max(c(autocorr.lags(y), 2L)))

  opt.func <- function(lmb)
  {
    #transform
    # tfm <- BoxCox(y, lmb)
    # rsd <- zoo::rollapply(tfm, period, sd)

    #remove trend
    fit <- try({tslm(rsd, lambda = lmb)}, silent = TRUE)
    if (inherits(fit, "try-error"))
      return(Inf)

    # extract residuals
    resids <- residuals(fit)
    res <- ts(diff(resids, ifelse(period >= length(resids) - 2L, 1L, period)))

    #check lagged autocorrelation
    # lbt <- Box.test(res, lag = period, type = "Ljung-Box")
    return(sd(res, na.rm = TRUE) / mean(res, na.rm = TRUE))
  }
  return(opt.func)
}


## internal function
gen.func2 <- function(y)
{
  # period <- as.integer(max(round(c(frequency(y), autocorr.lags(y), 2L))))

  opt.func <- function(lmb)
  {
    #transform
    tfm <- BoxCox(y, lmb)

    if (any(is.na(tfm)))
      return(Inf)

    v.mean <- mean(tfm)
    v.sd <- sd(tfm)
    v.rat <- v.sd / v.mean

    return(sd(v.rat, na.rm = TRUE) / mean(v.rat, na.rm = TRUE))
  }
  return(opt.func)
}


#' Calculate optimal lambda based on \code{tslm} residuals
#'
#' Identifies lambda for \code{BoxCox} function which mimimzes lag 1 squared
#' autocorrelation of \code{tslm} residuals.
#'
#' @param y univariate time series
#' @param lower numeric. lower bound parameter for optimization search
#' @param upper numeric. upper bound parameter for optimization search
#' @param tol numeric. numerical tolerance for optimization search
#' @param ... additional arguments
#'
#' @return \code{numeric} list, to be passed as `lambda` parameters
#'
#' @importFrom stats lm
#' @importFrom forecast BoxCox.lambda
#'
#' @return numeric. Value to be passed as `lambda` parameter to package functions
#'   which accept `lambda` keyword arguments.
#'
#' @export
#'
#' @importFrom stats optimize lm Box.test residuals as.ts
#' @importFrom forecast BoxCox
#' @importFrom zoo rollapply
#'
#' @examples
#' library(ggplot2)
#' library(forecastR)
#' data("AirPassengers", package = "datasets")
#'
#' y <- ts.split(AirPassengers)
#'
#' (lmb <- optimize.lambda(y$in.sample))
#' a.std <- arima(y$in.sample)
#' a.lambda <- arima(y$in.sample, lambda = lmb)
#'
#' f.std <- forecast(a.std, h = length(y$out.of.sample))
#' f.lambda <- forecast(a.lambda, h = length(y$out.of.sample))
#'
#' vals <- cbind(actuals = y$out.of.sample, std = f.std$mean, lambda = f.lambda$mean)
#'
#' ## suppressWarnings(autoplot(vals))
#'
#' fcst.err <- cbind(std = f.std$mean, lambda = f.lambda$mean) - y$out.of.sample
#' ## autoplot(fcst.err)
optimize.lambda <- function(y, lower = ifelse(any(y < 0.00001), 0.01, -1.0),
                            upper = 2.0, tol = 0.001, ...)
{
  ll <- try({forecast::BoxCox.lambda(y, method = "loglik", lower = lower, upper = upper)}, silent = TRUE)
  g <- try({forecast::BoxCox.lambda(y, method = "guerrero", lower = lower, upper = upper)}, silent = TRUE)

  if (inherits(g, "try-error"))
    g <- 1.0

  if (inherits(ll, "try-error"))
    ll <- g

  # opt.func <- gen.func(y)
  # outputs <- suppressWarnings(optimize(opt.func, c(min(c(ll, g)) - 0.5, max(ll, g) + 0.50),
  #                                      maximum = FALSE, tol = tol))
  return(max(c(ll, g)))
}

