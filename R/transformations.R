### transformations.R


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
#' suppressWarnings(autoplot(vals))
#'
#' fcst.err <- cbind(std = f.std$mean, lambda = f.lambda$mean) - y$out.of.sample
#' autoplot(fcst.err)
optimize.lambda <- function(y, lower = -1.0, upper = 2.0, tol = 0.001, ...)
{
  gen.func <- function(y)
  {
    y.freq <- autocorr.lags(y)
    opt.func <- function(lmb)
    {
      #transform
      tfm <- BoxCox(y, lmb)
      #remove trend
      fit <- lm(tfm ~ time(tfm))
      #extract seasonality
      resids <- residuals(fit)
      #difference seasonality
      res <- as.ts(diff(resids, y.freq))
      #check lagged autocorrelation
      lbt <- Box.test(res, lag = y.freq, type = "Ljung-Box")
      return(lbt$p.value)
    }
    return(opt.func)
  }
  opt.func <- gen.func(y)
  outputs <- suppressWarnings(optimize(opt.func, c(lower, upper),
                                       maximum = FALSE, tol = tol))
  return(outputs$minimum)
}



#predecessor function, not exported
# .identify.lambdas <- function(y)
# {
#   outputs <- 1
#   lower <- 0.01
#   if (all(y > 0.0))
#   {
#     outputs <- c(outputs, 0)
#     lower <- -1.0
#   }
#   if (2 * frequency(y) < length(y))
#   {
#     outputs <- c(outputs, forecast::BoxCox.lambda(y, method="guerrero", lower=lower),
#                  BoxCox.lambda(y, method="loglik", lower=lower))
#   }
#   return(sort(unique(outputs)))
# }


