## ts_model_ets.R


#' ETS model fitting and updates
#'
#' Wrapper for forecast package functions \code{\link[forecast]{ets}} to
#' allow unified interface.
#'
#' @param y Univariate Time Series
#' @param model either a string identifying model method, or previously fitted
#'   model object. See \code{\link[forecast]{ets}} for more information
#' @param allow.multiplicative.trend logical. should the model consider
#'   multiplicative exponential smoothing models?
#' @param lambda optional, if null ignored otherwise treated as the Box-Cox
#'   parameter
#' @param ... additional argument for model function
#'
#' @return \code{tsm} object
#'
#' @seealso \code{\link[forecast]{ets}}
#'
#' @importFrom forecast ets
#' @importFrom stats is.ts
#'
#' @family Time Series Models
#'
#' @export
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#' fit <- ets(AirPassengers)
#' fit2 <- ets(AirPassengers, lambda = 0)
ets <- function(y, model = "ZZZ", allow.multiplicative.trend = FALSE, lambda = NULL, ...)
{
  model.name <- "ets"
  kw <- list(...)
  if(!is.null(lambda) && is.na(lambda))
    lambda <- NULL

  y.ts <- short.ts(y, freq.multiple = 2, lambda = lambda)
  y <- y.ts$y
  if (is.character(model))
  {
    fit <- .ets_initial_fit(y)
  } else
  {
    fit <- try({forecast::ets(y, model = model(model), ...)}, silent=TRUE)
    if (inherits(fit, "try-error"))
      fit <- .ets_initial_fit(y)
  }

  for (nm in package_options("ts.fields")[[model.name]])
  {
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]), nm)
  }

  output <- tsm(model.name, fit)
  return(output)
}


#' Internal function to determine fit based on small out of sample validation
#'
#' Performs a cross validation against a train / test subeet of the original
#' time series, selecting the model which minimizes mean absolute error.
#'
#' @param y univariate time series
#' @param split numeric. If between < 1, a fraction of samples. If >= 1 a fixed
#'  number of observations
#'
#' @importFrom forecast ets accuracy forecast
.ets_initial_fit <- function(y, split = 0.20)
{
  y.orig <- y
  opt.lmb <- optimize.lambda(y)
  y <- ts.split(y, split = split)
  oos.len <- length(y$out.of.sample)

  fits <- list(
    base = forecast::ets(y$in.sample)
  )

  base.damped <- try({forecast::ets(y$in.sample, damped=TRUE)}, silent=TRUE)
  if (!inherits(base.damped, "try-error"))
    fits$base.damped <- base.damped

  base.lambda = try({forecast::ets(y$in.sample, lambda = opt.lmb)}, silent = TRUE)
  if (!inherits(base.lambda, "try-error"))
    fits$base.lambda <- base.lambda

  base.damped.lambda = try({forecast::ets(y$in.sample, damped = TRUE, lambda = opt.lmb)}, silent = TRUE)
  if (!inherits(base.damped.lambda, "try-error"))
    fits$base.damped.lambda <- base.lambda

  fcst.accuracy <- sapply(fits, function(x) {
    fcst <- forecast::forecast(x, h = oos.len)
    vals <- cbind(as.matrix(fcst$mean, ncol=1), fcst$lower, fcst$upper)
    if (any(is.na(vals) | is.null(vals) | is.infinite(vals)))
    {
      return(Inf)
    } else
    {
      acc <- forecast::accuracy(fcst, y$out.of.sample)
      return(acc['Test set', 'MAE'])
    }
  })

  bf.idx <- which(fcst.accuracy == (min(fcst.accuracy)))
  best.fit <- fits[[bf.idx[1]]]
  y <- y.orig
  fit <- try({forecast::ets(y, model = best.fit)}, silent=TRUE)
  if (inherits(fit, "try-error"))
    fit <- forecast::ets(y)

  return(fit)
}
