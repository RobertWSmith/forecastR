## model_fit.R

#' Generic Time Series Model Fit for Package Models
#'
#' @param y \code{ts}. Univariate Time Series
#' @param ts.model.type character. Name of model function from package.
#' @param ... additional keyword arguments provided to `ts.model.type` function.
#'
#' @importFrom stats as.ts
#'
#' @return \code{tsm} object, with subspecialization determined by `ts.model.type`
#' argument.
#'
#' @seealso \code{\link[forecastR]{arima}} \code{\link[forecastR]{arfima}}
#'   \code{\link[forecastR]{bats}} \code{\link[forecastR]{ets}}
#'   \code{\link[forecastR]{nnetar}} \code{\link[forecastR]{tbats}}
#'   \code{\link[forecast]{auto.arima}} \code{\link[forecast]{Arima}}
#'   \code{\link[forecast]{arfima}} \code{\link[forecast]{bats}}
#'   \code{\link[forecast]{ets}} \code{\link[forecast]{nnetar}}
#'   \code{\link[forecast]{tbats}}
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#'
#' ap.split <- ts.split(AirPassengers)
#'
#' mf <- ts.model.fit(ap.split$in.sample, ts.model.type = 'arima')
#' mf.fcst <- forecast(mf, length(ap.split$out.of.sample))
#' summary(mf)
#' coef(mf)
#'
#' ## autoplot(resid(mf))
#'
#' ## vals <- window(cbind(Data = ap.split$data, Forecast.Mean = mf.fcst$mean),
#' ##   start = c(1958,1))
#' ## autoplot(vals)
ts.model.fit <- function(y, ts.model.type = c("arfima", "arima", "bats", "ets",
                                              "nnetar", "stlm", "tbats", "tslm"),
  ...)
  {
  ts.model.type <- match.arg(ts.model.type)
  kw <- list(...)

  kw$y <- quote(y)
  mdl <- try({do.call(ts.model.type, kw)}, silent=TRUE)

  if (inherits(mdl, "try-error"))
  {
    y <- forecast::tsclean(y, lambda = kw$lambda)
    kw$y <- quote(y)
    mdl <- do.call(ts.model.type, kw)
  }

  return(mdl)
}




#' Update Generic Time Series Model Fit for Package Models
#'
#' @describeIn ts.model.fit Update Time Series Model
#'
#' @param model \code{tsm} object. Model which was fit in call to
#'   \code{\link[forecastR]{ts.model.fit}}
#'
#' @importFrom stats as.ts
#'
#' @return \code{tsm} object, with subspecialization determined by `ts.model.type`
#' argument.
#'
#' @seealso \code{\link[forecastR]{ts.model.fit}}
#'
#' @export
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#'
#' ap.split <- ts.split(AirPassengers)
#'
#' mf <- ts.model.autofit(ap.split$in.sample, ts.model.type = 'arima', return.all.models = FALSE)
#' mf <- ts.model.refit(ap.split$in.sample, mf)
#' mf.fcst <- forecast(mf, length(ap.split$out.of.sample))
#' print(mf.fcst)
#'
#' vals <- cbind(Data = ap.split$data, Forecast.Mean = mf.fcst$mean)
#' ## suppressWarnings(plot(window(vals, start = c(1958,1)), plot.type="single", col=1:ncol(vals)))
#'
#' mf2 <- ts.model.refit(ap.split$data, mf)
#' mf2.fcst <- forecast(mf2, 24)
#' summary(mf2)
#'
#' vals <- cbind(Data = ap.split$data, In.Sample.Fcst = mf.fcst$mean,
#'               Out.Of.Sample.Fcst = mf2.fcst$mean)
#' ## suppressWarnings(plot(window(vals, start = c(1958,1)), plot.type="single", col=1:ncol(vals)))
ts.model.refit <- function(y, model, ...)
{
  y.orig <- y
  kw <- list(...)

  mdl <- model(model)

  kw$y <- quote(y)

  kw$model <- mdl
  if (!is.character(mdl))
    kw$model <- quote(mdl)

  ts.model.type <- model$function.name
  if (is.null(ts.model.type))
    ts.model.type <- model$fit$function.name

  mdl <- try({do.call(ts.model.type, kw)}, silent=TRUE)

  if (inherits(mdl, "try-error"))
  {
    y <- forecast::tsclean(y, lambda = kw$lambda)
    kw$y <- quote(y)
    mdl <- do.call(ts.model.type, kw)
  }

  return(mdl)
}



#' Autofit Time Series models attempting multiple standardized transformations
#'
#' @describeIn ts.model.fit Autofit Time Series Model
#'
#' @param lambda list of values to pass to \code{lambda} values
#' @param alpha numeric. significance threshold for \code{\link[forecast]{dm.test}}
#'   which determines if transformation makes for an improved model
#' @param return.all.models logical. controls if only best in sample fit is returned
#'   or if all models are returned
#' @param split numeric. parameter passed to \code{\link[forecastR]{ts.split}}
#'
#' @importFrom stats AIC
#' @importFrom forecast tsclean
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#'
#' ap.split <- ts.split(AirPassengers)
#'
#' mf <- mf.fcst <- list()
#'
#' mf <- ts.model.autofit(ap.split$in.sample, ts.model.type = 'arima')
#'
#' for (i in 1:length(mf))
#' {
#'   print(i)
#'   mdl <- mf[[i]]
#'   print(mdl)
#' }
#'
#' vals <- cbind(
#'    in.sample = ap.split$in.sample,
#'    out.of.sample = ap.split$out.of.sample,
#'    forecast = mf$forecast$mean
#'    )
#'
#' cn <- c("in.sample", "out.of.sample", "forecast")
#' colnames(vals) <- cn
#' head(cn)
#' ## plot(cn, col=1:ncol(cn))
#'
#' errs <- mf$forecast$mean - ap.split$out.of.sample
#' print(errs)
#' ## plot(errs)
ts.model.autofit <- function(y, lambda = NULL, alpha = 0.05,
                             split = 0.20, return.all.models = FALSE,
                             ts.model.type = c("arfima", "arima", "bats", "ets",
                                               "nnetar", "stlm", "tbats", "tslm"),
                             ...)
{
  #internal function to standardize output
  .fcst.acc <- function(fit, y.oos, is.short)
  {
    if (inherits(fit, "try-error"))
    {
      fit <- ts.model.fit(forecast::tsclean(y), ts.model.type = ts.model.type, lambda = NULL)
    }
    if (is.short)
    {
      fcst <- try({forecast(fit)}, silent=TRUE)
      if (inherits(fcst, "try-error"))
      {
        fit <- ts.model.fit(y, ts.model.type = ts.model.type, lambda = NULL)
        fcst <- forecast(fit, h = length(y.oos))
      }
      acc <- forecast::accuracy(fcst)
    } else
    {
      fcst <- try({forecast(fit, h = length(y.oos))}, silent=TRUE)
      if (inherits(fcst, "try-error"))
      {
        fit <- ts.model.fit(y, ts.model.type = ts.model.type, lambda = NULL)
        fcst <- forecast(fit, h = length(y.oos))
      }
      tsp(fcst$mean) <- tsp(y.oos)
      acc <- forecast::accuracy(fcst, y.oos)
    }
    outputs <- list(fit = fit, forecast = fcst, accuracy = acc)
    outputs <- structure(outputs, class = "tsm.autofit")
    return(outputs)
  }

  #makes sure values is comprable to a number
  .acc.avail <- function(x)
  {
    return(!(is.na(x) || is.nan(x) || is.null(x) || is.infinite(x)))
  }

  is.short <- TRUE
  ts.model.type <- match.arg(ts.model.type)
  y.orig <- y
  y.oos <- NULL

  if (!is.null(split))
  {
    y.split <- ts.split(y, split = split)
    # overwrite is.short w/ split value if available
    is.short <- attr(y.split, "is.short") || is.null(attr(y.split, "is.short"))
  }

  if (!is.short)
  {
    y <- y.split$in.sample
    y.oos <- y.split$out.of.sample
  }

  if (is.short && (ts.model.type == "stlm"))
  {
    # if we have a short time series, silently return NULL when user attempts to model
    message("`stlm` models require at least frequency * 2 units of history. No model returned.")
    return(NULL)
  }

  # BATS / TBATS models are already able to auto-fit to best in-sample AIC
  if (ts.model.type %in% c("bats", "tbats"))
  {
    fit <- ts.model.fit(y, ts.model.type = ts.model.type, ...)
    return(.fcst.acc(fit, y.oos, is.short=is.short))
  }

  if (length(lambda) == 1L || is.null(lambda))
  {
    fit <- try({ts.model.fit(y, ts.model.type = ts.model.type, lambda = lambda, ...)}, silent = TRUE)
    return(.fcst.acc(fit, y.oos, is.short=is.short))
  }

  # loop over lambdas, fitting each model
  outputs <- lapply(lambda, function(tst.lambda) {
    fit <- try({ts.model.fit(y, ts.model.type = ts.model.type, lambda = tst.lambda, ...)}, silent = TRUE)
    return(.fcst.acc(fit, y.oos, is.short=is.short))
  })

  # no need to check for in-sample best if we want all models returned
  if (return.all.models || length(lambda) < 2)
    return(outputs)

  # seed with first model, then loop over the others comparing each time
  best.idx <- 1L
  best.acc <- outputs[[best.idx]]$accuracy

  acc.row <- ifelse(is.short, "Training set", "Test set")
  stat.rankings <- c("MASE", "MAPE", "MPE", "MAE", "RMSE", "ME")

  for (i in (best.idx+1L):length(outputs))
  {
    #ensure variables are cleared between iterations
    test.acc <- NULL
    test.acc <- outputs[[i]]$accuracy
    for (rnk in stat.rankings)
    {
      best.avail <- .acc.avail(best.acc[acc.row, rnk])
      test.avail <- .acc.avail(test.acc[acc.row, rnk])

      ## have data from both and best is better than test
      ## OR best is available and test is not
      ## don't change anything
      if ((best.avail & test.avail) &&
              (test.acc[acc.row, rnk]^2 >= best.acc[acc.row, rnk]^2))
      {
        break
      } else if (best.avail & !test.avail)
      {
        break
      } else {
        ## `i` is the index iterator, `rnk` is the stat rankings iterator
        best.idx <- i
        best.acc <- test.acc
        # break upon first comparison (inner for loop over stat rankings)
        break
      }
    }
  }
  return(outputs[[best.idx]])
}
