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
  mdl <- do.call(ts.model.type, kw)

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

  func.name <- model$function.name
  if (is.null(func.name))
    func.name <- model$fit$function.name

  mdl <- do.call(func.name, kw)
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
ts.model.autofit <- function(y, lambda = optimize.lambda(y), alpha = 0.05, split = 0.20,
                             return.all.models = FALSE,
                             ts.model.type = c("arfima", "arima", "bats", "ets",
                                               "nnetar", "stlm", "tbats", "tslm"),
                             ...)
{
  #internal function to standardize output
  .fcst.acc <- function(fit, y.oos, is.short)
  {
    if (is.short)
    {
      fcst <- forecast(fit)
      acc <- forecast::accuracy(fcst)
    } else
    {
      fcst <- forecast(fit, h = length(y.oos))
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
    return(!(is.na(x) || is.nan(x) || is.null(x)))
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

  if (length(lambda) == 1L)
  {
    fit <- ts.model.fit(y, ts.model.type = ts.model.type, lambda = lambda, ...)
    return(.fcst.acc(fit, y.oos, is.short=is.short))
  }

  # loop over lambdas, fitting each model
  outputs <- lapply(lambda, function(tst.lambda) {
    fit <- ts.model.fit(y, ts.model.type = ts.model.type, lambda = tst.lambda, ...)
    return(.fcst.acc(fit, y.oos, is.short=is.short))
  })

  # no need to check for in-sample best if we want all models returned
  if (return.all.models)
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


### internal function, baseline forecasts is used inside
### `.parse.forecasts` @param data univariate time series
#  #' @importFrom stats fitted residuals
# .baseline.forecasts <- function(data, h = 18L, return.forecasts = FALSE)
# {
#   ret <- list(names = c("naive", "meanf", "thetaf"), data = data)
#   naive.fcst <- forecast::naive(data, h = h)
#   mean.fcst <- forecast::meanf(data, h = h)
#   theta.fcst <- forecast::thetaf(data, h = h)
#   if (return.forecasts)
#     ret$objects <- list(naive.fcst = naive.fcst, mean.fcst = mean.fcst,
#       theta.fcst = theta.fcst)
#   ret$fcst.mean <- cbind(naive.fcst$mean, mean.fcst$mean, theta.fcst$mean)
#   ret$fcst.resid <- cbind(residuals(naive.fcst), residuals(mean.fcst),
#     residuals(theta.fcst))
#   ret$fcst.fitted <- cbind(fitted(naive.fcst), fitted(mean.fcst),
#     fitted(theta.fcst))
#   colnames(ret$fcst.mean) <- ret$names
#   colnames(ret$fcst.resid) <- ret$names
#   colnames(ret$fcst.fitted) <- ret$names
#   return(ret)
# }


### internal function, run `.baseline.forecasts`, then add
### forecasts from the model list @param model.list
### \code{list} of \code{tsm} fits @param data univariate
### time series, provided to `.baseline.forecasts` @param
### refit.models logical. Default \code{FALSE}, if
### \code{TRUE} the model object is refit with the time series
### provided @param h integer. number of steps to forecast
### @param return.forecasts logical. if TRUE, field `objects`
### is present in the returned list, with a named list by model
### of the returned forecast objects
# .model.forecasts <- function(model.list, data, refit.models = FALSE,
#   h = 18L, return.forecasts = TRUE)
#   {
#   ret <- .baseline.forecasts(data, h = h, return.forecasts)
#   fcst.objects <- bf$objects
#   mdl.names <- names(model.list)
#   ret.names <- ret$names
#   for (i in 1:length(mdl.names))
#   {
#     nm <- mdl.names[i]
#     ret.names[length(ret.names) + 1] <- nm
#     mdl <- model.list[[i]]
#     if (refit.models)
#       mdl <- do.call(nm, list(y = data, model = model(mdl)))
#     fcst <- forecast(mdl, h = h)
#     ret$fcst.mean <- cbind(ret$fcst.mean, fcst$mean)
#     ret$fcst.resid <- cbind(ret$fcst.resid, residuals(fcst))
#     ret$fcst.fitted <- cbind(ret$fcst.resid, fitted(fcst))
#     if (return.forecasts)
#       fcst.objects[[nm]] <- fcst
#   }
#   ret$names <- ret.names
#   ret$objects <- fcst.objects
#   colnames(ret$fcst.mean) <- ret$names
#   colnames(ret$fcst.resid) <- ret$names
#   colnames(ret$fcst.fitted) <- ret$names
#   return(ret)
# }


### internal function, parses model lists into list of useful
### values
#  #' @importFrom stats fitted residuals
# .parse.forecasts <- function(is.model.list, y.split, oos.h = 18L,
#   return.models = TRUE)
#   {
#   data <- y.split$data
#   is <- y.split$in.sample
#   oos <- y.split$out.of.sample
#   output <- .baseline.forecasts(is, length(oos), oos.fits)
#   output$names <- c(output$names, mdl.names)
#   mdl.names <- names(is.model.list)
#   for (i in mdl.names)
#   {
#     mdl <- is.model.list[[i]]
#     is.fit <- mdl$in.sample
#     is.fcst <- forecast(is.fit, h = output$train.len)
#     output$train.resid <- cbind(output$train.resid, residuals(is.fcst))
#     if (oos.fits)
#     {
#       oos.fit <- ts.model.refit(data, is.fit)
#       oos.fcst <- forecast(oos.fit, h = oos.h)
#       output$train.experts <- cbind(output$train.experts,
#         is.fcst$mean)
#       output$oos.experts <- cbind(output$oos.experts, oos.fcst$mean)
#       output$oos.resid <- cbind(output$oos.resid, residuals(oos.fcst))
#     } else
#     {
#       output$train.experts <- cbind(output$train.experts,
#         fitted(is.fcst))
#     }
#   }
#   if (oos.fits)
#   {
#     colnames(output$oos.experts) <- output$names
#     colnames(output$train.resid) <- output$names
#     colnames(output$oos.resid) <- output$names
#     output$is.error <- (output$train.experts - oos)
#     colnames(output$is.error) <- output$names
#     output$is.abs.error <- abs(output$is.error)
#     colnames(output$is.abs.error) <- output$names
#   }
#   colnames(output$train.experts) <- output$names
#   base <- .parse.forecasts(models, y.split = y.split, oos.h = oos.h,
#     oos.fits = short.ts)
#   output$models <- is.model.list
#   return(structure(output, class = "tsm.multi"))
# }


