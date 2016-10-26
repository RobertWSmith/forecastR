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
#' autoplot(resid(mf))
#'
#' vals <- cbind(Data = ap.split$data, Forecast.Mean = mf.fcst$mean)
#' autoplot(window(vals, start = c(1958,1)), na.rm=TRUE)
ts.model.fit <- function(y, ts.model.type = c("arfima", "arima", "bats", "ets",
                                              "nnetar", "stlm", "tbats", "tslm"),
  ...)
  {
  ts.model.type <- match.arg(ts.model.type)
  kw <- list(...)
  y <- as.ts(y)
  kw$y <- quote(y)
  return(do.call(ts.model.type, kw))
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
#'   print(mf[[i]])
#'   mf.fcst[[i]] <- forecast(mf[[i]], length(ap.split$out.of.sample))
#' }
#'
#' vals <- do.call(cbind, lapply(mf.fcst, function(x) { x$mean }))
#' errs <- vals - ap.split$out.of.sample
#'
#' vals <- cbind(vals, ap.split$data)
#' colnames(vals) <- c(1:length(mf.fcst), "Data")
#' autoplot(window(vals, start = c(1958,1)), na.rm=TRUE)
#'
#' colnames(errs) <- 1:4
#' autoplot(errs)
#'
#' abs.errs <- abs(errs)
#' autoplot(abs.errs)
#'
#' cum.errs <- as.ts(apply(errs, 2, cumsum))
#' tsp(cum.errs) <- tsp(cum.errs)
#' autoplot(cum.errs)
ts.model.autofit <- function(y, ts.model.type = c("arfima", "arima", "bats",
                                                  "ets", "nnetar", "stlm",
                                                  "tbats", "tslm"),
                             lambda = 0,
                             alpha = 0.05, return.all.models = TRUE, ...)
{
  ts.model.type <- match.arg(ts.model.type)
  lambda <- as.numeric(lambda)
  temp.lambda <- try({forecast::BoxCox.lambda(y, "loglik")}, silent = TRUE)
  if (!inherits(temp.lambda, "try-error"))
  {
    lambda <- c(lambda, temp.lambda)
  }

  outputs <- list()
  if (!(ts.model.type %in% c("bats", "tbats")))
  {
    lambda <- as.numeric(lambda)
    if (length(y) > 2.25 * frequency(y))
    {
      temp.lambda <- try({forecast::BoxCox.lambda(y, "loglik")}, silent = TRUE)
      if (!inherits(temp.lambda, "try-error"))
      {
        lambda <- c(lambda, temp.lambda)
      }
    }
    lambda <- sort(unique(lambda))

    best.lambda <- NULL
    fit <- ts.model.fit(y, ts.model.type = ts.model.type, ...)
    best.fit <- fit
    outputs[[length(outputs)+1]] <- fit

    for (lmb in lambda)
    {
      fit <- ts.model.fit(y, ts.model.type = ts.model.type, lambda = lmb, ...)
      outputs[[length(outputs)+1]] <- fit
      aic.len <- try({length(AIC(model(fit)))}, silent = TRUE)
      aic.len <- ifelse(inherits(aic.len, "try-error"), 0, aic.len)

      if ((ts.model.type %in% c("nnetar", "stlm")))
      {
        if (sum(residuals(fit), na.rm=TRUE)^2 < sum(residuals(best.fit), na.rm=TRUE)^2)
        {
          best.fit <- fit
          best.lambda <- lmb
        }
      } else if (aic.len > 0 && AIC(model(fit)) < AIC(model(best.fit)))
      {
          best.fit <- fit
          best.lambda <- lmb
      } else if (sum(residuals(fit)^2) < sum(residuals(best.fit)^2))
      {
        best.fit <- fit
        best.lambda <- lmb
      }

    }
  } else
  {
    outputs <- best.fit <- ts.model.fit(y, ts.model.type = ts.model.type, ...)
  }

  if (return.all.models)
  {
    best.fit <- outputs
  }
  return(best.fit)
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
#' library(ggplot2)
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#'
#' ap.split <- ts.split(AirPassengers)
#'
#' mf <- ts.model.autofit(ap.split$in.sample, ts.model.type = 'arima', return.all.models = FALSE)
#' mf.fcst <- forecast(mf, length(ap.split$out.of.sample))
#' summary(mf)
#'
#' vals <- cbind(Data = ap.split$data, Forecast.Mean = mf.fcst$mean)
#' autoplot(window(vals, start = c(1958,1)), na.rm=TRUE)
#'
#' mf2 <- ts.model.refit(ap.split$data, mf)
#' mf2.fcst <- forecast(mf2, 24)
#' summary(mf2)
#'
#' vals <- cbind(Data = ap.split$data, In.Sample.Fcst = mf.fcst$mean,
#'               Out.Of.Sample.Fcst = mf2.fcst$mean)
#' autoplot(window(vals, start = c(1958,1)), na.rm=TRUE)
#'
#' coef(mf) ==  coef(mf2)
ts.model.refit <- function(y, model, ...)
{
  y.orig <- y
  kw <- list(...)


  mdl <- model(model)
  log.lambda <- ("lambda" %in% names(mdl)) && is.numeric(mdl$lambda) && as.integer(round(mdl$lambda)) == 0L
  if (log.lambda)
  {
    y[y<0] <- 0.0
    y <- y + 1
    tsp(y) <- tsp(y.orig)
  }

  kw$y <- quote(y)

  kw$model <- mdl
  if (!is.character(mdl))
    kw$model <- quote(mdl)

  return(do.call(model$function.name, kw))
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


