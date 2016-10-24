## autofit.R

#' Generic Time Series Model Fit for Package Models
#'
#' @param y \code{ts}. Univariate Time Series
#' @param ts.model.type character. Name of model function from package.
#' @param ... additional keyword arguments provided to `ts.model.type` function.
#'
#' @importFrom pryr dots
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
ts.model.fit <- function(y, ts.model.type = package_options("autofit.models"),
  ...)
  {
  ts.model.type <- match.arg(ts.model.type)
  y <- as.ts(y)
  kw <- pryr::dots(...)
  kw$y <- quote(y)
  return(do.call(ts.model.type, kw))
}


#' Update Generic Time Series Model Fit for Package Models
#'
#' @param y \code{ts}. Univariate Time Series
#' @param model \code{tsm} object. Model which was fit in call to
#'   \code{\link[forecastR]{ts.model.fit}}
#' @param ... additional keyword arguments provided to `ts.model.type` function.
#'
#' @importFrom pryr dots
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
#' mf <- ts.model.fit(ap.split$in.sample, ts.model.type = 'arima')
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
  if (missing(model) && is.tsm(model))
    stop("Must provide model to update")
  kw <- pryr::dots(...)
  y <- as.ts(y)
  kw$y <- quote(y)
  kw$model <- model(model)
  return(do.call(model$function.name, kw))
}


#' Multi- Time Series Model Fit
#'
#' Fits all models in `ts.model.types`, with the addition of providing a naive
#' forecast, a sample mean forecast and fitting a theta model forecast. Relative
#' Error returned is defined as `error <- estimate - actual`.
#'
#' @param y \code{\link[stats]{ts}} object. univariate time series
#' @param split argument passed to \code{ts.split}
#' @param oos.h integer. Out of sample forecast steps to be provided in the
#'   output.
#' @param alpha significance level for comparative testing
#' @param ts.model.types character vector. name of function returing \code{tsm}
#'   model object
#' @param freq.multiple numeric. see \code{\link[forecastR]{short.ts.test}}
#' @param ... extra arguments for `ts.model.type`
#'
#' @importFrom pryr dots
#' @importFrom forecast naive meanf thetaf
#'
#' @include models.R
#' @export
#'
#' @seealso \code{\link[forecast]{naive}} \code{\link[forecast]{meanf}}
#'   \code{\link[forecast]{thetaf}}
#'
#' @examples
#' library(ggplot2)
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#' mf <- ts.multimodel.fit(AirPassengers)
#' vals <- cbind(mf$data, mf$train.experts)
#' colnames(vals) <- c('Data', colnames(mf$train.experts))
#'
#' autoplot(window(vals, start = c(1958,1)), na.rm=TRUE)
ts.multimodel.fit <- function(y, split = 0.2, oos.h = 18L, alpha = 0.05,
  ts.model.types = package_options("autofit.models"), freq.multiple = package_options("short.ts.frequency.multiple"),
  ...)
  {
  kw <- pryr::dots(...)
  y.split <- ts.split(y, split)
  ts.model.types <- sort(unique(match.arg(ts.model.types, several.ok = TRUE)))
  if (short.ts <- short.ts.test(y, freq.multiple = freq.multiple))
    ts.model.type <- sort(unique(package_options("autofit.models.short.ts")))
  models <- vector("list", length = length(ts.model.types))
  names(models) <- ts.model.types
  for (func in ts.model.types) models[[func]] <- ts.model.fit(y.split$in.sample,
    ts.model.type = func, ...)
  base <- .parse.forecasts(models, y.split = y.split, oos.h = oos.h,
    oos.fits = short.ts)
  return(base)
}


#' Update Multimodel fit
#'
#' @param y \code{ts}. Univariate Time Series
#' @param model.list \code{tsm.multi} object. Model which was fit in call to
#'   \code{\link[forecastR]{ts.multimodel.fit}}
#' @param split argument passed to \code{ts.split}
#' @param oos.h integer. Out of sample forecast steps to be provided in the
#'   output.
#' @param alpha significance level for comparative testing
#' @param return.models logical. If \code{TRUE}, models are returned in the
#'   output list as `models` argument, otherwise does not return models.
#' @param ... additional keyword arguments provided to `ts.model.type` function.
#'
#' @importFrom pryr dots
#' @importFrom stats as.ts
#'
#' @return \code{tsm.mulit} object, with subspecialization determined by
#'   `ts.model.type` argument.
#'
#' @seealso \code{\link[forecastR]{ts.multimodel.fit}}
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
ts.multimodel.refit <- function(y, model.list, split = 0.2, oos.h = 18L,
  alpha = 0.05, return.models = TRUE, ...)
  {
  kw <- pryr::dots(...)
  y.split <- ts.split(y, split)
  models <- model.list$models
  ts.model.types <- sort(unique(names(models)))
  output <- vector("list", length = length(ts.model.types))
  names(output) <- ts.model.types
  for (func in ts.model.types) output[[func]] <- ts.model.refit(y.split$in.sample,
    model = func, ...)
  base <- .parse.forecasts(output, y.split = y.split, oos.h = oos.h,
    return.models = return.models)
  return(base)
}


#' Update Multimodel Fit
#'
#' @param y \code{ts}. Univariate Time Series
#' @param tsm.multi \code{tsm.multi} object. Model which was fit in call to
#'   \code{\link[forecastR]{ts.model.fit}}
#' @param boot.reps integer. Number of bootstrap replicates to test against
#' @param split numeric. Determines in/out-sample proportions
#' @param oos.h integer. Number of out of sample forecast steps
#' @param alpha numeric. Significance threshold for internal testing
#' @param ... additional keyword arguments provided to \code{\link[meboot]{meboot}}
#'   and the models when calling their update methods.
#'
#' @importFrom pryr dots
#' @importFrom stats as.ts
#' @importFrom meboot meboot
#'
#' @return \code{tsm} object, with subspecialization determined by `ts.model.type`
#' argument.
#'
#' @seealso \code{\link[forecastR]{ts.model.fit}}
#'
#' @export
ts.multimodel.resample <- function(y, tsm.multi, boot.reps = 100,
  split = 0.2, oos.h = 18L, alpha = 0.05, ...)
  {
  kw <- pryr::dots(...)
  y.bs <- meboot(y, reps = boot.reps)$ensemble
  mdl.names <- names(tsm.multi$models)
  output <- vector("list", length = boot.reps)
  for (i in 1:boot.reps)
  {
    output[[i]] <- ts.multimodel.refit(y.bs[,i], tsm.multi, split = split,
                                       oos.h = oos.h, alpha = alpha,
                                       return.models = FALSE, ...)
  }
}


#' Mixture Model fit
#'
#' @param y \code{\link[stats]{ts}} object. univariate time series
#' @param split argument passed to \code{ts.split}
#' @param oos.h integer. Out of sample forecast steps to be provided in the
#'   output.
#' @param alpha significance level for comparative testing
#' @param ts.model.types character vector. name of function returing \code{tsm}
#'   model object
#' @param ... extra arguments for `ts.model.type`
#' @param parent \code{environment} object. Default is \code{\link[base]{emptyenv}}
#'
#' @importFrom pryr dots
#' @importFrom opera mixture
#' @importFrom stats na.omit
#'
#' @include models.R
#' @export
ts.mixture <- function(y, split = 0.2, oos.h = 18L, alpha = 0.05,
  ts.model.types = c("arima", "arfima", "bats", "ets", "nnetar",
    "nnetar.w.decay", "tbats"), ..., parent = emptyenv())
    {
  mmf <- ts.multimodel.fit(y, split = split, oos.h = oos.h,
    alpha = alpha, ts.model.types = ts.model.types, ...,
    parent = parent)
  oos <- na.omit(mmf$data[, "Out.Of.Sample"])
  mx <- opera::mixture(Y = oos, experts = mmf$train.experts)
  return(list(mixture.model = mx, expert.models = mmf))
}


### internal function, baseline forecasts is used inside
### `.parse.forecasts` @param data univariate time series
#' @importFrom stats fitted residuals
.baseline.forecasts <- function(data, h = 18L, return.forecasts = FALSE)
{
  ret <- list(names = c("naive", "meanf", "thetaf"), data = data)
  naive.fcst <- forecast::naive(data, h = h)
  mean.fcst <- forecast::meanf(data, h = h)
  theta.fcst <- forecast::thetaf(data, h = h)
  if (return.forecasts)
    ret$objects <- list(naive.fcst = naive.fcst, mean.fcst = mean.fcst,
      theta.fcst = theta.fcst)
  ret$fcst.mean <- cbind(naive.fcst$mean, mean.fcst$mean, theta.fcst$mean)
  ret$fcst.resid <- cbind(residuals(naive.fcst), residuals(mean.fcst),
    residuals(theta.fcst))
  ret$fcst.fitted <- cbind(fitted(naive.fcst), fitted(mean.fcst),
    fitted(theta.fcst))
  colnames(ret$fcst.mean) <- ret$names
  colnames(ret$fcst.resid) <- ret$names
  colnames(ret$fcst.fitted) <- ret$names
  return(ret)
}


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
.model.forecasts <- function(model.list, data, refit.models = FALSE,
  h = 18L, return.forecasts = TRUE)
  {
  ret <- .baseline.forecasts(data, h = h, return.forecasts)
  fcst.objects <- bf$objects
  mdl.names <- names(model.list)
  ret.names <- ret$names
  for (i in 1:length(mdl.names))
  {
    nm <- mdl.names[i]
    ret.names[length(ret.names) + 1] <- nm
    mdl <- model.list[[i]]
    if (refit.models)
      mdl <- do.call(nm, list(y = data, model = model(mdl)))
    fcst <- forecast(mdl, h = h)
    ret$fcst.mean <- cbind(ret$fcst.mean, fcst$mean)
    ret$fcst.resid <- cbind(ret$fcst.resid, residuals(fcst))
    ret$fcst.fitted <- cbind(ret$fcst.resid, fitted(fcst))
    if (return.forecasts)
      fcst.objects[[nm]] <- fcst
  }
  ret$names <- ret.names
  ret$objects <- fcst.objects
  colnames(ret$fcst.mean) <- ret$names
  colnames(ret$fcst.resid) <- ret$names
  colnames(ret$fcst.fitted) <- ret$names
  return(ret)
}


### internal function, parses model lists into list of useful
### values
#' @importFrom stats fitted residuals
.parse.forecasts <- function(is.model.list, y.split, oos.h = 18L,
  return.models = TRUE)
  {
  with(y.split, {
    output <<- .baseline.forecasts(in.sample, out.of.sample,
      data, oos.h)
  })
  data <- y.split$data
  is <- y.split$in.sample
  oos <- y.split$out.of.sample
  output <- .baseline.forecasts(is, oos, data, oos.h, oos.fits)
  output$names <- c(output$names, mdl.names)
  mdl.names <- names(is.model.list)
  for (i in mdl.names)
  {
    mdl <- is.model.list[[i]]
    is.fit <- mdl$in.sample
    is.fcst <- forecast(is.fit, h = output$train.len)
    output$train.resid <- cbind(output$train.resid, residuals(is.fcst))
    if (oos.fits)
    {
      oos.fit <- ts.model.refit(data, is.fit)
      oos.fcst <- forecast(oos.fit, h = oos.h)
      output$train.experts <- cbind(output$train.experts,
        is.fcst$mean)
      output$oos.experts <- cbind(output$oos.experts, oos.fcst$mean)
      output$oos.resid <- cbind(output$oos.resid, residuals(oos.fcst))
    } else
    {
      output$train.experts <- cbind(output$train.experts,
        fitted(is.fcst))
    }
  }
  if (oos.fits)
  {
    colnames(output$oos.experts) <- output$names
    colnames(output$train.resid) <- output$names
    colnames(output$oos.resid) <- output$names
    output$is.error <- (output$train.experts - oos)
    colnames(output$is.error) <- output$names
    output$is.abs.error <- abs(output$is.error)
    colnames(output$is.abs.error) <- output$names
  }
  colnames(output$train.experts) <- output$names
  base <- .parse.forecasts(models, y.split = y.split, oos.h = oos.h,
    oos.fits = short.ts)
  output$models <- is.model.list
  return(structure(output, class = "tsm.multi"))
}
