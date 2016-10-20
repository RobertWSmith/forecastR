

#' @importFrom pryr dots
#' @importFrom stats as.ts
ts.model.fit <- function(y,
   ts.model.type = c("arima", "sarima", "arfima", "bats", "ets",
   "ets.multiplicative", "nnetar", "nnetar.w.decay", "tbats"), ...)
  {
  ts.model.type <- match.arg(ts.model.type)
  y <- as.ts(y[,1])
  kw <- kw.orig <- pryr::dots(...)
  kw$y <- quote(y)
  return(do.call(ts.model.type, kw))
}


#' @importFrom pryr dots
#' @importFrom stats as.ts
#' @importFrom forecast BoxCox.lambda
ts.model.lambda <- function(y, model = NULL, lambda=NULL,
  ts.model.type = c("arima", "sarima", "arfima", "bats", "ets",
    "ets.multiplicative", "nnetar", "nnetar.w.decay", "tbats"), ...)
  {
  ts.model.type <- match.arg(ts.model.type)
  y <- as.ts(y[,1])
  kw <- kw.orig <- pryr::dots(...)
  if (is.null(lambda))
  {
    lambda <- list(NULL, 0, forecast::BoxCox.lambda(y, method="loglik"))
    if (length(y) > frequency(y)*2.25)
      lambda[[length(lambda)+1L]] <- forecast::BoxCox.lambda(y, method="guerrero")
  } else
  {
    tmp <- list()
    if (is.numeric(lambda))
    {
      for (i in 1:length(lambda))
      {
        tmp[[i]] <- lambda[[1]]
      }
      lambda <- tmp
      tmp <- NULL
    } else
    {
      lambda <- list(lambda)
    }
  }
  lambda <- unique(lambda)
  fits <- vector("list", length=length(lambda))
  for (i in 1:length(lambda))
  {
    fit <- ts.model.fit(y, ts.model.type=ts.model.type, model=model,
                        lambda=lambda[[i]])
    fits[[length(fits)+1]] <- fit
  }
  return(fits)
}



#' Compare and analyze models
#'
#' @param y \code{\link[stats]{ts}} object. univariate time series
#' @param split argument passed to \code{\link[forecastR]{ts.split}}
#' @param oos.h integer. Out of sample forecast steps to be provided in the
#'   output.
#' @param alpha significance level for comparative testing
#' @param ts.model.type character. name of function from package
#' @param model \code{tsm} object, used to refit model to new data.
#' @param ... extra arguments for `ts.model.type`
#'
#' @return \code{model.fit}
#'
#' @importFrom stats ts tsp tsp<- na.omit fitted fivenum
#' @importFrom forecast forecast accuracy BoxCox.lambda
#' @importFrom pryr dots partial
#'
#' @include models.R
#' @export
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package='datasets')
#' mf <- ts.model(AirPassengers, ts.model.type='arima')
ts.model <- function(y, split = 0.2, oos.h = 18L, alpha = 0.05,
  ts.model.type = c("arima", "sarima", "arfima", "bats", "ets",
    "ets.multiplicative", "nnetar", "nnetar.w.decay", "tbats"),
  model = NULL, ...)
  {
  .calc.fits <- function(m, out.sample)
  {
    out.sample <- as.ts(hasTsp(out.sample))
    fcst <- forecast(model(m), h = length(out.sample))
    tsp(fcst$mean) <- tsp(out.sample)
    acc <- forecast::accuracy(fcst, out.sample)
    return(structure(list(model = m, forecast = fcst, accuracy = acc),
      class = "model.fits"))
  }

  .best.fit <- function(m1, m2, out.sample)
  {
    if (is.null(m1) && is.null(m2))
      return(NULL)

    fit1 <- m1
    fit2 <- m2
    if (!inherits(m1, "model.fits") && !is.null(m1))
    {
      fit1 <- .calc.fits(m1, out.sample)
      if (is.null(m2))
        return(fit1)
    }

    if (!inherits(m2, "model.fits") && !is.null(m2))
    {
      fit2 <- .calc.fits(m2, out.sample)
      if (is.null(m1))
        return(fit2)
    }

    if (fit1$accuracy["Test set", "MASE"] < fit2$accuracy["Test set",
      "MASE"])
      return(fit1)

    return(fit2)
  }

  ts.model.type <- match.arg(ts.model.type)
  kw <- kw.orig <- pryr::dots(...)
  y.orig <- y
  y.split <- ts.split(y, split = split)

  data <- y.split$data
  in.sample <- y.split$in.sample
  out.of.sample <- y.split$out.of.sample

  lambda.orig <- list(NULL, 0, forecast::BoxCox.lambda(in.sample,
    method = "loglik"))
  if (length(in.sample) > 2.25 * frequency(in.sample))
    lambda.orig[[length(lambda.orig) + 1L]] <- forecast::BoxCox.lambda(in.sample,
      method = "guerrero")
  lambda.orig <- unique(lambda.orig)

  kw$y <- quote(in.sample)
  kw$lambda <- output <- mdl <- NULL
  for (lambda in lambda.orig)
  {
    mdl <- NULL
    kw$lambda <- lambda
    mdl <- try({
      do.call(ts.model.type, kw)
    }, silent = FALSE)
    if (inherits(mdl, "try-error"))
      mdl <- NULL
    output[[(length(output) + 1L)]] <- mdl
  }

  best.fit <- NULL
  if (length(output) > 0L)
  {
    kw$y <- NULL
    kw$y <- quote(data)

    for (i in 1:length(output))
    {
      oos.mdl <- oos.fcst <- NULL
      mdl <- output[[i]]

      kw$lambda <- model(mdl)$lambda
      # arfima does not support refit with same coef
      kw$model <- NULL
      if (ts.model.type != "arfima")
        kw$model <- model(mdl)

      fit <- .calc.fits(mdl, out.of.sample)
      if (is.null(best.fit))
        best.fit <- fit

      oos.mdl <- do.call(ts.model.type, kw)
      oos.fcst <- forecast(model(oos.mdl), h = oos.h)

      # guard against explosive forecasts
      if (max(oos.fcst$mean) < (2 * max(data)))
        best.fit <- .best.fit(mdl, best.fit, out.of.sample)
    }

    kw$y <- quote(data)
    # arfima does not support refit with same coef
    if (ts.model.type != "arfima")
      kw$model <- model(mdl)

    # update best model and out of sample forecast
    oos.mdl <- do.call(ts.model.type, kw)
    oos.fcst <- forecast::forecast(model(oos.mdl), h = oos.h)
    oos.fcst$residuals <- hasTsp(as.ts(oos.fcst$residuals))
    oos.fcst$fitted <- hasTsp(as.ts(oos.fcst$fitted))
    tsp(oos.fcst$residuals) <- tsp(data)
    tsp(oos.fcst$fitted) <- tsp(data)

    test.forecast <- as.ts(best.fit$forecast$mean)
    oos.forecast <- as.ts(oos.fcst$mean)
    test.resid <- as.ts(resid(best.fit$forecast))
    forecast.resid <- as.ts(resid(oos.fcst))
    test.fitted <- as.ts(fitted(best.fit$forecast))
    forecast.fitted <- as.ts(fitted(oos.fcst))
    in.samp.error <- as.ts((test.forecast - out.of.sample))
    in.samp.abs.error <- as.ts(abs(in.samp.error))

    op.data <- cbind(Data = data, In.Sample = in.sample,
      Out.Of.Sample = out.of.sample, Test.Forecast = test.forecast,
      Forecast = oos.forecast, Test.Residuals = test.resid,
      Forecast.Residuals = forecast.resid, Test.Fitted = test.fitted,
      Forecast.Fitted = forecast.fitted, In.Sample.Error = in.samp.error,
      In.Sample.Abs.Error = in.samp.abs.error)
    output <- list(data = op.data, model = best.fit$model,
      accuracy = best.fit$accuracy, in.sample = best.fit$forecast,
      out.of.sample = oos.fcst)
    return(output)
  }
  # If nothing returned inside the above statement, cannot
  # provide forecast
  return(NULL)
}


#' Multimodel fit
#'
#' @param y \code{\link[stats]{ts}} object. univariate time series
#' @param split argument passed to \code{ts.split}
#' @param oos.h integer. Out of sample forecast steps to be provided in the
#'   output.
#' @param alpha significance level for comparative testing
#' @param ts.model.types character vector. name of function returing \code{tsm}
#'   model object
#' @param ... extra arguments for `ts.model.type`
#'
#' @importFrom pryr dots
#'
#' @include models.R
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(forecast)
#' library(forecastR)
#' data('AirPassengers', package='datasets')
#' mf <- ts.multimodel(AirPassengers)
#' autoplot(mf$data)
ts.multimodel <- function(y, split = 0.2, oos.h = 18L, alpha = 0.05,
  ts.model.types = c("arima", "sarima", "arfima", "bats", "ets",
    "ets.multiplicative", "nnetar", "nnetar.w.decay", "tbats"),
  ...)
  {
  y.split <- ts.split(y, split)
  ts.model.types <- sort(match.arg(ts.model.types, several.ok = TRUE))
  kw <- pryr::dots(...)

  output <- vector("list", length = length(ts.model.types))
  names(output) <- ts.model.types

  has.output <- rep(FALSE, times = length(ts.model.types))
  names(has.output) <- ts.model.types

  train.len <- length(y.split$out.of.sample)
  base.fcst <- c("naive", "meanf")

  train.naive <- forecast::naive(y.split$in.sample, h = train.len)
  train.mean <- forecast::meanf(y.split$in.sample, h = train.len)
  oos.naive <- forecast::naive(y.split$data, h = oos.h)
  oos.mean <- forecast::meanf(y.split$data, h = oos.h)

  train.experts <- cbind(train.naive$mean, train.mean$mean)
  oos.experts <- cbind(oos.naive$mean, oos.mean$mean)
  test.resid <- cbind(resid(train.naive), resid(train.mean))
  oos.resid <- cbind(resid(oos.naive), resid(oos.mean))
  is.error <- train.experts - y.split$out.of.sample
  is.abs.error <- abs(is.error)

  for (func in ts.model.types)
  {
    mf <- ts.model(y, split = split, oos.h = oos.h, alpha = alpha,
      ts.model.type = func, ...)
    has.output[func] <- (!is.null(mf))
    output[[func]] <- mf
    if (has.output[func])
    {
      if (is.null(train.experts))
      {
        train.experts <- na.omit(mf$data[, "Test.Forecast"])
        oos.experts <- na.omit(mf$data[, "Forecast"])
        test.resid <- na.omit(mf$data[, "Test.Residuals"])
        oos.resid <- na.omit(mf$data[, "Forecast.Residuals"])
        is.error <- na.omit(mf$data[, "In.Sample.Error"])
        is.abs.error <- na.omit(mf$data[, "In.Sample.Abs.Error"])
      } else
      {
        train.experts <- cbind(train.experts, na.omit(mf$data[,
          "Test.Forecast"]))
        oos.experts <- cbind(oos.experts, na.omit(mf$data[,
          "Forecast"]))
        test.resid <- cbind(test.resid, na.omit(mf$data[,
          "Test.Residuals"]))
        oos.resid <- cbind(oos.resid, na.omit(mf$data[,
          "Forecast.Residuals"]))
        is.error <- cbind(is.error, na.omit(mf$data[,
          "In.Sample.Error"]))
        is.abs.error <- cbind(is.abs.error, na.omit(mf$data[,
          "In.Sample.Abs.Error"]))
      }
    }
  }
  colnames(train.experts) <- c(base.fcst, ts.model.types[has.output])
  colnames(oos.experts) <- c(base.fcst, ts.model.types[has.output])
  colnames(test.resid) <- c(base.fcst, ts.model.types[has.output])
  colnames(oos.resid) <- c(base.fcst, ts.model.types[has.output])
  colnames(is.error) <- c(base.fcst, ts.model.types[has.output])
  colnames(is.abs.error) <- c(base.fcst, ts.model.types[has.output])

  data <- ts.split(y, split, as.list = FALSE)
  colnames(data) <- c("Data", "In.Sample", "Out.Of.Sample")

  op <- list(data = data, train.experts = train.experts, oos.experts = oos.experts,
    test.resid = test.resid, oos.resid = oos.resid, is.error = is.error,
    is.abs.error = is.abs.error)
  return(op)
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
#'
#' @importFrom pryr dots
#' @importFrom opera mixture
#'
#' @include models.R
#' @export
ts.mixture <- function(y, split = 0.2, oos.h = 18L, alpha = 0.05,
  ts.model.types = c("arima", "sarima", "arfima", "bats", "ets",
    "ets.multiplicative", "nnetar", "nnetar.w.decay", "tbats"),
  ...)
  {
  mmf <- ts.multimodel(y, split = split, oos.h = oos.h, alpha = alpha,
    ts.model.types = ts.model.types, ...)
  oos <- na.omit(mmf$data[, "Out.Of.Sample"])
  mx <- opera::mixture(Y = oos, experts = mmf$train.experts)
  return(list(mixture.model = mx, expert.models = mmf))
}

