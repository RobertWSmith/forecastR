
#' Identify lags threshold for autocorrelation analysis
#'
#' @param y univariate time series
#' @param alpha numeric, determines significance threshold for p-value analysis
#'
#' @return integer
#' @export
#'
#' @importFrom forecast findfrequency
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package='datasets')
#' autocorr.signif.lags(AirPassengers)
autocorr.signif.lags <- function(y, alpha = 0.05)
{
  freq <- forecast::findfrequency(y)
  return(as.integer(if (freq <= 1L)
  {
    (min(10L, ceiling(length(y)/5), na.rm = TRUE))
  } else
  {
    (min(2 * freq, ceiling(length(y)/5), na.rm = TRUE))
  }))
}


#' Durbin-Watson test
#'
#' @param y univariate time series
#' @param alpha numeric, determines significance threshold for p-value analysis
#' @param max.lag integer. If undefined, calculates an appropriate value
#'
#' @return \code{'DurbinWatsonTest'} object
#'
#' @seealso \code{\link[car]{durbinWatsonTest}}
#'
#' @export
#'
#' @importFrom car durbinWatsonTest
#' @importFrom stats lm
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package='datasets')
#' aa <- arima(AirPassengers)
#' durbin.watson.test(resid(aa))
#' aa2 <- arima(AirPassengers, lambda=0)
#' durbin.watson.test(resid(aa2))
#'
#' #check for GARCH residuals
#' durbin.watson.test(resid(aa)^2)
#' durbin.watson.test(resid(aa2)^2)
durbin.watson.test <- function(y, alpha = 0.05, max.lag = 1L)
  {
  test <- car::durbinWatsonTest(lm(y ~ time(y)), max.lag)
  is.signif <- test$p < alpha
  return(structure(list(test = test, is.significant = any(is.signif)),
    class = c("act", "DurbinWatsonTest")))
}


#' Ljung-Box Test
#'
#' @param y univariate time series
#' @param alpha numeric, determines significance threshold for p-value analysis
#' @param max.lag integer. If undefined, calculates an appropriate value
#'
#' @return \code{'LjungBoxTest'} object
#'
#' @seealso \code{\link[stats]{Box.test}}
#'
#' @export
#'
#' @importFrom stats Box.test
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package='datasets')
#' aa <- arima(AirPassengers)
#' ljung.box.test(resid(aa))
#' aa2 <- arima(AirPassengers, lambda=0)
#' ljung.box.test(resid(aa2))
#'
#' #check for GARCH residuals
#' ljung.box.test(resid(aa)^2)
#' ljung.box.test(resid(aa2)^2)
ljung.box.test <- function(y, alpha = 0.05, max.lag = autocorr.signif.lags(y,
  alpha))
  {
  test <- stats::Box.test(y, type = "Ljung-Box", lag = max.lag)
  is.signif <- test$p.value < alpha
  return(structure(list(test = test, is.significant = any(is.signif)),
    class = c("act", "DurbinWatsonTest")))
}


#' McLeod - Li Test
#'
#' @param y univariate time series
#' @param alpha numeric, determines significance threshold for p-value analysis
#' @param max.lag integer. If undefined, calculates an appropriate value
#'
#' @return \code{'McLeodLiTest'} object
#'
#' @seealso \code{\link[TSA]{McLeod.Li.test}}
#'
#' @export
#'
#' @importFrom TSA McLeod.Li.test
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package='datasets')
#' aa <- arima(AirPassengers)
#' mcleod.li.test(aa)
#' aa2 <- arima(AirPassengers, lambda=0)
#' mcleod.li.test(aa2)
#'
#' #check for GARCH residuals
#' mcleod.li.test(resid(aa)^2)
#' mcleod.li.test(resid(aa2)^2)
mcleod.li.test <- function(y, alpha = 0.05, max.lag = autocorr.signif.lags(y,
  alpha)) UseMethod("mcleod.li.test")

.mcleod.li.signif <- function(test, alpha = 0.05)
{
  is.signif <- (test$p.values > alpha)
  return(structure(list(test = test, is.significant = any(is.signif)),
    class = c("act", "McLeodLiTest")))
}

#' @export
mcleod.li.test.default <- function(y, alpha = 0.05, max.lag = autocorr.signif.lags(y,
  alpha))
  {
  test <- TSA::McLeod.Li.test(y = y, gof.lag = max.lag, plot = FALSE)
  return(.mcleod.li.signif(test, alpha))
}

#' @export
mcleod.li.test.ts <- function(y, alpha = 0.05, max.lag = autocorr.signif.lags(y,
  alpha))
  {
  return(mcleod.li.test.default(y, alpha, max.lag))
}

#' @export
mcleod.li.test.tsm <- function(y, alpha = 0.05, max.lag = autocorr.signif.lags(residuals(model(y)),
  alpha))
  {
  test <- TSA::McLeod.Li.test(object = model(y), gof.lag = max.lag,
    plot = FALSE)
  return(.mcleod.li.signif(test, alpha))
}

#' @export
mcleod.li.test.Arima <- function(y, alpha = 0.05, max.lag = autocorr.signif.lags(residuals(model(y)),
  alpha))
  {
  test <- TSA::McLeod.Li.test(object = y, gof.lag = max.lag,
    plot = FALSE)
  return(.mcleod.li.signif(test, alpha))
}


#' Print classes of \code{act} (autocorrelation test)
#'
#' @param x input object
#' @param ... additional arguments for print function
#'
#' @export
print.act <- function(x, ...)
{
  test.type <- class(x)[2]
  cat(paste0(test.type, "\nSignificant Results: ", x$is.significant, '\n'))
  switch(test.type, McLeodLiTest = {
    for (i in 1:length(x$test$p.value))
    {
      cat("\n", sprintf("%1.6g", x$test$p.value[i]), sep = "")
    }
    cat("\n")
  }, {
    print(x = x$test)
  })
}

