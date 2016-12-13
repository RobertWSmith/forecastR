## autocorrelation_tests.R

#' Identification of lag threshold for Autocorrelation analysis
#'
#' @param y univariate time series
#' @param alpha numeric, determines significance threshold for p-value analysis
#'
#' @return integer
#' @export
#'
#' @importFrom forecast findfrequency
#'
#' @family Autocorrelation Tests
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#' autocorr.lags(AirPassengers)
autocorr.lags <- function(y, alpha = 0.05)
{
  freq <- try({forecast::findfrequency(y)}, silent=TRUE)
  if (inherits(freq, "try-error"))
    freq <- frequency(hasTsp(as.ts(y)))
  return(as.integer(if (freq <= 1L)
  {
    (min(10L, ceiling(length(y)/5), na.rm = TRUE))
  } else
  {
    (min(2 * freq, ceiling(length(y)/5), na.rm = TRUE))
  }))
}


#' Durbin-Watson Test for autocorrelation
#'
#' @param y univariate time series
#' @param alpha numeric, determines significance threshold for p-value analysis
#' @param max.lag integer. If undefined, calculates an appropriate value
#' @param squared logical Set to true to test squared residuals
#' @param ... additional arguments
#'
#' @return \code{'DurbinWatsonTest'} object
#'
#' @seealso \code{\link[car]{durbinWatsonTest}}
#'
#' @export
#'
#' @family Autocorrelation Tests
#'
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#' aa <- arima(AirPassengers)
#'
#' # returns equivalent results
#' durbin.watson.test(aa)
#' durbin.watson.test(residuals(aa))
#' aa2 <- arima(AirPassengers, lambda = 0)
#' durbin.watson.test(residuals(aa2))
#'
#' #check for GARCH residuals
#' durbin.watson.test(residuals(aa)^2)
#' #equivalent to above call
#' durbin.watson.test(aa, squared = TRUE)
#'
#' durbin.watson.test(residuals(aa2)^2)
durbin.watson.test <- function(y, alpha = 0.05, max.lag = 1L,
  squared = FALSE, ...) UseMethod("durbin.watson.test")


#' @export
#' @describeIn durbin.watson.test Default method
#' @importFrom car durbinWatsonTest
#' @importFrom stats lm residuals
durbin.watson.test.default <- function(y, alpha = 0.05, max.lag = 1L,
  squared = FALSE, ...)
  {
  if (squared)
    y <- y^2
  test <- car::durbinWatsonTest(lm(y ~ time(y)), max.lag)
  test$significant <- test$p < alpha
  return(structure(list(test = test, is.significant = any(test$significant),
    squared = squared), class = c("act", "DurbinWatsonTest")))
}


#' @export
#' @describeIn durbin.watson.test \code{tsm} method
#' @importFrom stats residuals
durbin.watson.test.tsm <- function(y, alpha = 0.05, max.lag = 1L,
  squared = FALSE, ...)
  {
  return(durbin.watson.test.default(residuals(y), alpha = alpha,
    max.lag = max.lag, squared = squared, ...))
}


#' Ljung-Box Test for autocorrelation
#'
#' @param y univariate time series
#' @param alpha numeric, determines significance threshold for p-value analysis
#' @param max.lag integer. If undefined, calculates an appropriate value
#' @param ... additional arguments
#'
#' @return \code{'LjungBoxTest'} object
#'
#' @seealso \code{\link[stats]{Box.test}}
#'
#' @family Autocorrelation Tests
#' @export
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#' aa <- arima(AirPassengers)
#' ljung.box.test(residuals(aa))
#' aa2 <- arima(AirPassengers, lambda = 0)
#' ljung.box.test(residuals(aa2))
#'
#' #check for GARCH residuals
#' ljung.box.test(residuals(aa)^2)
#' ljung.box.test(residuals(aa2)^2)
ljung.box.test <- function(y, alpha = 0.05, max.lag = autocorr.lags(y,
  alpha), ...) UseMethod("ljung.box.test")


#' @export
#' @importFrom stats Box.test residuals
#' @describeIn ljung.box.test Default Ljung-Box Test
ljung.box.test.default <- function(y, alpha = 0.05, max.lag = autocorr.lags(y,
  alpha), ...)
  {
  test <- stats::Box.test(y, type = "Ljung-Box", lag = max.lag)
  is.signif <- test$p.value < alpha
  return(structure(list(test = test, is.significant = any(is.signif)),
    class = c("act", "LjungBoxTest")))
}


#' @export
#' @importFrom stats Box.test residuals
#' @describeIn ljung.box.test Default Ljung-Box Test
ljung.box.test.tsm <- function(y, alpha = 0.05, max.lag = autocorr.lags(residuals(y),
  alpha), ...)
  {
  return(ljung.box.test.default(residuals(y), alpha = alpha,
    max.lag = max.lag, ...))
}


#' McLeod - Li Test for autocorrelation
#'
#' @param y univariate time series
#' @param alpha numeric, determines significance threshold for p-value analysis
#' @param max.lag integer. If undefined, calculates an appropriate value
#'
#' @return \code{'McLeodLiTest'} object
#'
#' @seealso \code{\link[TSA]{McLeod.Li.test}}
#'
#' @family Autocorrelation Tests
#' @export
#'
#' @importFrom TSA McLeod.Li.test
#' @importFrom stats residuals
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#' aa <- arima(AirPassengers)
#' mcleod.li.test(aa)
#' aa2 <- arima(AirPassengers, lambda = 0)
#' mcleod.li.test(aa2)
#'
#' #check for GARCH residuals
#' mcleod.li.test(residuals(aa)^2)
#' mcleod.li.test(residuals(aa2)^2)
mcleod.li.test <- function(y, alpha = 0.05, max.lag = autocorr.lags(y,
  alpha)) UseMethod("mcleod.li.test")
.mcleod.li.signif <- function(test, alpha = 0.05)
{
  is.signif <- (test$p.values < alpha)
  return(structure(list(test = test, is.significant = any(is.signif)),
    class = c("act", "McLeodLiTest")))
}


#' @describeIn mcleod.li.test \code{numeric} specialization of the McLeod-Li Test.
#' @export
mcleod.li.test.default <- function(y, alpha = 0.05, max.lag = autocorr.lags(y,
  alpha))
  {
  test <- TSA::McLeod.Li.test(y = y, gof.lag = max.lag, plot = FALSE)
  return(.mcleod.li.signif(test, alpha))
}


#' @describeIn mcleod.li.test \code{ts} specialization of the McLeod-Li Test.
#' @export
mcleod.li.test.ts <- function(y, alpha = 0.05, max.lag = autocorr.lags(y,
  alpha))
  {
  return(mcleod.li.test.default(y, alpha, max.lag))
}


#' @describeIn mcleod.li.test \code{tsm} specialization of the McLeod-Li Test.
#' @export
mcleod.li.test.tsm <- function(y, alpha = 0.05, max.lag = autocorr.lags(residuals(model(y)),
  alpha))
  {
  test <- TSA::McLeod.Li.test(object = model(y), gof.lag = max.lag,
    plot = FALSE)
  return(.mcleod.li.signif(test, alpha))
}


#' @describeIn mcleod.li.test \code{\link[forecast]{Arima}} specialization of the McLeod-Li Test
#' @export
mcleod.li.test.Arima <- function(y, alpha = 0.05, max.lag = autocorr.lags(residuals(model(y)),
  alpha))
  {
  test <- TSA::McLeod.Li.test(object = y, gof.lag = max.lag,
    plot = FALSE)
  return(.mcleod.li.signif(test, alpha))
}


# plot.act <- function(x, ...)  { }
#' Print classes of \code{act} (autocorrelation test)
#'
#' @param x input object
#' @param ... additional arguments for print function
#'
#' @family Autocorrelation Tests
#'
#' @export
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#' aa <- arima(AirPassengers)
#'
#' aa.dw <- durbin.watson.test(aa)
#' print(aa.dw)
#'
#' aa.dw.sq <- durbin.watson.test(aa, squared = TRUE)
#' print(aa.dw.sq)
#'
#' aa.lb <- ljung.box.test(aa)
#' print(aa.lb)
#'
#' aa.ml <- mcleod.li.test(aa)
#' print(aa.ml)
print.act <- function(x, ...)
{
  test.type <- class(x)[2]
  cat(paste0(test.type, "\nSignificant Results: ", x$is.significant,
    "\n"))
  if (class(x)[2] == "McLeodLiTest")
  {
    for (i in 1:length(x$test$p.value))
    {
      cat("\n", sprintf("%1.6g", x$test$p.value[i]), sep = "")
    }
    cat("\n")
  } else
  {
    print(x$test)
  }
}
