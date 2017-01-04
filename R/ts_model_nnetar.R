## ts_model_nnetar.R

#' NNETAR model fitting and updates
#'
#' Wrapper for forecast package functions \code{\link[forecast]{nnetar}} to
#' allow unified interface.
#'
#' @param y Univariate Time Series
#' @param model previously fit model object
#' @param lambda optional, if null ignored otherwise treated as the Box-Cox
#'   parameter
#' @param ... additional argument for model function
#'
#' @return \code{tsm} object
#'
#' @seealso \code{\link[forecast]{nnetar}}
#'
#' @importFrom forecast nnetar
#' @importFrom stats frequency tsp tsp<- ts is.ts
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(forecastR)
#' data('AirPassengers', package = 'datasets')
#' ap.split <- ts.split(AirPassengers)
#' out.sample.len <- length(ap.split$out.of.sample)
#'
#' #base fit
#' fit <- nnetar(ap.split$in.sample)
#' #log transformation
#' fit2 <- nnetar(ap.split$in.sample, lambda = 0)
#' #log transformation w/ nnet decay parameter
#' fit3 <- nnetar(ap.split$in.sample, lambda = 0, decay = 0.01)
#'
#' fcst <- forecast(fit, h = out.sample.len)
#' fcst2 <- forecast(fit2, h = out.sample.len)
#' fcst3 <- forecast(fit3, h = out.sample.len)
#'
#' vals <- window(cbind(
#'  data = ap.split$data,
#'  base = fcst$mean,
#'  lambda = fcst2$mean,
#'  decay = fcst3$mean
#'  ), start = c(1957,1))
#'
#' ## suppressWarnings(autoplot(vals, na.rm = TRUE))
nnetar <- function(y, model = NULL, lambda = NULL, ...)
{
  model.name <- "nnetar"
  kw <- list(...)
  if(!is.null(lambda) && is.na(lambda))
    lambda <- NULL

  y.orig <- y
  y.ts <- short.ts(y, freq.multiple = 2, lambda = lambda)
  y <- y.ts$y

  if (is.null(model))
    fit <- .nnetar_initial_fit(y)
  else
    fit <- suppressWarnings(forecast::nnetar(y, model = model(model), lambda = lambda, ...))

  for (nm in package_options("ts.fields")[[model.name]])
  {
    fit[[nm]] <- short.ts.inv(y.ts, as.ts(fit[[nm]]), nm)
  }

  output <- tsm(model.name, fit)
  return(output)
}


#' @importFrom stats deltat
#' @importFrom forecast nnetar ndiffs
.nnetar_initial_fit <- function(y, split = 0.20)
{
  y.orig <- y
  y <- ts.split(y, split = split)
  oos.len <- length(y$out.of.sample)

  if (short.ts.test(y$in.sample))
  {
    in.sample <- ts(as.numeric(y$in.sample))
    out.of.sample <- ts(as.numeric(y$out.of.sample),
                        start = end(y$in.sample) + deltat(y$in.sample))
    P <- 0
  }
  p <- forecast::ndiffs(y$in.sample)
  if (p < 1)
    p <- 1
  P <- 1

  gen.opt.func <- function(in.sample, out.of.sample, lambda = NULL)
  {
    oos.len <- length(out.of.sample)
    opt.func <- function(decay)
    {
      fit <- suppressWarnings(forecast::nnetar(in.sample, p = p, P = P, lambda = lambda, decay = decay))
      fcst <- forecast::forecast(fit, h = oos.len)
      acc <- forecast::accuracy(fcst, out.of.sample)
      return(acc['Test set', 'MAE'])
    }
    return(opt.func)
  }

  base.opt.func <- gen.opt.func(y$in.sample, y$out.of.sample, lambda = NULL)
  base.decay <- optimize(base.opt.func, c(0.0, 1.0))

  fits <- list(
    base = suppressWarnings(forecast::nnetar(y$in.sample, p = p, P = P, maxit = 250)),
    base.decay = suppressWarnings(forecast::nnetar(y$in.sample, p = p, P = P,
                                  decay = base.decay$minimum, maxit = 250))
  )

  fcst.accuracy <- sapply(fits, function(x) {
    fcst <- forecast::forecast(x, h = oos.len)
    vals <- as.numeric(fcst$mean)
    if (any(is.na(vals)) || any(is.null(vals)) || any(is.infinite(vals)) ||
        any(abs(vals) > max(fcst$x)^10 ))
    {
      return(Inf)
    } else
    {
      acc <- forecast::accuracy(fcst, y$out.of.sample)
      return(acc['Test set', 'MAE'])
    }
  })
  best.acc <- which(fcst.accuracy == min(fcst.accuracy))
  best.fit <- fits[[best.acc]]
  y <- y.orig
  return(suppressWarnings(forecast::nnetar(y, model = best.fit)))
}


