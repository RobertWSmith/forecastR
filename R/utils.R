## utils.R
#' Splits time series into train & test subsets
#'
#' @param x Univariate Time Series
#' @param split numeic. If between 0 and 1, modeled as a percentage of out of
#'   sample observations. If greater than 1 then number is rounded, and used
#'   as an explicit number of out of sample records. Defaults to 20\% of records
#'   out of sample.
#' @param as.list logical. Default \code{TRUE} returns a list with 3 elements,
#'   if \code{FALSE}, returns a \code{mts} object with 3 columns
#' @param short.ts.multiple numeric. Multiple of \code{frequency(x)} which
#'   if \code{frequency(x) * short.ts.multiple > length(x)} indicates that a
#'   time series is short. Defaults to 2.25. Controls attribute \code{is.short}.
#' @param ... additional options, not currently used
#'
#' @return \code{ts.split} object.
#' @export
#'
#' @importFrom stats tsp frequency window na.omit
#'
#' @examples
#' library(forecastR)
#' x <- ts(1:100, freq=12)
#' frac <- 0.2
#' x.split <- ts.split(x, split.frac = frac)
#'
#' x.data <- x.split$data
#' x.in.sample <- x.split$in.sample
#' x.out.of.sample <- x.split$out.of.sample
#'
#' length(na.omit(x.out.of.sample)) == (length(x.data) * frac)
#' x.data == c(na.omit(x.in.sample), na.omit(x.out.of.sample))
ts.split <- function(x, split = 0.2, as.list = TRUE, short.ts.multiple = package_options("short.ts.frequency.multiple"),
  ...)
  {
  is.short <- (length(x) < (frequency(x) * short.ts.multiple))
  record.count <- dim(x)
  if (length(record.count) > 1)
    record.count <- record.count[1] else if (is.null(record.count))
    record.count <- length(x)
  if (split < 0)
    stop("`split` must be >= 0.0")
  split.records <- as.numeric(1L)
  if (split < 1)
    split.records <- as.numeric(round(record.count * split,
      digits = 0)) else if (split > 1)
    split.records <- as.numeric(round(split.records, digits = 0))
  # default is to provide records for each
  out.of.sample <- in.sample <- data <- x
  if (!is.short)
  {
    in.sample <- window(x, end = tsp(x)[2] - (split.records/frequency(x)))
    out.of.sample <- window(x, start = tsp(x)[2] - ((split.records -
      1)/frequency(x)))
  }
  if (!as.list)
  {
    output <- cbind(data = data, in.sample = in.sample, out.of.sample = out.of.sample)
  } else
  {
    output <- list(data = data, in.sample = in.sample, out.of.sample = out.of.sample)
  }
  output <- structure(output, class = c("ts.split", class(output)))
  attr(output, "is.short") <- is.short
  return(output)
}


#' Tests if object is \code{ts.split}
#'
#' @param x object to be tested
#'
#' @return \code{TRUE} if \code{x} inherits from \code{ts.split} else
#'   \code{FALSE}.
#' @export
#'
#' @examples
#' library(forecastR)
#' x <- ts(1:100, freq=12)
#' sample.frac <- 0.2
#' !is.ts.split(x)
#' x.split <- ts.split(x, split.frac = sample.frac)
#' is.ts.split(x.split)
is.ts.split <- function(x)
{
  return(is(x, "ts.split"))
}


#' Modifies \code{ts.split} to a unified \code{data.frame} object
#'
#' @param x \code{ts.split} object
#' @param ... arguments passed on to \code{\link{as.data.frame}}
#'
#' @seealso \code{\link{as.data.frame}}
#'
#' @export
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package='datasets')
#' split.ex <- ts.split(AirPassengers)
#' split.df <- as.data.frame(split.ex)
#' head(split.df)
as.data.frame.ts.split <- function(x, ...)
{
  x.tmp <- x
  x.names <- "x"
  if (is.list(x))
  {
    x.tmp <- do.call(cbind, x)
    x.names <- names(x)
  } else if (inherits(x, "mts"))
  {
    x.tmp <- x
    x.names <- colnames(x)
  }
  ts.time <- as.numeric(time(x.tmp))
  ts.cycle <- as.integer(cycle(x.tmp))
  ts.x <- as.data.frame(x.tmp)
  df <- data.frame(time = ts.time, cycle = ts.cycle, ts.x)
  colnames(df) <- c("time", "cycle", x.names)
  return(df)
}


#' Tidy \code{ts.split} object into \code{data.frame}.
#'
#' @param x \code{ts.split} object
#' @param ... arguments passed on to \code{as.data.frame}
#'
#' @return \code{\link{data.frame}} with `time` column and columns for
#'   each \code{\link[stats]{ts}} object. If \code{\link[stats]{ts}} inherits
#'   from \code{mts}, \code{\link{matrix}} or is a
#'   \code{\link{list}} of \code{\link[stats]{ts}} objects, it is coerced
#'   to a \code{mts} before using the native call to
#'   \code{\link{as.data.frame.ts}}.
#'
#' @export
#'
#' @importFrom stats time cycle frequency deltat
#' @importFrom broom tidy
#'
#' @seealso \code{\link[broom]{tidy}} \code{\link{as.data.frame.ts}}
#'   \code{\link{as.data.frame.list}}
#'
#' @examples
#' library(forecastR)
#' library(broom)
#' x <- ts(1:100, freq=12)
#' x.split <- ts.split(x)
#' x.tidy <- tidy(x.split)
#' is.data.frame(x.tidy)
#' head(x.tidy)
tidy.ts.split <- function(x, ...)
{
  return(as.data.frame(x, ...))
}


#' Tests if a time series is short
#'
#' Used internally to determine the appropriate time series models to apply.
#' Short time series will be modeled as non-seasonal and will be fit against
#' simpler models.
#'
#' @param y univariate time series
#' @param freq.multiple numeric. multiple of the frequency of x to be compared
#'   against the length of x
#'
#' @export
#'
#' @importFrom stats frequency
#'
#' @examples
#' library(forecastR)
#' data('AirPassengers', package='datasets')
#' print('Default Freq. Multiple:', package_options('short.ts.frequency.multiple'))
#' ## [1] 2.25
#' short.ts.test(AirPassengers) #returns FALSE
#' short.ts.test(AirPassengers, 15.0) #returns TRUE
short.ts.test <- function(y, freq.multiple = package_options("short.ts.frequency.multiple"))
{
  return((frequency(y) * freq.multiple) > length(y))
}


### template for short time series y.ts <- short.ts(y,
### freq.multiple = 2.25) y <- y.ts$y fit <- sapply(fit,
### function(x) { if (is.ts(x)) return(short.ts.inv(x, y.ts))
### return(x) }) template for short time series
# internal function to parse short time series
#' @importFrom stats ts start tsp frequency
short.ts <- function(y, freq.multiple = package_options("short.ts.frequency.multiple"))
{
  orig.y <- y
  orig.freq <- as.numeric(frequency(y))
  needs.transformed <- short.ts.test(y, freq.multiple)
  if (needs.transformed)
    y <- ts(as.numeric(y))
  return(list(y = y, freq.multiple = freq.multiple, orig.freq = orig.freq,
    orig.start = start(orig.y), orig.tsp = tsp(orig.y), was.transformed = needs.transformed))
}


#' @importFrom stats ts tsp
short.ts.inv <- function(y.short.ts, x.ts = NULL, ...)
{
  if (!is.null(x.ts) && !is.ts(x.ts))
    stop("`x.ts` must be `ts` object.")
  ts.tsp <- y.short.ts$orig.tsp
  if (!is.list(y.short.ts))
    ts.tsp <- tsp(y.short.ts)
  if (is.null(x.ts))
    x.ts <- y.short.ts$y
  ts.start <- ts.tsp[1]
  ts.freq <- ts.tsp[3]
  return(ts(as.numeric(x.ts), start = ts.start, frequency = ts.freq))
}
