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
#'   time series is short. Defaults to 2. Controls attribute \code{is.short}.
#' @param ... additional options, not currently used
#'
#' @return \code{ts.split} object.
#' @export
#'
#' @importFrom stats tsp frequency window na.omit deltat
#'
#' @examples
#' library(forecastR)
#' x <- ts(1:100, freq=12)
#' frac <- 0.20
#' x.split <- ts.split(x, split = frac)
#'
#' x.data <- x.split$data
#' x.in.sample <- x.split$in.sample
#' x.out.of.sample <- x.split$out.of.sample
#'
#' length(na.omit(x.out.of.sample)) == (length(x.data) * frac)
#' x.data == c(na.omit(x.in.sample), na.omit(x.out.of.sample))
#'
#' data("AirPassengers", package="datasets")
#' split <- 24L
#' ap.split <- ts.split(AirPassengers, split = split)
#' length(ap.split$data) == length(AirPassengers)
#' length(ap.split$out.of.sample) == split
#' length(ap.split$in.sample) == (length(AirPassengers) - split)
ts.split <- function(x, split = 0.2, as.list = TRUE, short.ts.multiple = 2.0, ...)
  {
  is.short <- (length(x) < (frequency(x) * short.ts.multiple))

  if (is.null(record.count <- dim(x)[1]))
    record.count <- length(x)

  if (split < 0)
    stop("`split` must be >= 0.0")

  split.records <- as.integer(1L)
  if (split < 1)
  {
    split.records <- as.integer(round(record.count * split, digits = 0))
  } else if (split > 1)
  {
    split.records <- as.integer(round(split, digits = 0))
  }

  # default is to provide records for each
  out.of.sample <- in.sample <- data <- x
  if (length(x) > 2)
  {
    if (!is.short)
    {
      in.sample.end <- tsp(x)[2] - (split.records * deltat(x))
    } else if (is.short && (length(x) > 2))
    {
      in.sample.end <- tsp(x)[2] - deltat(x)
    }
    out.sample.start <- in.sample.end + deltat(x)

    in.sample <- window(x, end = in.sample.end)
    out.of.sample <- window(x, start = out.sample.start)
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
#' x.split <- ts.split(x, split = sample.frac)
#' is.ts.split(x.split)
is.ts.split <- function(x)
{
  return(is(x, "ts.split"))
}


#' Modifies \code{ts.split} to a unified \code{data.frame} object
#'
#' @param x \code{ts.split} object
#' @param ... arguments passed on to \code{\link[base]{as.data.frame}}
#'
#' @seealso \code{\link[base]{as.data.frame}}
#'
#' @export
#'
#' @importFrom stats time cycle
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
  } else if (inherits(x, "matrix"))
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
#' ## [1] 2
#' short.ts.test(AirPassengers) #returns FALSE
#' short.ts.test(AirPassengers, 15.0) #returns TRUE
short.ts.test <- function(y, freq.multiple = 2.0)
{
  return(
    (frequency(y) < freq.multiple) ||
      (length(y) <= (freq.multiple * frequency(y)))
    )
}


### template for short time series y.ts <- short.ts(y,
### freq.multiple = 2) y <- y.ts$y fit <- sapply(fit,
### function(x) { if (is.ts(x)) return(short.ts.inv(x, y.ts))
### return(x) }) template for short time series
# internal function to parse short time series
#' @importFrom stats ts start tsp frequency
short.ts <- function(y, lambda = NULL,
                     freq.multiple = 2)
{
  orig.y <- y
  orig.freq <- as.numeric(frequency(y))
  needs.transformed <- short.ts.test(y, freq.multiple)
  if (needs.transformed)
    y <- ts(as.numeric(y))
  return(list(y = y, freq.multiple = freq.multiple, orig.freq = orig.freq,
    orig.start = start(orig.y), orig.tsp = tsp(orig.y),
    was.transformed = needs.transformed))
}


#' @importFrom stats ts tsp
short.ts.inv <- function(y.short.ts, x.ts = NULL, nm = "", ...)
{
  x.ts <- as.ts(x.ts)
  ts.tsp <- y.short.ts$orig.tsp
  if (!is.list(y.short.ts))
    ts.tsp <- tsp(y.short.ts)
  if (is.null(x.ts))
    x.ts <- y.short.ts$y
  ts.start <- ts.tsp[1]
  ts.freq <- ts.tsp[3]
  return(ts(as.numeric(x.ts), start = ts.start, frequency = ts.freq))
}


#' Tidy Time Series to data.frame
#'
#' @param x time series object
#'
#' @export
tidy_ts_df <- function(x) {
  output <- as.data.frame(x)
  for (n in colnames(output))
    output[[n]] <- as.numeric(output[[n]])
  cn <- colnames(output)
  output$time <- round(as.numeric(time(x)), digits = 3)
  output$cycle <- as.integer(cycle(x))
  output <- output[, c("time", "cycle", cn)]
  return(output)
}

