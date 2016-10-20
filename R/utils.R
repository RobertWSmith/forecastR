
#' Splits time series into train & test subsets
#'
#' @param x Univariate Time Series
#' @param split numeic. If between 0 and 1, modeled as a percentage of out of
#'   sample observations. If greater than 1 then number is rounded, and used
#'   as an explicit number of out of sample records. Defaults to 20\% of records
#'   out of sample.
#' @param as.list logical. Default \code{TRUE} returns a list with 3 elements,
#'   if \code{FALSE}, returns a \code{mts} object with 3 columns
#' @param ... additional options, not currently used
#'
#' @return \code{ts.split} object.
#' @export
#'
#' @importFrom stats tsp frequency window
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
ts.split <- function(x, split = 0.2, as.list = TRUE, ...)
{
  record.count <- length(x)
  if (inherits(x, "mts"))
    record.count <- nrow(x)

  split.records <- as.numeric(if (split >= 0 & split <= 1) {
    as.integer(round(record.count * split))
  } else if (split > 1) {
    as.integer(round(split))
  } else {
    as.integer(1L)
  })
  data <- x
  in.sample <- window(x, end = tsp(x)[2] - (split.records/frequency(x)))
  out.of.sample <- window(x, start = tsp(x)[2] - ((split.records - 1)/frequency(x)))

  if (!as.list) {
    output <- cbind(data = data, in.sample = in.sample, out.of.sample = out.of.sample)
  } else {
    output <- list(data = data, in.sample = in.sample, out.of.sample = out.of.sample)
  }
  output <- structure(output, class = c("ts.split", class(output)))
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
  return(inherits(x, "ts.split"))
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
  if (is.list(x)) {
      x.tmp <- do.call(cbind, x)
      x.names <- names(x)
  } else if (inherits(x, "mts")) {
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
