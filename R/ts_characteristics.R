## ts_characteristics.R


# f1 maps [0,infinity) to [0,1]
f1 <- function(x, a, b)
{
  eax <- exp(a*x)
  if (is.infinite(eax))
    f1eax <- 1
  else
    f1eax <- (eax - 1) / (eax + b)
  return(f1eax)
}


# f2 maps [0,1] onto [0,1]
f2 <- function(x, a, b)
{
  eax <- exp(a * x)
  ea <- exp(a)
  return( (eax - 1) / (eax + b) * (ea + b) / (ea - 1) )
}



#' Gratuitously borrowed from:
#' http://robjhyndman.com/hyndsight/tscharacteristics/
#'
#' @importFrom stats stl ts.union sd var
#' @importFrom forecast InvBoxCox BoxCox BoxCox.lambda findfrequency
#' @importFrom mgcv gam s
#' @importFrom stats as.ts frequency na.contiguous
#' @importFrom TSA skewness kurtosis
#' @importFrom fracdiff fracdiff
#' @importFrom tseries terasvirta.test
.ts.statistics <- function(y, transform)
{
  y <- as.ts(y)
  lambda <- NULL
  y.tsp <- tsp(y)
  # y.start <- start(y)
  y.length <- length(na.contiguous(y))
  y.freq <- frequency(y)
  y.ffreq <- try({findfrequency(na.contiguous(y))}, silent = TRUE)

  if (inherits(y.ffreq, "try-error"))
    y.ffreq <- NA_integer_

  if (transform)
  {
    lambda <- optimize.lambda(na.contiguous(y))
    y <- BoxCox(y, lambda)
  }

  season <- trend <- rep(NA_real_, length(y))
  if (y.length > y.freq)
  {
    if (!is.na(y.ffreq) && y.ffreq > 1 && y.length > (y.freq * 2))
    {
      y.stl <- stl(y, s.window = "periodic", na.action = na.contiguous)
      trend <- y.stl$time.series[ ,2]
      season <- y.stl$time.series[ ,1]
    } else
    {
      trend[!is.na(y)] <- fitted( gam(y ~ s( seq_along(y) ) ))
    }
  }

  remainder <- y - trend - season

  ts.output <- ts.union(y = y, trend = trend, season = season, remainer = remainder)

  if (transform)
    ts.output <- InvBoxCox(ts.output, lambda)

  df <- data.frame(
    start = y.tsp[1],
    end = y.tsp[2],
    frequency = y.tsp[3],
    find.frequency = as.numeric(y.ffreq),
    lambda = ifelse(is.null(lambda), NA_real_, lambda),
    mean = mean(y, na.rm = TRUE)
  )

  if (y.length > 1)
  {
    df$sd = sd(y, na.rm = TRUE)
    df$var = var(y, na.rm = TRUE)
  }

  if (y.length > 3)
    df$skew = f1(skewness(y, na.rm = TRUE), 1.510, 5.933)

  if (y.length > 4)
    df$kurt = f1(kurtosis(y, na.rm = TRUE), 2.273, 11567)

  if (y.length > 5)
  {
    df$serial.corr = f2( Box.test(y, lag = 10)$statistic / (y.length * 10), 7.53, 0.103)
    df$nonlinearity = f1(terasvirta.test(na.contiguous(y))$statistic, 0.069, 2.304)
    df$hurst.fracdiff = fracdiff(na.contiguous(y), 0, 0)$d + 0.5
  }

  # lyapunov exponent
  # determines a notion of predictability in dynamic systems
  # positive indicates likelihood of chaotic system
  # https://en.wikipedia.org/wiki/Lyapunov_exponent
  if (y.freq > y.length - 10)
  {
    df$lyapunov <- NA_real_
  } else
  {
    lyp <- numeric(ly.len <- y.length - y.freq)
    for (i in 1:ly.len)
    {
      ix <- order(abs(y[i] - y))
      ix <- ix[ix < ly.len]
      j <- ix[2]

      lyp[i] <- log(abs( (y[i + y.freq] - y[j + y.freq]) / (y[i] - y[j]) )) / y.freq
      if (is.na(lyp[i]) || is.nan(lyp[i]) || is.infinite(lyp[i]))
        lyp[i] <- NA_real_
    }

    lyap <- mean(lyp, na.rm = TRUE)
    df$lyapunov <- exp(lyap) / (1 + exp(lyap))
  }

  output <- list(
    ts.output = ts.output,
    df = df
  )
  return(output)
}


#' Time Series Characteristics
#'
#' @param y time series vector
#'
#' @export
ts.characteristics <- function(y)
{
  output <- .ts.statistics(y, FALSE)

  t.df <- .ts.statistics(y, TRUE)$df
  colnames(t.df) <- paste("tfm", colnames(t.df), sep = ".")

  output$df = cbind(output$df, t.df)

  return(output)
}
