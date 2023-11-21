#' @title Confidence interval for a population mean of one sample.
#'
#' @description Calculates the confidence interval for the population mean
#' for a single sample, given the sample with an error.
#'
#' @importFrom stats sd
#'
#' @param x numeric vector representing the sample.
#' @param alpha confidence level for the interval, where the default is 0.05.
#'
#' @return returns a vector containing (L,U) bounds of a (1-alpha)100 percent CI.
#' @export
#'
#' @examples
#' \dontrun{myci(x = rnorm(30, mean = 10, sd = 12), alpha = 0.05)}
myci <- function(x, alpha = 0.05) {

  # Compute the sample mean, standard deviation, and store the sample size.
  ybar <- mean(x)
  s <- sd(x)
  n <- length(x)

  # Compute the t-stat with the alpha passed in.
  qt <- qt(1 - alpha/2, n - 1)

  # Compute the interval: lower--upper bounds.
  interval <- c()
  interval[1] <- ybar - (qt * (s / sqrt(n)))
  interval[2] <- ybar + (qt * (s / sqrt(n)))

  # Add some names to the interval.
  names(interval) <- c("Lower (L)", "Upper (U)")

  # Return the interval
  return(interval)
}
