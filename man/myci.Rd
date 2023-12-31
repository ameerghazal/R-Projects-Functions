% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myci.R
\name{myci}
\alias{myci}
\title{Confidence interval for a population mean of one sample.}
\usage{
myci(x, alpha = 0.05)
}
\arguments{
\item{x}{numeric vector representing the sample.}

\item{alpha}{confidence level for the interval, where the default is 0.05.}
}
\value{
returns a vector containing (L,U) bounds of a (1-alpha)100 percent CI.
}
\description{
Calculates the confidence interval for the population mean
for a single sample, given the sample with an error.
}
\examples{
\dontrun{myci(x = rnorm(30, mean = 10, sd = 12), alpha = 0.05)}
}
