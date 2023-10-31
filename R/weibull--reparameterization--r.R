#' @title Weibull reparameterization from the book to R.
#'
#' @param a alpha value given from the book.
#' @param b beta value given from the book.
#'
#' @return returns a list of the correct R parameterization.
#' @export
#'
reparwbl = function(a,b){ # a,b from text
  list(ar=a,br=b^(1/a)) # ar, br for R
}

