
#' @title Piecewise function checker.
#'
#' @description This function will check if a function follows a piece-wise model.
#'
#' @param x the x-vector containg points to be checked or just one x-value.
#' @param coef the coefficents of the model (b0, b1, b2..)
#' @param xk the point at which both lines meet up.
#'
#' @return returns the a vector of y-points based on the equation or just one y-value.
#' @export
#'
#' @examples
#' \dontrun{pieceWiseFunction(x=1, coef=2, xk=3)}
pieceWiseFunction = function(x,coef, xk){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)
}
