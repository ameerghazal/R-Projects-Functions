
#' @title Negative Binomial
#'
#' @param y the number of trials until the 'rth' sucess is observed.
#' @param r the size / number of total trials.
#' @param p the probability.
#'
#' @return returns the negative binomial distribution data.
#' @export
#'
#' @examples #P(Y=10)Y~NegBin(p=0.4,r=3)
mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}
