
#' @title Negative Binomial
#'
#' @description {The following function will calculate the negative binomial distribution by pure definition.
#' That is, it will utilize the choose function. For example (P<=10).}
#'
#' @param y the number of trials until the 'rth' success is observed.
#' @param r the size / number of total trials.
#' @param p the probability.
#'
#' @return returns the negative binomial distribution data.
#' @export
#'
#' @examples
#' \dontrun{mynbin(y=10, r=3, p=0.4)}
mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}
