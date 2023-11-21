#' @title Normal Distribution Plot
#'
#' @description The following function will plot a normal distribution with the given mean, sd, and y.
#'
#' @importFrom stats dnorm
#' @importFrom graphics polygon
#' @importFrom graphics text
#' @importFrom graphics hist
#' @importFrom stats rbinom
#'
#' @param mu the expected (mean) value.
#' @param sigma the expected standard deviation value.
#' @param a the "y" value to check, such that P(Y <= a).
#'
#' @return returns the normal distribution plotted, shaded for the area under the curve, given a, and the P(Y<=a).
#' @export
#'
#' @example
#' \dontrun{myncurve(mu=0, sigma=1, a = 0.05)}
myncurve = function(mu, sigma, a){

  # For the dnorm function below, assign x to null.
  x=NULL

  # Utlizing the curve function will display the normal distribution in this case, and the bounds for the xlim are based on the mean and s.d.
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma), col = "Blue")

  # Shaded area between the curve
  area <- round(pnorm(a, mean = mu, sd = sigma),4)

  ## Area under the curve ranging from (-inf, a]
  xcurve = seq(-100, a, length = 1000)

  ## Y-values related to the area to be drawn by passing in the mean, sd, and the xcurve just created.
  ycurve <- dnorm(xcurve, mean = mu, sd = sigma)

  ## Fill in the polygon with the given vertices above to draw on the curve..
  polygon(c(-100,xcurve,a),c(0,ycurve,0),col="Blue")

  # Comptuing the Area under the curve P(X<=a), given a.
  pnorm(a, mean = mu, sd = sigma)

  # Label the area on the graph along with the expected value.
  text(x=mu + 2, y=0.5*dnorm(mu, mean = mu, sd = sigma), paste0("Area = ", area))

}
