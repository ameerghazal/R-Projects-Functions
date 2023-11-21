#' @title Overbooking Ticket Function (CLT)
#'
#' @description Based on the Central limit theorem, we can use a continuous normal approximation for a discrete variable.
#' Thus, we solve for -n- number of seats sold, with the possibility of an overbook in both the discrete case and the continuous case.
#' We used functions such as which.min() and stats::uniroot() to calculate these values.
#' From there, we calculated nc and nd as-well as the other given values and put them into a list.
#' Finally, we plotted both the discrete and continuous approximations.
#'
#'
#' @importFrom stats pnorm
#' @importFrom stats pbinom
#' @importFrom stats qbinom
#' @importFrom graphics abline
#' @importFrom graphics curve
#' @importFrom graphics lines
#'
#' @param N the number of seats on the flight.
#' @param gamma the probability a plane will be truly overbooked.
#' @param p the probability of showing up.
#'
#' @return returns multiple plots, a list of the discrete and continuous approximations w/ other parameters.
#' @export
#'
#' @examples
#' \dontrun{ntickets(N=200, gamma=0.02, p=0.95)}
ntickets <- function (N, gamma, p) {

  # First, define the objective function inside of the n-tickets (discrete).
  objFunctionDiscrete <- function(n) {
    1 - gamma - pbinom(N, n, p)
  }

  # Store the possible n-values.
  npos <- seq(N, N+30, by=1)

  # Plug into the objective function and plot them.
  objNpos <- objFunctionDiscrete(npos)
  absPoint <- abs(objNpos)

  # Find the minimum
  idx <- which.min(absPoint)

  # Use the minimum index and access the npos to define n.
  nd <- npos[idx]
  print(nd)

  # Now, we have our n-value so we can calculate for the discrete distribution.
  print(pbinom(N, nd, p))

  # Now, let's plot the discrete distributon:

  ## Plot the disrecte distribution (STEP 3)
  plot(x=npos, y=objNpos, xlab="n", ylab = "Objective-Discrete", main = paste("Objective vs. n (Discrete Binomial) \n (n = ", nd, ", gamma = ", gamma, ", probability = ", p, ", N = ", N), ylim=c(0,1), pch=19, col="Blue")

  # Draw the curve.
  lines(x = npos, y = objNpos, col = "Black")

  # Draw the redline, indicating the point minimum.
  abline(v = nd, h=0, col="Red")


  # Now, let's do the continuous approximation.

  # Continuous Approximation objective function.
  objFunctionContinous <- function (n) {

    # Calculate the mean for the "n" we are testing.
    sampleMeans <- n * p

    # Calculate the variance for the "n" we are testing.
    sampleVars <- sampleMeans * (1-p)

    # Calculate the sd for the "n" we are testing.
    sampleSDs <- sqrt(sampleVars)

    # Now, use the pnorm() function for the approximation with q=N.
    list <- 1 - gamma - pnorm(N+0.5, sampleMeans, sampleSDs)

    # Return the list of the pnorm's.
    return(list)
  }

  # Find the root (0) by using the uni-root function.
  nc <- stats::uniroot(objFunctionContinous, interval = c(N, N+20), maxiter=1000000)$root

  # Saved the root above, print out.
  print(nc)

  # Draw the curve for the continous plot (STEP 3).
  curve(objFunctionContinous, xlim = c(N, N+30), ylim=c(0,1), xlab="n", ylab="Objective-Continous", main=paste("Objective vs. n (Continous Normal Apprx.) \n (n = ", nc, ", gamma = ", gamma, ", probability = ", p, ", N = ", N), lwd=4)

  # Draw the red-curve, signaling the correct point.
  abline(v = nc, h = 0, col="Red")

  # So, we now have our n for the continuous, thus, we can calculate nc:

  # Calculate the mean and the sd.
  sampleMean <- nc * p
  sampleSD <- sqrt(sampleMean * (1-p))

  # Use the values above with a right correction for the nc approximation.
  print(pnorm(N+0.5, sampleMean, sampleSD))

  ## Step 2: Create a list and print containing nc, nd, p, N, and gamma and label all.
  list <- c("Discrete (Binomial) n"= nd, "Continous Approx. n" = nc, "Seats on Flight" = N, "Probability Showing" = p, "  Probability Overbooked" = gamma)
  print(list)



  ### NOT USED CODE: ENDED UP USING DIFFERENT METHODS [BELOW]

  ## Discrete Distribution

  # Given all the variables above, we must find n:
  # N = qbinom(1-gamma, n, p)

  # We can create a vector from (N) to nMax, which is the number of seats on the flight to the max number of seats booked (which can be up to 10 more, per say).
  #npos <- N:(N+30)
  #print(npos)

  # We can then plug this into the function for the qbinom() and find the index of (N) in the list returned.
  # idx <- qbinom(p = 1-gamma, size = npos, prob = p)
  #idx <- which(idx == N)

  # From there, we can access the idx of the original npos list to find nd (n discrete).
  #n <- npos[idx]
  #print(n)


  # So, now we have n, and from here, we can compute the correct calculations. Below is the objective function.
  # pbinom(N, n, 0.95) - 1 + gamma
  #nd <- pbinom(N, n, p)
  #print(nd)


  ## UNUSED APPROCH BELOW FOR CONTINOUS APPROX.

  # Compare this value with the previous approach, and we see they are roughly the same.

  # Store the possible n-values in a list.
  #nposCont <- seq(N, N+20, length = 1000000)

  # Update the npos values with the right correction..
  #nposCont <- nposCont + 0.5

  # Plug into the objective function and plot them.
  #objContPos <- objFunctionContinous(npos, p)

  # Find the minimum
  #idx <- which.min(objContPos)

  # Use the minimum index and access the npos to define n.
  #ncOther <- nposCont[idx]

  # Compare both n's through the continuous approach.
  #print(ncOther)


  # Now, let's plot these n-values vs. the objective function.
  # plot(x=npos, y=objContPos, xlim=c(N, N+20), ylim=c(0,1), xlab="n", ylab="Objective-Continous", main=paste("Objective vs. n (Continous Normal Apprx.) \n (n = ", ncUni, ", gamma = ", gamma, ", probability = ", p, ", N = ", N))

}








