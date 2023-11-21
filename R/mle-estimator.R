#' @title Maximum Likelihood Estimator
#'
#' @description {Used for repeated sampling from the same distribution, and
#' calculates the maximum likelihood estimator.}
#'
#' @importFrom stats dbinom
#' @importFrom graphics points
#' @importFrom graphics axis
#'
#' @param lfun the function to be used.
#' @param x the list of values we want to find an approx for.
#' @param param similar to bounds.
#' @param ... any other relevant parameters (title, etc.).
#'
#' @return returns a plot of the MLE estimator and a list of data.
#' @export
#'
#' @examples
#' \dontrun{mymaxlik(x=c(3,3,4,3,4,5,5,4),param=seq(0,1,length=1000),
#' \dontrun lfun=logbin,xlab=expression(pi),main="Binomial",cex.main=2)}
mymaxlik=function(lfun=logbin,x,param,...){

  # Must define the function logbin.
  logbin=function(x,param) log(dbinom(x,prob=param,size=20))

  # How many param values are there: 3 and more.
  np=length(param)

  # Outer -- notice the order: x then param
  # this produces a matrix -- try outer(1:4,5:10,function(x,y) paste(x,y,sep=" "))   to understand

  # z is a matrix where each x,param is replaced with the function evaluated at those values
  z=outer(x,param,lfun)

  # y is a vector made up of the column sums
  # Each y is the log lik for a new parameter value
  y=apply(z,2,sum)

  plot(param,y,col="Blue",type="l",lwd=2,...)

  # which gives the index for the value of y == max.
  # there could be a max between two values of the parameter, therefore 2 indices
  # the first max will take the larger indice
  i=max(which(y==max(y)))
  abline(v=param[i],lwd=2,col="Red")

  # plots a nice point where the max lik is
  points(param[i],y[i],pch=19,cex=1.5,col="Black")
  axis(3,param[i],round(param[i],2))
  #check slopes. If it is a max the slope shoud change sign from + to
  # We should get three + and two -vs
  ifelse(i-3>=1 & i+2<=np, slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),slope<-"NA")
  return(list(i=i,parami=param[i],yi=y[i],slope=slope))
}

# Function call.
#mymaxlik(x=c(3,3,4,3,4,5,5,4),param=seq(0,1,length=1000),lfun=logbin,xlab=expression(pi),main="Binomial",cex.main=2)
