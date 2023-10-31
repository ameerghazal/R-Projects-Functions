#' @title Central Limit Theorem Binomial Function
#'
#' The function works for discrete and continous distributions.
#' @param n the size.
#' @param iter the number of iterations.
#' @param p the probability.
#' @param ... any remaning parameters.
#'
#' @return a vector containg the x-values of the plot (sample mean) vs. the y-values of the plot (density).
#' @export
#'
#' @examples mycltb(n=10, iter=10000, p=0.5)
mycltb=function(n,iter,p=0.5,...){

  ## For the dnorm function below and the RMD check, we set x to "null".
  x=NULL

  ## R-random sample from the Binomial
  y=rbinom(n*iter,size=n,prob=p)


  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration
  # And the rows will equal the sample size n
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)

  ## Spply the function mean to the columns (2) of the matrix, which are placed in w.
  w=apply(data,2,mean)

  ## We will make a histogram of the values in w.
  ## How high should we make y axis?
  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param=hist(w,plot=FALSE)

  ## Since the histogram will be a density plot we will find the max density
  ymax=max(param$density)

  ## To be on the safe side we will add 10% more to this
  ymax=1.1*ymax

  ## Now we can make the histogram:

  ## freq=FALSE means take a density.
  hist(w,freq=FALSE,  ylim=c(0,ymax),
       main=paste("Histogram of sample mean","\n", "sample size= ",n,sep=""),
       xlab="Sample mean",...)

  ## Add a density curve made from the sample distribution
  #lines(density(w),col="Blue",lwd=3) # add a density plot
  ## Add a theoretical normal curve
  curve(dnorm(x,mean=n*p,sd=sqrt(p*(1-p))),add=TRUE,col="Red",lty=2,lwd=3)

}

w <- mycltb(n=4, iter=10000, p=0.3)
w
