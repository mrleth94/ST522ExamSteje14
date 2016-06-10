#' @export
#' @return Returns the true correlation, bootstrap correlation, standard error, bias and confidence interval.
#' @title Bootstrap
#' @usage bootstrap(n=1000,X,Y,plot=TRUE)
#'
#' @keywords boot bootstrap correlation
#'
#' @description Estimate the correlation, by using bootstrap.
#'
#' @param n number of tests
#' @param X is the first vector
#' @param Y is the second vector
#' @param plot, boolean do we want to plot the histogram
#' @author Steffan Leth Jensen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, SDU, Odense \cr
#' \email{steje14@student.sdu.dk} \cr
#'
#' @examples bootstrap(1000,cars$speed,cars$dist,TRUE)

bootstrap <- function(n=1000,X,Y,plot=TRUE){

  ###SYNTEX
  #----------------------
  if(!(n  == as.integer(n) && n>0 && !is.logical(n))){
    stop("'n' must be an integer greater than 0")
  }
  if(n==1){
    warning("'n' is 1, you can't calculate standard error, because you will divide by 0")
  }
  if(!is.numeric(X)){
    stop("'X' must be numeric")
  }
  if(!is.numeric(Y)){
    stop("'Y' must be numeric")
  }
  if(!is.logical(plot)){
    stop("'plot' must be an logic")
  }
  if(!(length(X) == length(Y))){
    stop("'X' and 'Y' must have same length")
  }
  #----------------------

  bootCor <- rep(0,n) # Preper an empty list for the different correlations
  for (i in 1:n){
    j <- sample(1:length(X), size = length(X), replace=TRUE) # Makes the sample, which will give the new vectors
    bootX <- X[j]
    bootY <- Y[j]
    bootCor[i] <- cor(bootX,bootY) # Generates the correlation between the two new vectors
  }

  se <- sqrt((1/(n-1))*sum((bootCor-mean(bootCor))^2)) # Find the standard error

  bias <- mean(bootCor)-cor(X,Y) # Find the bias

  CI <- c(mean(bootCor) - se * 1.96, mean(bootCor) + se * 1.96) # Make the comfidense interval, the first number is the lower bound, and the second is the upperbound.

  #plot. Only if it is true.
  if(plot){
    hist(bootCor, breaks = length(seq(from = min(bootCor), to=max(bootCor),by=0.02))) # Plot the histogram
    abline(v=CI) # Plot the confidense inverval vertical
  }

  return (list(trueCorrelation = cor(X,Y) , bootstrapCorrelation = mean(bootCor) , standardError = se , bias = bias , confidenceInterval = CI))
}
