#'
#' @export
#' @return Plots the density function
#'
#' @title Plot the density
#' @description Plot the density of a method.
#' @usage densityPlot(x,n=500, method = "naive" , from, to)
#' @keywords density plot
#'
#' @param x A nummeric variable
#' @param n Number of plottet points
#' @param method Can be either naive or kernel
#' @param from Where the function want to start
#' @param to Where the function want to end.
#'
#' @author Steffan Leth Jensen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, SDU, Odense \cr
#' \email{steje14@student.sdu.dk} \cr
#'
#' @examples densityPlot(cars$dist,1000,"naive", 0,10)

densityPlot <- function(x,n=500, method ="naive", from, to){
  ###SYNTEX
  #----------------------
  if(!(is.numeric(x))){
    stop("'x' must be nummeric")
  }

  if(missing(from)){
    from <- min(x)-1/3*sd(x)
  }
  if(!is.double(from)){
    stop("'from' has to be a number")
  }
  if (missing(to)){
    to <- max(x)+1/3*sd(x)
  }
  if(!is.double(to)){
    stop("'to' has to be a number")
  }

  if(!(all.equal(n,as.integer(n)) || n<0)){
    stop("'n' must be a positive integer")
  }

  if(!(method =="naive" || method =="kernel")){
    stop("'method' must be either 'naive' og 'kernel'")
  }
  #----------------------

  if(method == "naive"){
    X <- seq(from,to,(to-from)/(n-1))
    Y <- rep(0,n)
    for(i in 1:n){
      Y[i] <- densityEstimate(x,X[i])
    }
    plot(X,Y,type='s')
  }
  if(method == "kernel"){
    X <- seq(from,to,(to-from)/(n-1))
    Y <- rep(0,n)
    for(i in 1:n){
      Y[i] <- densityEstimate(x,X[i],method="kernel")
    }
    plot(X,Y,type='l')
  }
}
