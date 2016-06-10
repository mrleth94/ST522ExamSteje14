#'
#' @export
#' @return Return the evalutated density at the point d
#'
#' @title Estimated density
#' @description Calculate the density at the point d.
#' @usage densityEstimate(x,d,h,method = "naive")
#' @keywords density estimate
#'
#' @param x A numeric variable
#' @param d The point we want to evaluate the density at
#' @param h The bandwith
#' @param method Can be either naive or kernel
#'
#' @author Steffan Leth Jensen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, SDU, Odense \cr
#' \email{steje14@student.sdu.dk} \cr
#'
#' @examples densityEstimate(cars$dist,method = "naive")

densityEstimate <- function(x,d,h,method = "naive"){
  ###SYNTEX
  #----------------------
  if(!(is.numeric(x))){
    stop("'x' must be nummeric")
  }


  if(!(method =="naive" ||method =="kernel")){
    stop("'method' must be either 'naive' og 'kernel'")
  }

    if(missing(h)){
    if(method == "naive"){
      h <- (max(x)-min(x))/(1+log2(length(x))) # This is found in the book Statistical comoution with R at p. 283
    } else{
      h <- 0.9 * sd(x) * length(x)^(-1/5) # Silverman
    }
  }
  missd=FALSE # If we miss d we have to do another thing.
  if(missing(d)){
    missd = TRUE
    px <- c(min(x), quantile(x,.25),quantile(x,0.5),mean(x),quantile(x,0.75),max(x))
  } else {
      if(!is.double(d)){
        stop("'d' must be a number")
      }
  }
  #----------------------

  v <- rep(0,length(x))
  if(method == "naive"){
    if (missd){
      py <- c(densityEstimate(x,px[1],h), densityEstimate(x,px[2],h),densityEstimate(x,px[3],h),densityEstimate(x,px[4],h),densityEstimate(x,px[5],h),densityEstimate(x,px[6],h))

      density1 <- data.frame(px,py)
      row.names(density1) <- c("Min: ", "1st Qu.: ", "Median: ","Mean: ","3rd Qu.: ","Max: ")
      colnames(density1) <- c("x","y")
      cat("Bandwidth = ",h, "\n \n")
    } else {
      for(i in 1:length(x)){
        if(abs((d-x[i])/h) < 1){
          v[i] <- 1/2
        }
      }
      density1 <- sum(v)/(length(x)*h)
    }
  } else {
    if (missd){
      py <- c(densityEstimate(x,px[1],h,"kernel"), densityEstimate(x,px[2],h,"kernel"),densityEstimate(x,px[3],h,"kernel"),densityEstimate(x,px[4],h,"kernel"),densityEstimate(x,px[5],h,"kernel"),densityEstimate(x,px[6],h,"kernel"))

      density1 <- data.frame(px,py)
      row.names(density1) <- c("Min: ", "1st Qu.: ", "Median: ","Mean: ","3rd Qu.: ","Max: ")
      colnames(density1) <- c("x","y")
      cat("Bandwidth = ",h, "\n \n")
      }
    else {
      for(i in 1:length(x)){
        v[i] <- exp(-(((d-x[i])/h)^2)/2)/sqrt(2*pi)
      }
      density1 <- sum(v)/(length(x)*h)
    }
  }
  return(density1)
}
