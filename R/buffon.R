#'
#' @export
#' @return Return the number of hits and the estimated pi.
#'
#' @title Buffons needle
#' @description Estimating pi by throwing needles at lines, whcih is buffons experiment.
#' @usage buffon(n=1000,l=1,d=1)
#' @keywords buffon lazzerini pi-estimation
#'
#' @param n Number of throwed needle
#' @param l The length of the needle
#' @param d The distance between the lines
#'
#' @author Steffan Leth Jensen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, SDU, Odense \cr
#' \email{steje14@student.sdu.dk} \cr
#'
#' @examples Lazzerini experiment: buffon(3408,2.5,3)

buffon <- function(n=1000,l=1,d=1) {

  ###SYNTEX
  #----------------------
  if(!(n == as.integer(n) && n>0 && !is.logical(n))){
    stop("'n' must be a integer greater than 0")
  }
  if(!(is.double(l))&&l>0){
    stop("'l' must be a double greater than 0")
  }
  if(!(is.double(d)&&d>0)){
    stop("'d' must be a double greater than 0")
  }
  #----------------------

  Y <- runif(n,0,d) # Generate the position of the end of the needle
  Theta <- runif(n,0,2*pi) # Generate the angel compared to the lines
  hit <- 0 # Hit counter
  for (i in 1:n){
    if(Theta[i] <= pi/2){ # The needle can have an angle in four different quadrants
      y1 <- sin(Theta[i])*l+Y[i]
    } else if (Theta[i] <= pi){
      y1 <- sin(pi-Theta[i])*l+Y[i]
    } else if (Theta[i] <= (3/2)*pi){
      y1 <- Y[i]-sin(Theta[i]-pi)*l
    } else {
      y1 <- Y[i]+sin((2*pi-Theta[i]))*l
    }
    if(y1 <= 0 || y1 >= d) { # Does it hit a line.
      hit = hit+1
    }
  }
  return (list(numberOfHit = hit , estimatedPi = (n*2*l)/(hit*d))) # Now we return both number of hit, and the estimated pi.
}
