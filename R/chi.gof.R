#'
#' @export
#' @return Return the Goodness of fit
#'
#' @title Goodness of fit
#' @description Calculate the chi-squared goodness of fit.
#' @usage chi.gof(x,p)
#' @keywords chi goodness fit
#'
#' @author Steffan Leth Jensen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, SDU, Odense \cr
#' \email{steje14@student.sdu.dk} \cr
#'
#' @param x is vector of observed variabel
#' @param p the different expected probabilities
#'
#' @examples chi.gof(c(2,5),c(0.3,0.7))

chi.gof <- function(x,p){

  ###SYNTEX
  #----------------------
  if(missing(x)){
    stop("'x' is missing")
  }

  if(!is.numeric(x)){
    stop("'x' must be a numeric")
  }

  if(missing(p)){
    p <- rep(1/length(x),length(x))
    warning("'p' is missing, it is set as uniform distributed")
  }

  if(!is.numeric(p)){
    stop("p' must be a numeric")
  }

  if(length(p) == 1){
    if(!(p*length(x)==1)){
      stop("Your expected does not give 100%")
    }
  } else{
    if(!(sum(p)==1)){
      stop("Your expected does not give 100%")
    }
    if(!(length(x) == length(p))){
      stop("'x' and 'p' must have same lenght")
    }
  }
  #----------------------
  expected <- sum(x)*p
  chi2 <- sum((((x-expected)^2)/expected))
  return (list(goodnessOfFit = chi2))
}
