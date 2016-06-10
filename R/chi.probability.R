#'
#' @export
#' @return Return the probability that the chi-squared distribution is greater than the value x.
#'
#' @title Chi probability
#' @description Calcutate the probability that the chisquared distribution is larger than a number x.
#' @usage chi.probability(x,df=1,n=10)
#' @keywords chi probability chisquare
#'
#' @param x The value that we will estimate about
#' @param df Degrees of freedom
#' @param n Number of generated random variables
#'
#' @author Steffan Leth Jensen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, SDU, Odense \cr
#' \email{steje14@student.sdu.dk} \cr
#'
#' @examples chi.probability(4,19,1000)

chi.probability <- function(x, df = 1, n=10){

  ###SYNTEX
  #----------------------
  if(missing(x)){
    stop("'x' is missing")
  }

  if (!is.numeric(x)) {
    stop("'x' must be a numeric")
  }

  if(!(df == as.integer(df) && df>0 && !is.logical(df))){
    stop("'df' must be an integer greater than 0")
  }

  if(!(n == as.integer(n) && n>0 && !is.logical(n))){
    stop("'n' must be an integer greater than 0")
  }
  #----------------------

 chi <- rnorm(n)^2

 if (df > 1){
   for (i in 2:df){
     chi <- chi + rnorm(n)^2
   }
 }

 cdf <- ecdf(chi)
 a <- cdf(x)
 return(list("pVal" = 1-a))
}

