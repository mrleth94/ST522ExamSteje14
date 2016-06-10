#'
#' @export
#' @return The stationary probability of each stage
#'
#' @title Markov Chain
#' @description Calculate the probability to be in a stage after n iteration, where we start in stage k.
#' @usage markovChain(p,k,n=1000)
#' @keywords markov chain monte carlo stationary stage
#'
#' @param p Is a probability matrix
#' @param k The stage where we start
#' @param n Number of iteration.
#'
#' @author Steffan Leth Jensen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, SDU, Odense \cr
#' \email{steje14@student.sdu.dk} \cr
#'
#' @examples markovChain(matrix(c(0.5,0.5,0.2,0.8),2,2,byrow = TRUE),2,1000)

markovChain <- function(p,k,n = 1000){
  ###SYNTEX
  #----------------------
    if(missing(p)){
      stop("'p' is missing")
    }
    if(!is.matrix(p)){
      stop("'p' must be a matrix")
    } else if (!(dim(p)[1] == dim(p)[2])){
      stop("'p' must be a quadratic matrix")
    } else {
      for (i in 1: dim(p)[1]){
        if (!(sum(p[i,]) == 1)){
          stop("The sum of the elements in each row of 'p' has to be 1")
        }
        for (j in 1:dim(p)[1]){
          if(!p[i,j]>=0){
            stop("Your probability must be positive")
          }
        }
      }
    }

    if(missing(k)){
      stop("'k' is missing")
    }
    if(!all.equal(k, as.integer(k))){
      stop("'k' must be an integer")
    }
    if(! k <= dim(p)[1]){
      stop("'k' must be less than or equal dimential of the matrix")
    }

    if(!(n  == as.integer(n) && n>=0 && !is.logical(n))){
      stop("'n' must be an integer greater than or equal 0")
    }
  #----------------------
  number <- rep(0,dim(p)[1])
  number[k] <- 1000 # This tells that there will be 1000 in the system, if the number have been higher, the probability would be better.
  if(n > 0){
    for (j in 1:n){
      number1 <- rep(0,dim(p)[1])
      for(l in 1:dim(p)[1]){
        run <- sample(1:dim(p)[1],number[l],TRUE,p[l,])
        for (m in 1:length(run)){
          number1[run[m]] <- number1[run[m]] + 1
        }
      }
      number <- number1
    }
  }
  return(number/1000)
}
