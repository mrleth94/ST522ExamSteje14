#' @export
#' @return The function return residual standard error, R-squared, R-squared adjusted, F-statistic, Degrees of freedom, p-value and the betas (this is the intercept with the y-axis and the slope, and so on)
#' @title Linear model
#' @usage linMod(formula)
#'
#' @keywords linear model regretion residual
#'
#' @description It calculate the linear model. Which can find the regression.
#' @author Steffan Leth Jensen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, SDU, Odense \cr
#' \email{steje14@student.sdu.dk} \cr
#'
#' @examples linMod(cars$speed ~ cars$dist)

linMod <- function(formula){

  ###SYNTEX
  #----------------------
  if(!is.language(formula)){
    stop("'formula' must be provided")
  }
  #----------------------
  ### The first 4 lines is from r's function lm(), it's line 7,8,13,14 in lm()
  mcall <- match.call() # will not be cange during til function, this is only for summary in the end
  mf <- match.call() # Setup the matrix
  mf[[1L]] <- quote(stats::model.frame) # 1L is 1 as a integer.
  mf <- eval(mf, parent.frame())

  ## this matrix have dimensions:
  nrows <- dim(mf)[1]
  ncols <- dim(mf)[2]

  # At page 131 we have the function Y = X*beta+e
  Y <- mf[,1]
  X <- rep(1,nrows)

  # Now we will make the rest of X, X is a coloum of 1's and then the rest.
  for (i in 2:ncols){
    X <- cbind(X,mf[,i])
  }

  # betaHat will be calculated by say (X'X)^(-1)X'Y
  betaHat <- solve(t(X)%*%X) %*% (t(X) %*% Y)

  # Now we can find YHat, which is X*betaHat
  YHat <- X %*% betaHat

  # Now we can find eHat, which is Y-YHat
  eHat <- Y-YHat

  # Residual standard error is the same at the standard deviation of the residual, which is the same as eHat
  RSE <- sd(eHat)

  # Now we find residual sum of square
  RSS <- sum((eHat)^2)

  # Now we find sum of squre total
  SST <- sum((Y-mean(Y))^2)

  # Finaly we can find the R square, which is 1- RSS/SST
  R2 <- 1- RSS/SST

  # Now we want to find the adjusted
  R2.adjusted <- 1- (RSS/(nrows - ncols)) /(SST/(nrows -1))

  # Now we find the F-statistic, to do this we need SSReg
  SSreg <- sum((YHat-mean(Y))^2)
  Fstat <- (SSreg/(ncols-1))/(RSS/(nrows-ncols))

  # Now we find the degrees of freedom. In residual it is nrows-ncols
  df <- nrows-ncols

  # Then we find the p value. Since we know the F-statistics, we can use that distribution form R.
  pVal <- 1-pf(Fstat,ncols-1,df)

  # Now we find the standard error, to do this we need standard diviation of y, this we know can be calculated as the error variance
  EV <- RSS/(nrows-ncols)

  # Now we calculate the standard error.
  SE <-  sqrt(diag(EV * solve(t(X) %*% X)))

  # Now we find the t value
  tVal <- betaHat/SE

  pr <- 2*(1-pt(abs(tVal),df))

    print(list("Call" = mcall,
               "Residuals" = data.frame("Min" = min(eHat), "1Q" =quantile(eHat,0.25), "Median" = median(eHat), "3Q" = quantile(eHat,0.75), "Max" = max(eHat),row.names=" "),
               "Coefficients" = data.frame("Estimate" = betaHat, "Std. Error" = SE, "t value" = tVal, "Pr(<|t|)" = pr)))
    cat("Residual standard error: ", RSE, "on", df, "degress of freedom \n")
    cat("Multiple R-squared: ", R2, "Adjusted R-squared: ", R2.adjusted, "\n")
    cat("F-statistic: ", Fstat, "on", ncols-1, "and", df, "DF", " p-value: ", pVal)

  return(invisible(list("RSE" = RSE, "$RSqrt$" = R2, "$R2.adj" = R2.adjusted, "F-statistic" = Fstat, "DegreeOfFreedom" = df, "p-value" = pVal, "betaHat" = betaHat)))
}
