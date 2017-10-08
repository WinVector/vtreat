

#' Solve for best non-decreasing fit using isotone splines 
#' (from the "scam" package \url{https://CRAN.R-project.org/package=scam}.
#'
#' Return a vector of length y that is a function of x
#' (differs at must where x differs) obeying the same order
#' constraints as x.  This vector is picked as close to
#' y (by square-distance) as possible.
#'
#' @param varName character, name of variable
#' @param x numeric, factor, or character input (not empty, no NAs). 
#' @param y numeric or castable to such (same length as x no NAs), output to match
#' @param w numeric positive, same length as x (weights, can be NULL)
#' @return isotonicly adjusted y (non-decreasing)
#'
#'
#' @examples
#' 
#' solveNonDecreasingS('v', 1:3, c(1,2,1))
#' # [1] 1.0 1.5 1.5
#' 
solveNonDecreasingS <- function(varName, x, y, w=NULL) {
  if(is.character(x)) {
    x <- as.factor(x)
  }
  x <- as.numeric(x)
  n <- length(x)
  if(n<=0) {
    return(NULL)
  }
  y <- as.numeric(y)
  if(length(y)!=n) {
    stop(paste("solveNonDecreasingS", varName, "expect length(y)==length(x)"))
  }
  if(is.null(w)) {
    w <- rep(1.0, n)
  }
  if(!is.numeric(w)) {
    stop(paste("solveNonDecreasingS", varName, "expect w numeric"))
  }
  if(length(w)!=n) {
    stop(paste("solveNonDecreasingS", varName, "expect length(w)==length(x)"))
  }
  if(min(w)<=0) {
    stop(paste("solveNonDecreasingS", varName, "expect positive weights"))
  }
  d <- data.frame(x=x, y=y, w=w)
  f <- "y ~ s(x, bs='mpi')"
  m <- scam::scam(as.formula(f), data=d)
  pred <- predict(m, newdata=d)
  pred
}

#' Solve for best non-increasing fit.
#'
#' Return a vector of length y that is a function of x
#' (differs at must where x differs) obeying the opposite order
#' constraints as x.  This vector is picked as close to
#' y (by square-distance) as possible.
#'
#' @param varName character, name of variable
#' @param x numeric, factor, or character input (not empty, no NAs). 
#' @param y numeric (same length as x no NAs), output to match
#' @param w numeric positive, same length as x (weights, can be NULL)
#' @return isotonicly adjusted y (non-decreasing)
#'
#'
#' @examples
#' 
#' solveNonIncreasingS('v', 1:3, c(1,2,1))
#' # [1] 1.0 1.5 1.5
#' 
solveNonIncreasingS <- function(varName, x, y, w=NULL) {
  -solveNonDecreasingS(varName, x, -y, w)
}


#' Solve for best single-direction (non-decreasing or non-increasing) fit.
#'
#' Return a vector of length y that is a function of x
#' (differs at must where x differs) obeying the either the same 
#' order contraints or the opposite order
#' constraints as x.  This vector is picked as close to
#' y (by square-distance) as possible.
#'
#' @param varName character, name of variable
#' @param x numeric, factor, or character input (not empty, no NAs). 
#' @param y numeric (same length as x no NAs), output to match
#' @param w numeric positive, same length as x (weights, can be NULL)
#' @return isotonicly adjusted y (non-decreasing)
#'
#'
#' @examples
#' 
#' solveIsotoneS('v', 1:3, c(1,2,1))
#' # [1] 1.0 1.5 1.5
#' 
solveIsotoneS <- function(varName, x, y, w=NULL) {
  soln1 <- solveNonDecreasingS(varName, x, y, w)
  d1 <- sum((y-soln1)^2)
  soln2 <- solveNonIncreasingS(varName, x, y, w)
  d2 <- sum((y-soln2)^2)
  if(d1<=d2) {
    return(soln1)
  }
  return(soln2)
}


