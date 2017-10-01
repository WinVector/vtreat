

#' Solve for best non-decreasing fit.
#'
#' Return a vector of length y that is a function of x
#' (differs at must where x differs) obeying the same order
#' constraints as x.  This vector is picked as close to
#' y (by square-distance) as possible.
#'
#' @param varName character, name of variable
#' @param x numeric input (not empty, no NAs). 
#' @param y numeric (same length as x no NAs), output to match
#' @param w numeric positive, same length as x (weights, can be NULL)
#' @return isotonicly adjusted y (non-decreasing)
#'
#'
#' @examples
#' 
#' 
#' solveIsotonicProblemW('v', 1:3, c(1,2,1))
#' # [1] 1.0 1.5 1.5
#' 
solveNonDecreasing <- function(varName, x, y, w=NULL) {
  if(!is.numeric(x)) {
    stop(paste("solveIsotonicProblemW", varName, "expect x numeric"))
  }
  n <- length(x)
  if(n<=0) {
    return(NULL)
  }
  if(!is.numeric(y)) {
    stop(paste("solveIsotonicProblemW", varName, "expect y numeric"))
  }
  if(length(y)!=n) {
    stop(paste("solveIsotonicProblemW", varName, "expect length(y)==length(x)"))
  }
  if(is.null(w)) {
    w <- rep(1.0, n)
  }
  if(!is.numeric(w)) {
    stop(paste("solveIsotonicProblemW", varName, "expect w numeric"))
  }
  if(length(w)!=n) {
    stop(paste("solveIsotonicProblemW", varName, "expect length(w)==length(x)"))
  }
  if(min(w)<=0) {
    stop(paste("solveIsotonicProblemW", varName, "expect positive weights"))
  }
  d <- data.frame(x=x, y=y, w=w)
  # get some corner cases
  if(n<=2) {
    v <- sum(w*y)/sum(w)
    if(n<=1) {
      return(v)
    }
    if((y[[2]]>=y[[1]])&&(x[[2]]>x[[1]])) {
      return(as.numeric(y))
    }
    return(c(v,v))
  }
  dord <- order(d$x)
  # see:
  # http://www.win-vector.com/blog/2017/09/permutation-theory-in-action/
  # http://www.win-vector.com/blog/2017/05/on-indexing-operators-and-composition/
  invPerm <- 1:n
  invPerm[dord] <- 1:n
  d <- d[dord, , drop=FALSE]
  Atot <- matrix(ncol=2,nrow=0,data=0)
  # build order relations to insist on a monotone function transform
  # first all order constraints
  Atot <- cbind(1:(n-1),2:n)
  # then any additional equality constraints to force result to be a
  # function of x
  noIncrease <- which(d$x[1:(n-1)]>=d$x[2:n]-1.0e-6)
  if(length(noIncrease)>0) {
    Atot <- rbind(Atot,cbind(noIncrease+1,noIncrease))
  }
  # sum of squares objective is default if y is specified
  sqIso <- isotone::activeSet(Atot, y=d$y, weights=d$w)
  adjPred <- sqIso$x
  # undo permutation
  adjPred <- adjPred[invPerm]
  adjPred
}
