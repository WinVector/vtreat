
#' @importFrom stats predict
NULL

#' Solve for best non-decreasing fit using isotone regression 
#' (from the "isotone" package \url{https://CRAN.R-project.org/package=isotone}).
#'
#' Return a vector of length y that is a function of x
#' (differs at must where x differs) obeying the same order
#' constraints as x.  This vector is picked as close to
#' y (by square-distance) as possible.
#' 
#' Please see \url{https://github.com/WinVector/vtreat/blob/main/extras/MonotoneCoder.md}.
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
#' if(requireNamespace("isotone", quietly = TRUE)) {
#'    solveNonDecreasing('v', 1:3, c(1,2,1))
#' }
#' 
#' @export
#' 
solveNonDecreasing <- function(varName, x, y, w=NULL) {
  if(!requireNamespace("isotone", quietly = TRUE)) {
    stop("vtreat::solveNonDecreasing requires the isotone package")
  }
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
    stop(paste("solveNonDecreasing", varName, "expect length(y)==length(x)"))
  }
  if(is.null(w)) {
    w <- rep(1.0, n)
  }
  if(!is.numeric(w)) {
    stop(paste("solveNonDecreasing", varName, "expect w numeric"))
  }
  if(length(w)!=n) {
    stop(paste("solveNonDecreasing", varName, "expect length(w)==length(x)"))
  }
  if(min(w)<=0) {
    stop(paste("solveNonDecreasing", varName, "expect positive weights"))
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
  # https://win-vector.com/2017/09/02/permutation-theory-in-action/
  # https://win-vector.com/2017/05/18/on-indexing-operators-and-composition/
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
  if(!is.matrix(Atot)) {
    stop("solveNonDecreasing: !is.matrix(Atot)")
  }
  if(nrow(Atot)<1) {
    stop("solveNonDecreasing: nrow(Atot)<1")
  }
  if(ncol(Atot)!=2) {
    stop("solveNonDecreasing: ncol(Atot)!=2")
  }
  if(min(Atot)<1) {
    stop("solveNonDecreasing: min(Atot)<1")
  }
  if(max(Atot)>length(d$y)) {
    stop("solveNonDecreasing: (max(Atot)>length(d$y)")
  }
  # sum of squares objective is default if y is specified
  sqIso <- isotone::activeSet(Atot, y=d$y, weights=d$w)
  adjPred <- sqIso$x
  # undo permutation
  adjPred <- adjPred[invPerm]
  adjPred
}

#' Solve for best non-increasing fit.
#'
#' Return a vector of length y that is a function of x
#' (differs at must where x differs) obeying the opposite order
#' constraints as x.  This vector is picked as close to
#' y (by square-distance) as possible.
#' 
#' Please see \url{https://github.com/WinVector/vtreat/blob/main/extras/MonotoneCoder.md}.
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
#' 
#' if(requireNamespace("isotone", quietly = TRUE)) {
#'    solveNonIncreasing('v', 1:3, c(1,2,1))
#' }
#' 
#' @export
#' 
solveNonIncreasing <- function(varName, x, y, w=NULL) {
  -solveNonDecreasing(varName, x, -y, w)
}


#' Solve for best single-direction (non-decreasing or non-increasing) fit.
#'
#' Return a vector of length y that is a function of x
#' (differs at must where x differs) obeying the either the same 
#' order contraints or the opposite order
#' constraints as x.  This vector is picked as close to
#' y (by square-distance) as possible.
#' 
#' Please see \url{https://github.com/WinVector/vtreat/blob/main/extras/MonotoneCoder.md}.
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
#' if(requireNamespace("isotone", quietly = TRUE)) {
#'    solveIsotone('v', 1:3, c(1,2,1))
#' }
#' 
#' @export
#' 
solveIsotone <- function(varName, x, y, w=NULL) {
  soln1 <- solveNonDecreasing(varName, x, y, w)
  d1 <- sum((y-soln1)^2)
  soln2 <- solveNonIncreasing(varName, x, y, w)
  d2 <- sum((y-soln2)^2)
  if(d1<=d2) {
    return(soln1)
  }
  return(soln2)
}


