

# x numeric input 
# y numeric same length as x, output to match
# w numeric positive, same length as x (weights)
# return isotonicly adjusted y (non-decreasing)
# this is a vector of length y that is a function of x
# with at least as many order constraints as x and as close
# to y (by square-distance) as possible.
solveIsotonicProblemW <- function(x, y, w) {
  if(!is.numeric(x)) {
    stop("expect x numeric")
  }
  if(!is.numeric(y)) {
    stop("expect y numeric")
  }
  if(is.null(w)) {
    w <- rep(1.0, length(x))
  }
  if(!is.numeric(w)) {
    stop("expect w numeric")
  }
  if(length(y)!=length(x)) {
    stop("expect length(y)==length(x)")
  }
  if(length(w)!=length(x)) {
    stop("expect length(w)==length(x)")
  }
  if(min(w)<=0) {
    stop("expect positive weights")
  }
  d <- data.frame(x=x, y=y, w=w)
  n <- nrow(d)
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
  # sum of squares objective 
  sqIso <- isotone::activeSet(Atot, y=d$y, weights=d$w)
  adjPred <- sqIso$x
  # undo permutation
  adjPred <- adjPred[invPerm]
  adjPred
}
