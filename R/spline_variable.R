

mk_spline_eval_fn <- function(spline, mn_x, mx_x) {
  force(spline)
  force(mn_x)
  force(mx_x)
  function(x) {
    x <- pmax(x, mn_x)
    x <- pmin(x, mx_x)
    stats::predict(spline, x)$y
  }
}

#' Spline variable numeric target.
#'
#' Return a spline approximation of data.
#'
#' @param varName character, name of variable
#' @param x numeric input (not empty, no NAs). 
#' @param y numeric or castable to such (same length as x no NAs), output to match
#' @param w numeric positive, same length as x (weights, can be NULL)
#' @return spline y prediction
#'
#'
#' @export
#' 
spline_variable <- function(varName, x, y, w = NULL) {
  tryCatch({
    n <- length(x)
    if(n<=2) {
      return(NULL)
    }
    nunique <- length(unique(x))
    if(nunique<=2) {
      return(NULL)
    }
    if(is.null(w)) {
      w <- numeric(n) + 1
    }
    d <- data.frame(x = x, y = y, w = w, orig_idx = seq_len(n))
    d <- d[order(d$x, stats::runif(length(d$x))), , drop = FALSE]
    nknots <- min(nunique/2, 100)
    spline <- stats::smooth.spline(d$x, d$y, 
                                   w = d$w,
                                   nknots = nknots,
                                   keep.data = FALSE, 
                                   keep.stuff = FALSE,
                                   cv = TRUE)$fit
    estimate <- stats::predict(spline, x)$y
    attr(estimate, "eval_fn") <- mk_spline_eval_fn(spline, min(d$x), max(d$x))
    attr(estimate, "method") <- "linear"
    return(estimate)
  },
  error = function(e) { return(NULL) })
}

#' Spline variable categorical target.
#'
#' Return a spline approximation of the change in log odds.
#'
#' @param varName character, name of variable
#' @param x numeric input (not empty, no NAs). 
#' @param y numeric or castable to such (same length as x no NAs), output to match
#' @param w numeric positive, same length as x (weights, can be NULL)
#' @return spline y prediction
#'
#'
#' @export
#' 
spline_variablec <- function(varName, x, y, w = NULL) {
  v <- spline_variable(varName = varName, 
                       x = x, y = y , w = w)
  .logit(v) - .logit(.wmean(y, w))
}
