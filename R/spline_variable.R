

mk_spline_eval_fn <- function(spline) {
  force(spline)
  function(x) {
    stats::predict(spline, x)$y
  }
}

#' Spline numeric variable
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
    nknots <- min(nunique, 1000)
    spline <- stats::smooth.spline(x, y, 
                                   w = w,
                                   nknots = nknots,
                                   keep.data = FALSE, 
                                   keep.stuff = FALSE,
                                   cv = TRUE)$fit
    estimate <- stats::predict(spline, x)$y
    attr(estimate, "eval_fn") <- mk_spline_eval_fn(spline)
    attr(estimate, "method") <- "linear"
    return(estimate)
  },
  error = function(e) { return(NULL) })
}

