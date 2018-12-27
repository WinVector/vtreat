
#' Spline numeric variable
#'
#' Return a spline approximation of data.
#'
#' @param varName character, name of variable
#' @param x numeric input (not empty, no NAs). 
#' @param y numeric or castable to such (same length as x no NAs), output to match
#' @param w numeric positive, same length as x (weights, can be NULL)
#' @return segmented y prediction
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
    spline <- stats::smooth.spline(x, y, keep.data = FALSE, cv = TRUE)
    estimate <- stats::predict(spline, x)$y
    attr(estimate, "method") <- "linear"
    return(estimate)
  },
  error = function(e) { return(NULL) })
}

