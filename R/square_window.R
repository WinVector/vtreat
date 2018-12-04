
#' Build a square windows variable.
#'
#' Build a square moving average window (KNN in 1d).  This is a high-frequency feature.
#'
#' @param varName character, name of variable
#' @param x numeric input (not empty, no NAs). 
#' @param y numeric or castable to such (same length as x no NAs), output to match
#' @param w numeric positive, same length as x (weights, can be NULL) IGNORED
#' @return segmented y prediction
#' 
#' @examples 
#' 
#' d <- data.frame(x = c(NA, 1:6), y = c(0, 0, 0, 1, 1, 0, 0))
#' square_window("v", d$x, d$y)
#'
#'
#' @export
#' 
square_window <- function(varName, x, y, w = NULL) {
  tryCatch({
    n <- length(x)
    if(n<=10) {
      return(NULL)
    }
    meany = mean(y)
    d <- data.frame(x = x, y = y, orig_idx = seq_len(n))
    d <- d[order(d$x), , drop = FALSE]
    d <- d[!is.na(d$x), , drop = FALSE]
    if(nrow(d)<=10) {
      return(NULL)
    }
    k <- max(min(20, floor(nrow(d)/3)), ceiling(nrow(d)/10000)) # customCoder down-samples at 10000 so no point having more points
    # user a convolution to build running windows
    ones <- rep(1, k)
    num <- stats::convolve(c(rep(0,k), d$y, rep(0,k)), ones, type="filter")
    den <- stats::convolve(c(rep(0,k), rep(1, length(d$x)), rep(0,k)), ones, type="filter")
    rat <- num/den
    d$est <- rat[(length(rat)-length(d$x))/2 + seq_len(length(d$x))]
    res <- rep(meany, n)
    res[d$orig_idx] <- d$est
    res
  },
  error = function(e) { return(NULL) })
}

