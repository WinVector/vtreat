
#' Solve a numeric partial pooling problem.
#' 
#' Please see \url{http://www.win-vector.com/blog/2017/09/custom-level-coding-in-vtreat/} and
#' \url{http://www.win-vector.com/blog/2017/09/partial-pooling-for-lower-variance-variable-encoding/}.
#'
#' @param v character variable name
#' @param vcol character, independent or input variable
#' @param y numeric, dependent or outcome variable to predict
#' @param w row/example weights
#' @return scored training data column
#' 
#' @export
#' 
ppCoderN <- function(v, vcol, 
                     y, 
                     w = NULL) {
  if(!requireNamespace("lme4", quietly = TRUE)) {
    stop("vtreat::ppCoderN requires the lme4 package")
  }
  # regression case y ~ vcol
  d <- data.frame(x = vcol,
                  y = y,
                  stringsAsFactors = FALSE)
  m <- lme4::lmer(y ~ (1 | x), data=d, weights=w)
  predict(m, newdata=d)
}

#' Solve a categorical partial pooling problem.
#' 
#' Please see \url{http://www.win-vector.com/blog/2017/09/custom-level-coding-in-vtreat/} and
#' \url{http://www.win-vector.com/blog/2017/09/partial-pooling-for-lower-variance-variable-encoding/}.
#' 
#' @param v character variable name
#' @param vcol character, independent or input variable
#' @param y logical, dependent or outcome variable to predict
#' @param w row/example weights
#' @return scored training data column
#' 
#' @export
#' 
ppCoderC <- function(v, vcol, 
                     y, 
                     w = NULL) {
  if(!requireNamespace("lme4", quietly = TRUE)) {
    stop("vtreat::ppCoderC requires the lme4 package")
  }
  # classification case y ~ vcol
  d <- data.frame(x = vcol,
                  y = y,
                  stringsAsFactors = FALSE)
  m = lme4::glmer(y ~ (1 | x), data=d, weights=w, family=binomial)
  predict(m, newdata=d, type='link')
}