
test_custom_coder <- function() {
  set.seed(23525)
  x_codes <- rnorm(5)
  names(x_codes) <- letters[1:length(x_codes)]
  n_rows <- 1000
  d <- data.frame(x1 = sample(names(x_codes), n_rows, replace = TRUE),
                  x2 = rnorm(n_rows),
                  stringsAsFactors = FALSE)
  d$yN <- x_codes[d$x1] + d$x2 - rnorm(nrow(d))
  d$yC <- ifelse(d$yN>0, "Y", "N")
  
  lmCoder <- function(v, vcol, 
                       y, 
                       weights) {
    d <- data.frame(x = vcol,
                    y = y,
                    stringsAsFactors = FALSE)
    m = stats::lm(y ~ x, data=d, weights=weights)
    predict(m, newdata=d)
  }
  
  glmCoder <- function(v, vcol, 
                       y, 
                       weights) {
    d <- data.frame(x = vcol,
                    y = y,
                    stringsAsFactors = FALSE)
    m = stats::glm(y ~ x, data=d, weights=weights, family=binomial)
    predict(m, newdata=d, type='link')
  }
  
  customCoders = list('c.logit.center' = glmCoder,
                      'c.lm.center' = lmCoder,
                      'n.lm.center' = lmCoder,
                      'n.lm.num.center' = lmCoder)
  
  treatplanC = designTreatmentsC(d, 
                                 varlist = c("x1", "x2"),
                                 outcomename = 'yC',
                                 outcometarget= 'Y',
                                 customCoders = customCoders, 
                                 verbose=FALSE)
  RUnit::checkTrue("x1_logit" %in% treatplanC$scoreFrame$varName)
  RUnit::checkTrue("x1_lm" %in% treatplanC$scoreFrame$varName)
  
  treatplanN = designTreatmentsN(d, 
                                 varlist = c("x1", "x2"),
                                 outcomename = 'yN',
                                 customCoders = customCoders, 
                                 verbose=FALSE)
  RUnit::checkTrue("x1_lm" %in% treatplanN$scoreFrame$varName)
  RUnit::checkTrue("x2_lm" %in% treatplanN$scoreFrame$varName)
  
  invisible(NULL)
}
