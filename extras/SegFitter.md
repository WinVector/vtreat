SegFitter
================

``` r
library("ggplot2")
```

``` r
# encode xs as lambdas
encode_x_as_lambdas <- function(x, minx, maxx, xs) {
  n <- length(x)
  k <- length(xs)
  x[is.na(x)] <- (minx+maxx)/2
  x <- pmin(maxx, pmax(minx, x))
  ff <- data.frame("intercept" = rep(1, n))
  for(ki in seq_len(k)) {
    vname <- paste0("lambda_", sprintf("%05g", ki))
    v <- numeric(n)
    left <- xs[[ki]]
    if(ki<k) {
      right <- xs[[ki+1]]
    } else {
      right <- maxx
    }
    v <- (x-left)/(right-left)
    v[x<left] <- 0
    v[x>=right] <- 1 # copy to res of the models
    ff[[vname]] <- v
  }
  ff
}

#' Fit a piecewise linear function at cut-points
fit_segments <- function(x, y, w = NULL) {
  if(is.null(w)) {
    w = numeric(length(x))
  }
  meany = mean(y)
  missing_pred = meany
  na_posns = is.na(x)
  if(sum(na_posns)>20) {
    missing_pred = mean(y[na_posns])
  }
  x <- x[!na_posns]
  y <- y[!na_posns]
  w <- w[!na_posns]
  n <- length(x)
  minx <- min(x)
  maxx <- max(x)
  xs <- sort(x)
  idxs <- sort(unique(c(1, round(seq(1, n, by = n^(2/3))))))
  idxs <- pmin(n, pmax(1, idxs))
  idxs <- idxs[idxs<n]
  xs <- sort(unique(xs[idxs]))
  xs <- xs[xs<maxx]
  ff <- encode_x_as_lambdas(x, minx, maxx, xs)
  vars <- colnames(ff)
  ff$y <- y
  f <- paste("y", paste(c("0", vars), collapse = " + "), sep = " ~ ")
  model <- lm(f, data = ff)
  coef <- model$coefficients
  coef[is.na(coef)] <- 0
  list(minx = minx, 
       maxx = maxx,
       xs = xs,
       meany = meany,
       missing_pred = missing_pred,
       coef = coef)
}

pred_segs <- function(model, x) {
  ff <- encode_x_as_lambdas(x, model$min, model$maxx, model$xs)
  preds <- as.matrix(ff) %*% model$coef
  preds[is.na(x)] <- model$missing_pred
  preds
}


#' Solve as piecewise linear problem.
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
#' solveNonDecreasing('v', 1:3, c(1,2,1))
#' # [1] 1.0 1.5 1.5
#' 
solve_piecewise <- function(varName, x, y, w=NULL) {
  model <- fit_segments(x, y, w=w)
  pred_segs(model, x)
}

customCoders = list('c.PiecewiseV.num' = solve_piecewise,
                    'n.PiecewiseV.num' = solve_piecewise)
```

``` r
d <- data.frame(x = seq(0, 15, by = 0.01))
d$y_ideal <- sin(d$x)
d$y <- d$y_ideal + 0.5*rnorm(nrow(d))
d$is_train <- runif(nrow(d))>=0.2

ggplot(data=d) +
  geom_point(aes(x = x, y = y), alpha=0.5) + 
  geom_line(aes(x = x, y = y_ideal), color = "lightblue")
```

![](SegFitter_files/figure-markdown_github/example-1.png)

``` r
cfe <- vtreat::mkCrossFrameNExperiment(d[d$is_train, , drop=FALSE], 
                                        'x', 'y',
                                        customCoders = customCoders,
                                        verbose = FALSE)
cfe$treatments
```

    ##        varName varMoves         rsq           sig needsSplit
    ## 1 x_PiecewiseV     TRUE 0.655368743 2.136152e-279       TRUE
    ## 2      x_clean     TRUE 0.000658704  3.743851e-01      FALSE
    ##   extraModelDegrees origName       code
    ## 1              1200        x PiecewiseV
    ## 2                 0        x      clean

``` r
prepared <- vtreat::prepare(cfe$treatments, d)
d$x_PiecewiseV <- prepared$x_PiecewiseV

model <- fit_segments(d$x, d$y)
d$pred <- pred_segs(model, d$x)
ggplot(data=d) +
 # geom_point(aes(x = x, y = y)) + 
  geom_line(aes(x = x, y = y_ideal), color = "lightblue") + 
  geom_line(aes(x = x, y = x_PiecewiseV))
```

![](SegFitter_files/figure-markdown_github/example-2.png)

``` r
WVPlots::ScatterHist(d[d$is_train, , drop=FALSE], 
                     "x_PiecewiseV", "y",
                     "x_PiecewiseV versus observed y on train",
                     smoothmethod = "identity",
                     estimate_sig = TRUE)
```

![](SegFitter_files/figure-markdown_github/example-3.png)

``` r
WVPlots::ScatterHist(d[d$is_train, , drop=FALSE], 
                     "x_PiecewiseV", "y_ideal",
                     "x_PiecewiseV versus ideal y on train",
                     smoothmethod = "identity",
                     estimate_sig = TRUE)
```

![](SegFitter_files/figure-markdown_github/example-4.png)

``` r
WVPlots::ScatterHist(d[!d$is_train, , drop=FALSE], 
                     "x_PiecewiseV", "y",
                     "x_PiecewiseV versus observed y on test",
                     smoothmethod = "identity",
                     estimate_sig = TRUE)
```

![](SegFitter_files/figure-markdown_github/example-5.png)

``` r
WVPlots::ScatterHist(d[!d$is_train, , drop=FALSE], 
                     "x_PiecewiseV", "y_ideal",
                     "x_PiecewiseV versus ideal y on test",
                     smoothmethod = "identity",
                     estimate_sig = TRUE)
```

![](SegFitter_files/figure-markdown_github/example-6.png)
