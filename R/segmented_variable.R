
# low frequency model
# encode x as lambdas
# x_i = previous + lambda * next_x
# copying forward the variable causes later models to pick up the previous as above.
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
    v[x>=right] <- 1 # copy to rest of the models
    ff[[vname]] <- v
  }
  ff
}

# Fit a piecewise linear function at cut-points
fit_segments <- function(x, y, k,
                         ...,
                         w = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::fit_segments")
  if(is.null(w)) {
    w = numeric(length(x)) + 1
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
  idxs <- sort(unique(c(1, round(seq(1, n, length.out = k)))))
  idxs <- pmin(n, pmax(1, idxs))
  idxs <- idxs[idxs<n]
  xs <- sort(unique(xs[idxs]))
  xs <- xs[xs<maxx]
  ff <- encode_x_as_lambdas(x, minx, maxx, xs)
  vars <- colnames(ff)
  ff$y <- y
  f <- paste("y", paste(c("0", vars), collapse = " + "), sep = " ~ ")
  f <- as.formula(f)
  model <- stats::lm(f, data = ff, weights = w)
  coef <- model$coefficients
  coef[is.na(coef)] <- 0
  list(k =k,
       minx = minx, 
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


#' Solve as piecewise linear problem, numeric target.
#'
#' Return a vector of length y that is a piecewise function of x.
#' This vector is picked as close to
#' y (by square-distance) as possible for a set of x-only determined
#' cut-points.  Cross-validates for a good number of segments.
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
solve_piecewise <- function(varName, x, y, w = NULL) {
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
    if(n<=20) {
      # too small, 1 or 2 segments
      k <- 2
    } else {
      # cross-val for a good k
      ks <- c(1, 2, 4, 8, 16, 32, 64)
      ks <- ks[ks<=min(n/10, nunique)]
      if(length(ks)<1) {
        return(NULL)
      }
      is_test <- seq_len(n) %in% sample.int(n, n, replace = FALSE)[seq_len(floor(n/2))]
      xvals <- vapply(
        ks,
        function(k) {
          model <- fit_segments(x[!is_test], y[!is_test], k=k, w=w[!is_test])
          preds <- pred_segs(model, x[is_test])
          mean((y[is_test] - preds)^2)
        }, numeric(1))
      idx <- which.min(xvals)
      k <- ks[[idx]]
      # names(xvals) <- as.character(ks)
      # print(xvals)
      # print(k)
    }
    model <- fit_segments(x, y, k=k, w=w)
    estimate <- pred_segs(model, x)
    approx_table <- data.frame(predXs = sort(unique(c(min(x), model$xs, max(x)))))
    approx_table$predYs <- pred_segs(model, approx_table$predXs)
    attr(estimate, "approx_table") <- approx_table
    attr(estimate, "method") <- "linear"
    return(estimate)
  },
  error = function(e) { return(NULL) })
}


#' Solve as piecewise logit problem, categorical target.
#'
#' Return a vector of length y that is a piecewise function of x.
#' This vector is picked as close to
#' y (by square-distance) as possible for a set of x-only determined
#' cut-points.  Cross-validates for a good number of segments.
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
solve_piecewisec <- function(varName, x, y, w = NULL) {
  v <- solve_piecewisec(varName = varName, 
                     x = x, y = y , w = w)
  # don't copy over approx table as we are piecewise logit, not piecewise linear.
  .logit(v) - .logit(.wmean(y, w))
}
