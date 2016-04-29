

# maybe run parallel
plapply <- function(workList,worker,parallelCluster) {
  if(is.null(parallelCluster) || (!requireNamespace("parallel",quietly=TRUE))) {
    res <- lapply(workList,worker)
  } else {
    res <- parallel::parLapply(parallelCluster,workList,worker)
  }
  res
}


# rbind a list of dataframes into one
.rbindListOfFrames <- function(rowlist) {
  # catch trivial cases
  if(length(rowlist)<=1) {
    if(length(rowlist)<=0) {
      return(NULL)
    }
    return(rowlist[[1]])
  }
  # see if a library can supply a fast method
  if(requireNamespace("dplyr", quietly = TRUE)) {
    return(as.data.frame(dplyr::bind_rows(rowlist),
                         stringsAsFactor=FALSE))
  }
  # fall back to base R
  do.call(rbind,rowlist)
}



# take in a column and return a column that is safely one of the primative
# types: numeric, character 
# if the column is more exotic (multiple classes, AsIs, other issues) return null
# protects downstream code from surprises
# given how diverse R types are this is no way we can defend again everything, 
# this is supposed to be a consistent defense against common unexpected convertions
# see: http://www.win-vector.com/blog/2015/04/what-can-be-in-an-r-data-frame-column/
.cleanColumn <- function(xcol,expectedLength) {
  if(is.null(xcol)) {
    return(NULL)
  }
  if("AsIs" %in% class(xcol)) {
    return(NULL)
  }
  xcol <- as.vector(xcol) # defend against arrays, converts POSIXct to numeric, but not POSIXlt
  if(is.null(xcol)||(length(xcol)!=expectedLength)) {
    return(NULL)
  }
  if("POSIXt" %in% class(xcol)) {
    return(as.numeric(xcol))
  }
  if(length(class(xcol))!=1) {
    return(NULL)
  }
  if(class(xcol) %in% c('list','AsIs')) {
    return(NULL)
  }
  if(is.factor(xcol)||is.character(xcol)) {
    # factor, character case
    return(as.character(xcol))
  }
  if(is.logical(xcol)||is.numeric(xcol)) { # is.numeric(factor('a')) returns false, but lets not have factors here anyway
    # logical (treat as an indicator), integer, numeric case
    # is.numeric(1:10) returns TRUE (integers get into this block)
    return(as.numeric(xcol))
  }
  if(is.atomic(xcol)) {
    return(as.character(xcol))
  }
  return(NULL)
}




.is.bad <- function(v) { is.na(v) | is.nan(v) | (!is.finite(v)) }

# check if a vector has more than one value
.has.range <- function(v) {
  lv <- length(v)
  if(lv<=1) {
    return(FALSE)
  }
  nna <- sum(is.na(v))
  if(nna>=lv) {
    return(FALSE)
  }
  if(nna>0) {
    return(TRUE)
  }
  match1 <- v==v[[1]]
  sum(match1)<lv
}

# check if a clean numeric vector has more than one value
.has.range.cn <- function(v) {
  lv <- length(v)
  if(lv<=1) {
    return(FALSE)
  }
  return(max(v)>min(v))
}

# weighted mean
# assumes non-zero lists of clean entries, weights all >0
.wmean <- function(x,weights) {
  sum(x*weights)/sum(weights)
}




# build a weighted table
.wTable <- function(v1,v2,wts) {
  v1 <- as.character(v1)
  v2 <- as.character(v2)
  v1Levs <- sort(unique(v1))
  v2Levs <- sort(unique(v2))
  if(is.null(wts)) {
    wts <- rep(1.0,length(v1))
  } else {
    wts <- as.numeric(wts)
  }
  d <- data.frame(v1=v1,
                  v2=v2,
                  weights=wts,
                  stringsAsFactors=FALSE)
  agg <- aggregate(weights~v1+v2,data=d,FUN=sum)
  mat <- matrix(data=0,nrow=length(v1Levs),ncol=length(v2Levs))
  rownames(mat) <- v1Levs
  colnames(mat) <- v2Levs
  for(ii in seq_len(nrow(agg))) {
    mat[agg$v1[[ii]],agg$v2[[ii]]] <- agg$weights[[ii]]
  }
  as.table(mat)
}



# weighted PRESS statistic of a weighted mean
# so in this case it is sum((y_i - meanAllBut(y,i))^) where mean is computed of all but the i'th datum
# y numeric, no NAs/NULLS
# weights numeric, non-negative, no NAs/NULLs at least two positive positions
# all vectors same length
#'
#' of all but the i-th y.  Useful for normalizing PRESS style statistics.
#' @param y values to average (should not have NAs).
#' @param weights data weighing (should not have NAs, be non-negative and not all zero).
#' @return a vector of length(y) where the i-th entry is the weighted mean 
#' @seealso \code{\link{pressStatOfBestLinearFit}}
#' @export
hold1OutMeans <- function(y,weights) {
  # get per-datum hold-1 out grand means
  sumY <- sum(y*weights)
  sumW <- sum(weights)
  meanP <- (sumY - y*weights)/(sumW - weights)
  meanP[is.na(meanP)] <- 0.5
  meanP
}


# y: numeric vector no null/NAs
# w: numeric vector same length as y, no negative/null/NAs at least 2 position non-zero
# normalizationStrat: 'none': no normalization (traditional PRESS), 'total': divide by total variation, 'holdout': divide by 1-hold out variation (PRESS-line, larger than total variation)
.PRESSnormalization <- function(normalizationStrat,y,weights) {
  res <- switch(normalizationStrat,
                none = 1.0,
                total = { meanY <- .wmean(y,weights); sum(weights*(y-meanY)^2) },
                holdout = { meanH <- hold1OutMeans(y,weights); sum(weights*(y-meanH)^2) }
  )
  # switch default doesn't get called if use passes in a numeric
  if(is.null(res)) {
    stop("normalizationStrat must be one of 'none', 'total', or 'holdout'")
  }
  res
}


#' Compute the PRESS R-squared statistic of a 1-variable linear model
#' @param x numeric (no NAs/NULLs) effective variable
#' @param y numeric (no NAs/NULLs) outcome variable
#' @param weights (optional) numeric, non-negative, no NAs/NULLs at least two positive positions
#' @param normalizationStrat (optional) 'none': no normalization (traditional PRESS), 'total': divide by total variation, 'holdout': divide by 1-hold out variation (PRESS-line, larger than total variation)
#' @return PRESS statistic of model y ~ a*x + b divided by pressStatOfBestConstant(y,weights), it is an R-squared so 1 is good 0 is bad
#' @seealso \code{\link{hold1OutMeans}} 
pressStatOfBestLinearFit <- function(x,y,weights=c(),normalizationStrat='total') {
  n <- length(x)
  if(n<=1) {
    return(list(rsq=0.0,sig=1.0))
  }
  if(!.has.range.cn(x)) {
    return(list(rsq=0.0,sig=1.0))
  }
  if(!.has.range.cn(y)) {
    return(list(rsq=1.0,sig=0.0))
  }
  if(is.null(weights)) {
    weights <- 1.0+numeric(n)
  }
  error <- 0.0
  # get per-datum hold-1 out grand means (used for smoothing and fallback)
  meanP <- hold1OutMeans(y,weights)
  a <- matrix(data=0,nrow=2,ncol=2)
  a[1,1] <- 1.0e-5
  a[2,2] <- 1.0e-5
  b <- matrix(data=0,nrow=2,ncol=1)
  for(i in seq_len(n)) {
    xi <- x[i]
    yi <- y[i]
    wi <- weights[i]
    a[1,1] <- a[1,1] + wi*1.0
    a[1,2] <- a[1,2] + wi*xi
    a[2,1] <- a[2,1] + wi*xi
    a[2,2] <- a[2,2] + wi*xi*xi
    b[1,1] <- b[1,1] + wi*yi
    b[2,1] <- b[2,1] + wi*xi*yi
  }
  aM <- matrix(data=0,nrow=2,ncol=2)
  bM <- matrix(data=0,nrow=2,ncol=1)
  for(i in seq_len(n)) {
    xi <- x[i]
    yi <- y[i]
    wi <- weights[i]
    aM[1,1] <- a[1,1] - wi*1.0
    aM[1,2] <- a[1,2] - wi*xi
    aM[2,1] <- a[2,1] - wi*xi
    aM[2,2] <- a[2,2] - wi*xi*xi
    bM[1,1] <- b[1,1] - wi*yi
    bM[2,1] <- b[2,1] - wi*xi*yi
    ye <- meanP[i]  # const fn solution, for fallback
    tryCatch(
      ye <- sum(solve(aM,bM) * c(1,xi)),
      error = function(e) {})
    error <- error + wi*(yi-ye)^2
  }
  eConst <- .PRESSnormalization(normalizationStrat,y,weights)
  pRsq <- 1.0 - error/eConst
  sig <- 1.0
  if(error<eConst) {
    Fstat <- (eConst-error)*(n-2)/(error)
    sig <- stats::pf(Fstat,1,n-2,lower.tail=F)
  }
  list(rsq=pRsq,sig=sig)
}




#' return a pseudo R-squared from a 1 variable logistic regression
#' @param x numeric (no NAs/NULLs) effective variable
#' @param yC  (no NAs/NULLs) outcome variable
#' @param yTarget scalar target for yC to match (yC==tTarget is goal)
#' @param weights (optional) numeric, non-negative, no NAs/NULLs at least two positive positions
#' @return cross-validated pseudo-Rsquared estimate of a 1-variable logistic regression
#' @seealso \code{\link{hold1OutMeans}} 
catScore <- function(x,yC,yTarget,weights=c()) {
  n <- length(x)
  # grab some special cases
  if(n<=1) {
    return(list(pRsq=0.0,sig=1.0))
  }
  if(!.has.range.cn(x)) {
    return(list(pRsq=0.0,sig=1.0))
  }
  if(!.has.range(yC)) {
    return(list(pRsq=1.0,sig=0.0))
  }
  if(is.null(weights)) {
    weights <- 1.0+numeric(n)
  }
  tf <- data.frame(x=x,y=(yC==yTarget),
                   stringsAsFactors=FALSE)
  origOpt <- options()
  options(warn=-1)
  tryCatch({      
    model <- stats::glm(stats::as.formula('y~x'),
                        data=tf,
                        family=stats::binomial(link='logit'),
                        weights=weights)
    if(model$converged) {
      delta_deviance = model$null.deviance - model$deviance
      delta_df = model$df.null - model$df.residual
      sig <- 1.0
      pRsq <- 1.0 - model$deviance/model$null.deviance
      if(pRsq>0) {
        sig <- stats::pchisq(delta_deviance, delta_df, lower.tail=FALSE)
      }
      return(list(pRsq=pRsq,sig=sig))
    }
  },
  error=function(e){})
  return(list(pRsq=0.0,sig=1.0))
}



