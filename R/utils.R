

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





#' Return in-sample linear stats and scaling.
#' @param varName name of variable
#' @param xcol numeric vector of inputs (no NA/NULL/NaN)
#' @param ycol numeric vector of outcomes (no NA/NULL/NaN)
#' @param weights numeric vector of data weights (no NA/NULL/NaN, all>0.0)
linScore <- function(varName,xcol,ycol,weights) {
  if(is.null(weights)) {
    weights <- 1.0+numeric(length(xcol))
  }
  a <- 0.0
  sig <- 1.0
  if(.has.range.cn(xcol) && .has.range.cn(ycol)) {
    lmodel <- stats::lm(formula=y~x,
                        data=data.frame(x=xcol,y=ycol,stringsAsFactors=FALSE),
                        weights=weights)
    a <- lmodel$coefficients[['x']]
    if(is.na(a) || is.infinite(a)) {
      a <- 0.0
    } else {
      smodel <- summary(lmodel)
      if(smodel$fstatistic[['value']]>0) {
        sig <- stats::pf(smodel$fstatistic[['value']],
                         smodel$fstatistic[['numdf']], 
                         smodel$fstatistic[['dendf']],
                         lower.tail=F)
      }
    }
  }
  b <- -.wmean(a*xcol,weights)
  data.frame(varName=varName,
             a=a,b=b,
             sig=sig,
             stringsAsFactors=FALSE)
}



#' return a pseudo R-squared from a 1 variable logistic regression
#' @param varName name of variable
#' @param x numeric (no NAs/NULLs) effective variable
#' @param yC  (no NAs/NULLs) outcome variable
#' @param yTarget scalar target for yC to match (yC==tTarget is goal)
#' @param weights (optional) numeric, non-negative, no NAs/NULLs at least two positive positions
#' @return cross-validated pseudo-Rsquared estimate of a 1-variable logistic regression
catScore <- function(varName,x,yC,yTarget,weights) {
  if(is.null(weights)) {
    weights <- 1.0+numeric(length(x))
  }
  sig <- 1.0
  if(.has.range.cn(x) && 
     .has.range.cn(as.numeric(yC==yTarget))) {
    tf <- data.frame(x=x,y=(yC==yTarget),
                     stringsAsFactors=FALSE)
    suppressWarnings(tryCatch({      
      model <- stats::glm(stats::as.formula('y~x'),
                          data=tf,
                          family=stats::binomial(link='logit'),
                          weights=weights)
      if(model$converged) {
        delta_deviance <- model$null.deviance - model$deviance
        delta_df <- model$df.null - model$df.residual
        pRsq <- 1.0 - model$deviance/model$null.deviance
        if((pRsq>0)&&(delta_deviance>0)) {
          sig <- stats::pchisq(delta_deviance, delta_df, lower.tail=FALSE)
        }
      }
    },
    error=function(e){}))
  }
  data.frame(varName=varName,
             sig=sig,
             stringsAsFactors=FALSE)
}



