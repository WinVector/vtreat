
# importing all from parallel and then declaring getDefaultCluster() global to
# suppress spurious CRAN NOTE on parallel::getDefaultCluster for R < 3.5.0

#' @import parallel
NULL

utils::globalVariables("getDefaultCluster")

# maybe run parallel
plapply <- function(workList, worker, 
                    ...,
                    parallelCluster = NULL,
                    use_parallel = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), 
                          "vtreat:::plapply")
  use_parallel <- use_parallel &&
    (length(workList)>1) &&
    (requireNamespace("parallel", quietly=TRUE))
  if(use_parallel && 
     is.null(parallelCluster)) {
    if(exists('getDefaultCluster', where=asNamespace('parallel'), mode='function')) {
      parallelCluster <- getDefaultCluster()
    }
  }
  if((!use_parallel) || is.null(parallelCluster)) {
    res <- lapply(workList, worker)
  } else {
    res <- parallel::parLapplyLB(parallelCluster, workList, worker)
  }
  res
}


# rbind a list of dataframes into one
.rbindListOfFrames <- function(frame_list) {
  # catch trivial cases
  if(is.data.frame(frame_list)) {
    return(frame_list)
  }
  frame_list <- Filter(Negate(is.null), frame_list)
  if(length(frame_list)<=1) {
    if(length(frame_list)<=0) {
      return(NULL)
    }
    return(frame_list[[1]])
  }
  # see if a package can supply a fast method
  if(isTRUE(getOption('vtreat.use_data.table_binding', TRUE))) {
    if(requireNamespace("data.table", quietly = TRUE)) {
      return(as.data.frame(data.table::rbindlist(frame_list),
                           stringsAsFactor=FALSE))
    }
  }
  # fall back to base R
  do.call(rbind,frame_list)
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
  if(is.logical(xcol)||is.numeric(xcol)||is.integer(xcol)) { # is.numeric(factor('a')) returns false, but lets not have factors here anyway
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
  if(is.null(weights)) {
    return(mean(x))
  }
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
#' @param numberOfHiddenDegrees optional scalar >= 0 number of additional modeling degrees of freedom to account for.
#' @return significance estiamte and scaling.
#' 
#' @noRd
linScore <- function(varName,xcol,ycol,weights,numberOfHiddenDegrees=0) {
  if(is.null(weights)) {
    weights <- 1.0+numeric(length(xcol))
  }
  a <- 0.0
  rsq <- 0.0
  sig <- 1.0
  if(.has.range.cn(xcol) && .has.range.cn(ycol)) {
    suppressWarnings(tryCatch({ 
      d <- data.frame(x=xcol,y=ycol,stringsAsFactors=FALSE)
      lmodel <- stats::lm(formula=y~x,
                          data=d,
                          weights=weights)
      a <- lmodel$coefficients[['x']]
      if(is.na(a) || is.infinite(a)) {
        a <- 0.0
      } else {
        smodel <- summary(lmodel)
        n <- sum(weights)
        meany <- .wmean(d$y,weights)
        rss1 <- sum(weights*(d$y-meany)^2)
        rss2 <- sum(weights*smodel$residuals^2)
        rsq <- 1 - rss2/rss1
        p1 <- 1
        p2 <- 2 + numberOfHiddenDegrees
        if((n>p2)&&(rss1>rss2)&&(rss1>0)&&(p2>p1)) {
          if(rss2<=0) {
            sig <- 0.0 # summary(lm(y~x,data.frame(x=1:3,y=1:3))) # case
          } else {
            f = ((rss1-rss2)/(p2-p1))/(rss2/(n-p2))
            sig <- stats::pf(f,
                             p2-p1, 
                             n-p2,
                             lower.tail=FALSE)
          }
        }
      }
      },
      error=function(e){}))
  }
  b <- -.wmean(a*xcol,weights)
  data.frame(varName=varName,
             a=a,b=b,
             rsq=rsq,sig=sig,
             stringsAsFactors=FALSE)
}




#' return significnace 1 variable logistic regression
#' @param varName name of variable
#' @param x numeric (no NAs/NULLs) effective variable
#' @param yC  (no NAs/NULLs) outcome variable
#' @param yTarget scalar target for yC to match (yC==tTarget is goal)
#' @param weights (optional) numeric, non-negative, no NAs/NULLs at least two positive positions
#' @param numberOfHiddenDegrees optional scalar >= 0 number of additional modeling degrees of freedom to account for.
#' @return significance estimate of a 1-variable logistic regression
#' 
#' @examples
#' 
#' # d <- data.frame(y=c(1,1,0,0,1,1,0,0,1,1,1,1))
#' # d$x <- seq_len((nrow(d)))
#' # vtreat:::catScore('x',d$x,d$y,1,NULL)
#' 
#' @noRd
catScore <- function(varName,x,yC,yTarget,weights,numberOfHiddenDegrees=0) {
  if(is.null(weights)) {
    weights <- rep(1.0, length(x))
  }
  pRsq <- 0.0
  sig <- 1.0
  a <- 0.0
  if(.has.range.cn(x) && 
     .has.range.cn(as.numeric(yC==yTarget))) {
    tfp <- data.frame(x = x,
                     y = (yC==yTarget),
                     w = weights,
                     stringsAsFactors=FALSE)

    suppressWarnings(tryCatch({      
      model <- stats::glm(stats::as.formula('y~x'),
                          data=tfp,
                          family=stats::binomial(link='logit'),
                          weights=tfp$w)
      if(!model$converged) { 
        # put in small oposite to prevent unbounded models
        # try to fix non-converge
        tfo <- tfp
        tfo$w <- tfo$w*1.0e-5
        tfo$y <- !tfo$y
        tf <- rbind(tfp, tfo)
        model <- stats::glm(stats::as.formula('y~x'),
                            data=tf,
                            family=stats::binomial(link='logit'),
                            weights=tf$w)
      }
      if(model$converged) {
        delta_deviance <- model$null.deviance - model$deviance
        if((model$null.deviance>0)&&(delta_deviance>0)) {
          delta_df <- model$df.null - model$df.residual + numberOfHiddenDegrees
          pRsq <- 1.0 - model$deviance/model$null.deviance
          sig <- stats::pchisq(delta_deviance, delta_df, lower.tail=FALSE)
          a <- model$coefficients[['x']]
          # bg <- model$coefficients[['(Intercept)']]
          # sigmoid <- function(x) { 1/(1+exp(-x)) }
          # max(abs(predict(model,type='response')-sigmoid(a*x+bg))) small
        }
      }
    },
    error=function(e){}))
  }
  b <- -.wmean(a*x,weights)
  data.frame(varName=varName,
             rsq=pRsq,sig=sig,
             a=a,b=b,
             stringsAsFactors=FALSE)
}

vtreat_make_names <- function(nms_in) {
  nms <- gsub("[^[:alnum:]]+", "_", nms_in)
  nms <- make.names(nms, unique = TRUE, allow_ = TRUE)
  nms <- gsub("[^[:alnum:]]+", "_", nms)
  nms
}


.logit <- function(x, eps = 1.0e-6) {
  nms <- names(x)
  x <- pmax(eps, x)
  x <- pmin(1-eps, x)
  v <- log(x/(1-x))
  names(v) <- nms
  v
}


