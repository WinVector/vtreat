# variable treatments type def: list { origvar, newvars, f(col,args), args, treatmentName, scales } can share orig var


#' @importFrom stats aggregate anova as.formula binomial glm lm lm.wfit pchisq pf quantile
NULL


#' Return a list of new treated variable names (coresponding to optional original variable names)
#' @param treatments the treatments slot from a treatmentplan object
#' @param origVarNames optional restrict to only derived variable originating from these original variables (null is no restriction)
#' @return list of new treated variable names
#' @seealso \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}}
#' @examples
#' dTrainN <- data.frame(x=c('a','a','a','a','b','b','b'),
#'     z=c(1,2,3,4,5,6,7),y=c(0,0,0,1,0,1,1))
#' dTestN <- data.frame(x=c('a','b','c',NA),
#'     z=c(10,20,30,NA))
#' treatmentsN = designTreatmentsN(dTrainN,colnames(dTrainN),'y')
#' 
getNewVarNames <- function(treatments,origVarNames=c()) {
  resCount <- 0
  for(ti in treatments) {
     if( is.null(origVarNames) || (ti$origvar %in% origVarNames)) {
        resCount <- resCount + length(ti$newvars)
     }
  }
  names <- vector('list',resCount)
  j <- 1
  for(ti in treatments) {
     if( is.null(origVarNames) || (ti$origvar %in% origVarNames)) {
        for(ni in ti$newvars) {
          names[[j]] <- list(new=ni,orig=ti$origvar)
          j <- j + 1
        }
     }
  }
  names
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
    # logical, factor, character case
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

.vtreatA <- function(vtreat,xcol,scale,doCollar) {
  dout <- as.data.frame(vtreat$f(xcol,vtreat$args,doCollar),
                        stringsAsFactors=FALSE)
  colnames(dout) <- vtreat$newvars
  if(scale) {
    for(j in seq_along(vtreat$scales$a)) {
      dout[[j]] <- dout[[j]]*vtreat$scales$a[[j]] + vtreat$scales$b[[j]]
    }
  }
  dout
}

# colNames a subset of treated variable names
.vtreatList <- function(treatments,dframe,colNames,scale,doCollar,
                        parallelCluster) {
  resCounts <- vapply(treatments,function(ti) { 
    length(intersect(colNames,ti$newvars))
  },numeric(1))
  toProcess <- treatments[resCounts>0]
  procWorker <- function(ti) {
    xcolOrig <- dframe[[ti$origvar]]
    nRows <- length(xcolOrig)
    xcolClean <- .cleanColumn(xcolOrig,nRows)
    if(is.null(xcolClean)) {
      return(paste('column',ti$origvar,
                 'is not a type/class vtreat can work with (',class(xcolOrig),')'))
    }
    if(!is.null(ti$origColClass)) {
      curColClass <- class(xcolClean)
      if(curColClass!=ti$origColClass) {
        return(paste('column',ti$origvar,'expected to convert to ',
                   ti$origColClass,'saw',class(xcolOrig),curColClass))
      }
    }
    .vtreatA(ti,xcolClean,scale,doCollar)
  }
  if(is.null(parallelCluster)) {
    gs <- lapply(toProcess,procWorker)
  } else {
    gs <-  parallel::parLapply(parallelCluster,toProcess,procWorker)
  }
  # pass back first error
  for(gi in gs) {
    if(is.character(gi)) {
      stop(gi)
    }
  }
  cols <- vector('list',length(colNames))
  names(cols) <- colNames
  for(ii in seq_len(length(toProcess))) {
    ti <- toProcess[[ii]]
    gi <- gs[[ii]]
    wants <- intersect(colNames,ti$newvars)
    for(vi in wants) {
      cols[[vi]] <- gi[[vi]]
    }
  }
  as.data.frame(cols,stringsAsFactors=FALSE)
}


#'
#' Original variable name from a treatmentplan$treatment item.
#' @param x vtreatment item.
#' @seealso \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}} 
#' @export
#' 
vorig <- function(x) { x$origvar }


#'
#' New treated variable names from a treatmentplan$treatment item.
#' @param x vtreatment item
#' @seealso \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}} 
#' @export
vnames <- function(x) { x$newvars }

#'
#' Display treatment plan.
#' @param x treatment plan
#' @param ... additional args (to match general signature).
#' @export
format.vtreatment <- function(x,...) { paste(
  'vtreat \'',x$treatmentName,
  '\'(\'',x$origvar,'\'->',x$origColClass,'->\'',
  paste(x$newvars,collapse='\',\''),
  '\')',sep='') }

#'
#' Print treatmentplan.
#' @param x treatmentplan
#' @param ... additional args (to match general signature).
#' @seealso \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}} \code{\link{prepare}}
#' @export
print.vtreatment <- function(x,...) { 
  print(format.vtreatment(x),...) 
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




# xcol numeric vector of inputs (no NA/NULL/NaN)
# ycol numeric vector of outcomes (no NA/NULL/NaN)
# numeric vector of data weights (no NA/NULL/NaN, all>0.0)
.getScales <- function(xcol,ycol,weights) {
  lmatx <- matrix(data=0.0,nrow=length(ycol),ncol=2)
  lmatx[,1] <- 1
  lmatx[,2] <- xcol
  lmaty <- matrix(data=0.0,nrow=length(ycol),ncol=1)
  meany <- .wmean(ycol,weights)
  lmaty[,1] <- ycol-meany
  model <- stats::lm.wfit(lmatx,lmaty,weights)
  a <- 0.0
  b <- 0.0
  if(!is.na(model$coefficients[[2]])) {
    a <- model$coefficients[[2]]
    if(a!=0.0) {
      if(!is.na(model$coefficients[[1]])) {
        b <- model$coefficients[[1]]
      }
    }
  }
  list(a=a,b=b)
}



# pass a variable through (removing NAs) (should only by used for numerics)
.passThrough <- function(col,args,doCollar) {
  treated <- as.numeric(col)
  treated[.is.bad(treated)] <- args$nadist
  if(doCollar) {
     treated[treated<args$cuts[[1]]] <- args$cuts[[1]]
     treated[treated>args$cuts[[2]]] <- args$cuts[[2]]
  }
  treated
}

.mkPassThrough <- function(origVarName,xcol,ycol,weights,collarProb) {
  origColClass <- class(xcol)
  xcol <- as.numeric(xcol)
  napositions <- .is.bad(xcol)
  nna <- sum(napositions)
  if(nna>=length(xcol)) {
    return(c())
  }
  if(collarProb>0.0) {
     cuts <- as.numeric(stats::quantile(xcol[!napositions],
                                        probs=c(collarProb,1-collarProb)))
  } else {
     cuts <- c(min(xcol[!napositions]),max(xcol[!napositions]))
  }
  nadist <- .wmean(xcol[!napositions],weights[!napositions])
  xcol[napositions] <- nadist
  if(max(xcol)<=min(xcol)) {
    return(c())
  }
  treatment <- list(origvar=origVarName,origColClass=origColClass,
                    newvars=make.names(paste(origVarName,'clean',sep='_')),
                    f=.passThrough,
                    args=list(nadist=nadist,cuts=cuts),
                    treatmentName='Scalable pass through')
  class(treatment) <- 'vtreatment'
  treatment$scales <- .getScales(xcol,ycol,weights)
  treatment
}




# return if a variable is NA
.isBAD <- function(col,args,doCollar) {
  treated <- ifelse(.is.bad(col),1.0,0.0)
  treated
}

.mkIsBAD <- function(origVarName,xcol,ynumeric,weights) {
  origColClass <- class(xcol)
  badIDX <- .is.bad(xcol)
  nna <- sum(badIDX)
  if((nna<=0)||(nna>=length(xcol))) {
    return(c())
  }
  if(.wmean(ynumeric[badIDX],weights[badIDX])==.wmean(ynumeric[!badIDX],weights[!badIDX])) {
    return(c())
  }
  treatment <- list(origvar=origVarName,origColClass=origColClass,
                    newvars=make.names(paste(origVarName,'isBAD',sep='_')),
                    f=.isBAD,
                    args=list(),
                    treatmentName='is.bad')
  class(treatment) <- 'vtreatment'
  treatment$scales <- .getScales(ifelse(badIDX,1.0,0.0),ynumeric,weights)
  treatment
}


# pre-transform categorical column
# convert it to character, convert NA to "NA"
.preProcCat <- function(col,levRestriction) {
  origna <- is.na(col)
  # don't use blank as a key and get into defendable level space
  col <- paste('x',as.character(col))
  col[origna] <- 'NA'
  if(!is.null(levRestriction)) {
    # map rare levels to a new special level
    rares <- !(col %in% levRestriction$safeLevs)
    zaps <- col %in% levRestriction$supressedLevs
    col[rares] <- 'rare'
    col[zaps] <- 'zap'
  }
  col
}


# determine non-rare and significant levels for numeric/regression target
.safeLevels <- function(vcolin,yNumeric,weights,rareCount,rareSig) {
  vcol <- .preProcCat(vcolin,c())
  # first: keep only levels with enough weighted counts
  counts <- tapply(weights,vcol,sum)
  safeLevs <- names(counts)[(counts>rareCount) & (counts<sum(weights))]
  supressedLevs <- character(0)
  if(length(safeLevs)>0) {
    # second: keep only levels that look significantly different than grand mean
    aovCalc <- function(level) {
      m <- stats::lm(yNumeric~vcol==level,weights = weights)
      stats::anova(m)[1,'Pr(>F)']
    }
    sigs <- vapply(safeLevs,aovCalc,numeric(1))
    supressedLevs <- safeLevs[sigs>rareSig]
  }
  list(safeLevs=safeLevs,supressedLevs=supressedLevs)
}



# return categorical indicators
.catInd <- function(col,args,doCollar) {
  col <- .preProcCat(col,args$levRestriction)
  nres <- length(args$tracked)
  vals <- vector('list',nres)
  for(j in seq_len(nres)) {
    vi <- ifelse(col==args$tracked[j],1.0,0.0) 
    vals[[j]] <- vi
  }
  vals
}

# build categorical indicators
.mkCatInd <- function(origVarName,vcolin,ynumeric,minFraction,maxMissing,levRestriction,weights) {
  origColClass <- class(vcolin)
  vcol <- .preProcCat(vcolin,levRestriction)
  counts <- tapply(weights,vcol,sum)
  totMass <- sum(counts)
  tracked <- names(counts)[counts/totMass>=minFraction]
  tracked <- setdiff(tracked,'zap') # don't let zap group code
  if(length(tracked)<=0) {
    return(c())
  }
  counts <- counts[tracked]
  missingMass <- 1 - sum(counts)/totMass
  if(missingMass>maxMissing) {
    return(c())
  }
  treatment <- list(origvar=origVarName,origColClass=origColClass,
                    newvars=make.names(paste(origVarName,'lev',tracked,sep="_"),unique=TRUE),
                    f=.catInd,
                    args=list(tracked=tracked,
                              levRestriction=levRestriction),
                    treatmentName='Categoric Indicators')
  class(treatment) <- 'vtreatment'
  pred <- treatment$f(vcolin,treatment$args)
  nvar <- length(pred)
  treatment$scales <- list('a'=rep(1.0,nvar),'b'=rep(0.0,nvar))  
  for(j in seq_len(nvar)) {
    scales <- .getScales(pred[[j]],ynumeric,weights)
    treatment$scales$a[j] <- scales$a
    treatment$scales$b[j] <- scales$b
  }
  treatment
}


# apply a numeric impact model
# replace level with .wmean(x|category) - .wmean(x)
.catNum <- function(col,args,doCollar) {
  col <- .preProcCat(col,args$levRestriction)
  novel <- !(col %in% names(args$scores))
  keys <- col
  pred <- numeric(length(col))
  if(length(args$scores)>0) {
    keys[novel] <- names(args$scores)[[1]]   # just to prevent bad lookups
    pred <- as.numeric(args$scores[keys]) 
  }
  # mean delta impact averaged over all possibilities, should be zero in scaled mode, mean dist in unscaled
  pred[novel] <- 0.0 
  pred
}

# build a numeric impact model
# see: http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/
.mkCatNum <- function(origVarName,vcolin,rescol,smFactor,levRestriction,weights) {
  origColClass <- class(vcolin)
  vcol <- .preProcCat(vcolin,levRestriction)
  baseMean <- .wmean(rescol,weights)
  num <- tapply(rescol*weights,vcol,sum)
  den <- tapply(weights,vcol,sum)
  scores <- (num+smFactor*baseMean)/(den+smFactor)-baseMean
  scores <- as.list(scores)
  scores <- scores[names(scores)!='zap'] # don't let zap code
  treatment <- list(origvar=origVarName,origColClass=origColClass,
                    newvars=make.names(paste(origVarName,'catN',sep='_')),
                    f=.catNum,
                    args=list(scores=scores,
                              levRestriction=levRestriction),
                    treatmentName='Scalable Impact Code')
  pred <- treatment$f(vcolin,treatment$args)
  class(treatment) <- 'vtreatment'
  treatment$scales <- .getScales(pred,rescol,weights)
  treatment
}



# apply a classification impact model
# replace level with log(.wmean(x|category)/.wmean(x))
.catBayes <- function(col,args,doCollar) {
  col <- .preProcCat(col,args$levRestriction)
  novel <- !(col %in% names(args$logLift))
  keys <- col
  pred <- numeric(length(col))
  if(length(args$logLift)>0) {
     keys[novel] <- names(args$logLift)[[1]]  # just to prevent bad lookups
     pred <- as.numeric(args$logLift[keys]) 
  }
  pred[novel] <- 0.0
  pred
}

# build a classification impact model
# see: http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/
.mkCatBayes <- function(origVarName,vcolin,rescol,resTarget,smFactor,levRestriction,weights) {
  origColClass <- class(vcolin)
  vcol <- .preProcCat(vcolin,levRestriction)
  smFactor <- max(smFactor,1.0e-3)
  # T/F is true false of the quantity to be predicted
  # C is the feature we are looking at
  nT <- sum(as.numeric(rescol==resTarget)*weights)  # weighted sum of true examples
  nF <- sum(as.numeric(rescol!=resTarget)*weights)  # weighted sum of false examples
  probT <- nT/(nT+nF)   # unconditional probabilty target is true
  nCandT <- tapply(as.numeric(rescol==resTarget)*weights,vcol,sum)  # weighted sum of true examples for a given C (vector)
  nCandF <- tapply(as.numeric(rescol!=resTarget)*weights,vcol,sum)  # weighted sum of false examples for a give C (vector)
  pCgivenT <- (nCandT+probT*smFactor)/(nT+probT*smFactor)   # probability of a given evidence C, condition on outcome=T
  pCgivenF <- (nCandF+(1.0-probT)*smFactor)/(nF+(1.0-probT)*smFactor)  # probability of a given evidence C, condition on outcome=F
  pTgivenCunnorm <- pCgivenT*probT      # Bayes law, corret missing a /pC term (which we will normalize out)
  pFgivenCunnorm <- pCgivenF*(1-probT)  # Bayes law, corret missing a /pC term (which we will normalize out)
  pTgivenC <- pTgivenCunnorm/(pTgivenCunnorm+pFgivenCunnorm)
  logLift <- log(pTgivenC/probT)  # log probability ratio (so no effect is coded as zero)
  logLift <- as.list(logLift)
  logLift <- logLift[names(logLift)!='zap']  # don't let zap group code
  # fall back for novel levels, use zero impact
  treatment <- list(origvar=origVarName,origColClass=origColClass,
                    newvars=make.names(paste(origVarName,'catB',sep='_')),
                    f=.catBayes,
                    args=list(logLift=logLift,
                              levRestriction=levRestriction),
                    treatmentName='Bayesian Impact Code')
  pred <- treatment$f(vcolin,treatment$args)
  class(treatment) <- 'vtreatment'
  treatment$scales <- .getScales(pred,as.numeric(rescol==resTarget),weights)
  treatment
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
    return(1.0)
  }
  if(!.has.range.cn(x)) {
    return(0.0)
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
  tf <- data.frame(x=x,y=(yC==yTarget))
  n <- nrow(tf)
  if(is.null(weights)) {
    weights <- 1.0+numeric(n)
  }
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








# TODO: pivot warnings/print out of here
# design a treatment for a single variables
# bind a bunch of variables, so we pass exactly what we need to sub-processes
.varDesigner <- function(zoY,
                         zC,zTarget,
                         weights,
                         minFraction,smFactor,rareCount,rareSig,maxMissing,
                         collarProb,
                         trainRows,origRowCount,
                         verbose) {
  force(zoY)
  force(zC)
  force(zTarget)
  force(weights)
  force(minFraction)
  force(smFactor)
  force(rareCount)
  force(rareSig)
  force(maxMissing)
  force(collarProb)
  force(trainRows)
  force(origRowCount)
  force(verbose)
  nRows = length(zoY)
  function(argpair) {
    v <- argpair$v
    vcolOrig <- argpair$vcolOrig
    if(verbose) {
      print(paste('design var',v,date()))
    }
    treatments <- list()
    vcol <- .cleanColumn(vcolOrig,origRowCount)[trainRows]
    if(length(vcol)!=nRows) {
      warning("wrong column length")
      vcol <- NULL
    }
    acceptTreatment <- function(ti) {
      if(!is.null(ti)) {
        treatments[[length(treatments)+1]] <<- ti # Deliberate side-effect
      }
    }
    if(is.null(vcol)) {
      warning(paste('column',v,'is not a type/class/value vtreat can work with (',class(vcolOrig),')'))
    } else {
      colclass <- class(vcol)
      if(.has.range(vcol)) {
        if((colclass=='numeric') || (colclass=='integer')) {
          ti <- .mkPassThrough(v,vcol,zoY,weights,collarProb)
          acceptTreatment(ti)
          ti <- .mkIsBAD(v,vcol,zoY,weights)
          acceptTreatment(ti)
        } else if((colclass=='character') || (colclass=='factor')) {
          # expect character or factor here
          levRestriction <- .safeLevels(vcol,zoY,weights,rareCount,rareSig)
          if(length(levRestriction$safeLevs)>0) {
            ti <- .mkCatInd(v,vcol,zoY,minFraction,maxMissing,levRestriction,weights)
            acceptTreatment(ti)
            if(is.null(ti)||(length(unique(vcol))>2)) {  # make an impactmodel if catInd construction failed or there are more than 2 levels
              if(!is.null(zC)) {  # in categorical mode
                ti <- .mkCatBayes(v,vcol,zC,zTarget,smFactor,levRestriction,weights)
                acceptTreatment(ti)      
              }
              if(is.null(zC)) { # is numeric mode
                ti <- .mkCatNum(v,vcol,zoY,smFactor,levRestriction,weights)
                acceptTreatment(ti)
              }
            }
          }
        } else {
          warning(paste('variable',v,'has unexpected class:',colclass,
                        ', skipping, (want one of numeric,integer,character,factor)'))
        }  
      }
    }
    treatments
  }
}

# find the fairest cutpoint number sequence (similar count on each side)
.cutPoint <- function(y) {
  d <- data.frame(y=y,count=1,
                  stringsAsFactors=FALSE)
  tab <- aggregate(count~y,data=d,FUN=sum)
  if(nrow(tab)<=1) {
    return(tab$y[[1]])
  }
  if(nrow(tab)<=2) {
    return((tab$y[[1]]+tab$y[[2]])/2)
  }
  tab <- tab[order(tab$y),]
  tab$sum <- cumsum(tab$count)
  idx <- findInterval(length(y)/2,tab$sum)
  if(idx>=(length(y)-1)) {
    return((tab$y[[nrow(tab)-1]]+tab$y[[nrow(tab)]])/2)
  }
  return((tab$y[[idx+1]]+tab$y[[idx+2]])/2)
}


# build sets for out of sample evaluatin, train on complement of eval
.buildEvalSets <- function(zoY,fullCross) {
  nRows = length(zoY)
  # build a partition plan
  evalSets <- list()
  if((nRows<=1) || (min(zoY)>=max(zoY))) {
    return(evalSets) # no plan possible
  }
  if(nRows<=100) {
    # small case, 1-holdout Jackknife style
    for(i in seq_len(nRows)) {
      evalG <- i
      trainG <- setdiff(seq_len(nRows),evalG)
      if(min(zoY[trainG])<max(zoY[trainG])) {
        evalSets[[1+length(evalSets)]] <- evalG
      }
    }
    return(evalSets)
  }
  if(fullCross || (nRows<=1000)) {
    #  Try for full k-way cross val
    ncross <- 2
    # stratify the cut accross "large/small" zoY
    yCut <- .cutPoint(zoY)
    ySmall <- which(zoY<yCut)
    yLarge <- setdiff(seq_len(length(zoY)),ySmall)
    rareCount <- min(length(ySmall),length(yLarge))
    if(rareCount>=2) {
      # permute 
      ySmall <- sample(ySmall,length(ySmall),replace=FALSE)
      yLarge <- sample(yLarge,length(yLarge),replace=FALSE)
      ncross <- floor(min(ncross,rareCount/2))
      smallChunks <- split(ySmall, ceiling(seq_along(ySmall)/(length(ySmall)/ncross)))
      largeChunks <- split(yLarge, ceiling(seq_along(yLarge)/(length(yLarge)/ncross)))
      evalSets <- lapply(seq_len(ncross),
                         function(i) { c(smallChunks[[i]],largeChunks[[i]]) })
    }
  }
  if(length(evalSets)<1) {
    # fall back to test/train split
    repeat {
      evalTrainRows <- sort(sample.int(nRows,size=floor(0.75*nRows)))
      # technically need to check that y is varying to gaurantee we have good sample
      # with high probability we get on of each, so shouldn't repeat often (if at all)
      if(min(zoY[evalTrainRows])<max(zoY[evalTrainRows])) {
        break
      }
    }
    evalSets <- list(setdiff(seq_len(nRows),evalTrainRows))
  }
  evalSets
}


# build out-of-sample treated columns, can have any number of rows
.outSampleXform <- function(outcomename,zoY,
                            zC,zTarget,
                            weights,
                            minFraction,smFactor,rareCount,rareSig,maxMissing,
                            collarProb,
                            scale,doCollar,fullCross) {
  force(outcomename)
  force(zoY)
  force(zC)
  force(zTarget)
  force(weights)
  force(minFraction)
  force(smFactor)
  force(rareCount)
  force(rareSig)
  force(maxMissing)
  force(collarProb)
  force(scale)
  force(doCollar)
  force(fullCross)
  nRows = length(zoY)
  # build a partition plan
  evalSets <- .buildEvalSets(zoY,fullCross)
  function(argpair) {
    v <- argpair$v
    vcolOrig <- argpair$vcolOrig
    evalGroups <- list()
    for(evalSet in evalSets) {
      trainSet <- setdiff(seq_len(nRows),evalSet)
      vDesigner <- .varDesigner(zoY[trainSet],
                                zC[trainSet],zTarget,
                                weights[trainSet],
                                minFraction,smFactor,rareCount,rareSig,maxMissing,
                                collarProb,
                                trainSet,nRows,
                                FALSE)
      xcolClean <- .cleanColumn(vcolOrig,nRows)[evalSet]
      ti <- vDesigner(argpair)
      if((length(xcolClean)>0)&&(length(ti)>0)) {
        subF <- c()
        for(tij in ti) {
          subFj <- .vtreatA(tij,xcolClean,scale,doCollar)
          if(is.null(subF)) {
            subF <- subFj
          } else {
            subF <- cbind(subF,subFj)
          }
        }
        rownames(subF) <- evalSet
        subF[[outcomename]] <- zoY[evalSet]
        evalGroups[[1+length(evalGroups)]] <- subF
      }
    }
    df <- c()
    if(length(evalGroups)>0) {
      colset <- c(setdiff(unique(unlist(lapply(evalGroups,function(d) colnames(d)))),
                        outcomename),outcomename)
      df <- do.call(rbind,lapply(evalGroups,function(d) { 
        d[,setdiff(colset,colnames(d))] <- NA
        d[,colset,drop=FALSE]
        }))
      for(v in colnames(df)) {
        naPosn <- is.na(df[[v]])
        if(sum(naPosn)>0) {
          rv <- mean(df[[v]],na.rm=TRUE)
          if(is.na(rv)) {
            rv <- 0.0
          }
          df[[v]][naPosn] <- rv
        }
      }
      if(nrow(df)==nRows) {
        perm <- as.integer(rownames(df))
        invperm <- seq_len(length(perm))
        invperm[perm] <- seq_len(length(perm))
        df <- df[invperm,,drop=FALSE]
      }
    }
    df
  }
}


.buildScores <- function(xFrame,vset,outcomename,catTarget) {
  force(xFrame)
  force(vset)
  force(outcomename)
  force(catTarget)
  function(ti) {
    scoreFrame <- c()
    origName <- vorig(ti)
    for(nv in vnames(ti)) {
      if(nv %in% vset) {
        varMoves <- .has.range.cn(xFrame[[nv]])
        PRESSRsquared=0.0
        psig=1.0
        sig=1.0
        catPRSquared=0.0
        csig=1.0
        if(varMoves) {
          pstat <- pressStatOfBestLinearFit(xFrame[[nv]],
                                            xFrame[[outcomename]],
                                            c(),  # TODO: get weights to here
                                            'total')
          PRESSRsquared <- pstat$rsq
          psig <- pstat$sig
          sig <- pstat$sig
          if(catTarget) {
            cstat <- catScore(xFrame[[nv]],
                              xFrame[[outcomename]],1,
                              c())  # TODO: get weights here
            catPRSquared <- cstat$pRsq
            csig <- cstat$sig
            sig <- cstat$sig
          }
        }
        scoreFrameij <- data.frame(varName=nv,
                                   origName=origName,
                                   varMoves=varMoves,
                                   PRESSRsquared=PRESSRsquared,
                                   psig=psig,
                                   sig=sig,
                                   stringsAsFactors = FALSE)
        if(catTarget) {
          scoreFrameij$catPRSquared <- catPRSquared
          scoreFrameij$csig <- csig
          scoreFrameij$sig <- csig
        }
        scoreFrame <- rbind(scoreFrame,scoreFrameij)
      }
    }
    scoreFrame
  }
}



# build all treatments for a data frame to predict a given outcome
.designTreatmentsX <- function(dframe,varlist,outcomename,zoY,
                               zC,zTarget,
                               weights,
                               minFraction,smFactor,
                               rareCount,rareSig,maxMissing,
                               collarProb,
                               returnXFrame,scale,doCollar,
                               verbose,
                               parallelCluster) {
  if(!requireNamespace("parallel",quietly=TRUE)) {
    parallelCluster <- NULL
  }
  if(!is.data.frame(dframe)) {
    stop("dframe must be a data frame")
  }
  if(nrow(dframe)<=0) {
    stop("not enough rows in data frame")
  }
  if(collarProb>=0.5) {
    stop("collarProb must be < 0.5")
  }
  if(verbose) {
    print(paste("desigining treatments",date()))
  }
  varlist <- setdiff(varlist,outcomename)
  varlist <- intersect(varlist,colnames(dframe))
  varlist <- as.character(varlist)
  if(is.null(weights)) {
    weights <- rep(1.0,nrow(dframe))
  } else {
    if(!is.numeric(weights)) {
      stop("weights need to be numeric")
    }
    if(length(weights)!=nrow(dframe)) {
      stop("must have same number of weights as data frame rows")
    }
    goodPosns <- ifelse(.is.bad(weights),FALSE,weights>0.0)
    dframe <- dframe[goodPosns,,drop=FALSE]
    zoY <- zoY[goodPosns]
    weights <- weights[goodPosns]
    if(!is.null(zC)) {
      zC <- zC[goodPosns]
    }
    # the select goodPosns is duplicating the data frame, so it does cost
    # memory
  }
  if(nrow(dframe)<=0) {
    stop("no rows")
  }
  if(min(weights)<0) {
    stop("negative weights")
  }
  if(sum(weights)<=0) {
    stop("no non-zero weighted rows")
  }
  if(sum(.is.bad(zoY))>0) {
    stop("outcome variable had NAs")
  }
  if(min(zoY)>=max(zoY)) {
    stop("outcome variable doesn't vary")
  }
  # In building the workList don't transform any variables (such as making
  # row selections), only select columns out of frame.  This prevents
  # data growth prior to doing the work.
  workList <- lapply(varlist,function(v) {list(v=v,vcolOrig=dframe[[v]])})
  # build the treatments we will return to the user
  worker <- .varDesigner( zoY,
                          zC,zTarget,
                          weights,
                          minFraction,smFactor,rareCount,rareSig,maxMissing,
                          collarProb,
                          seq_len(nrow(dframe)),nrow(dframe),
                          verbose)
  if(is.null(parallelCluster)) {
    # print("design serial")
    treatments <- lapply(workList,worker)
  } else {
    # print("design parallel")
    treatments <- parallel::parLapply(parallelCluster,workList,worker)
  }
  treatments <- unlist(treatments,recursive=FALSE)
  if(length(treatments)<=0) {
    stop('no usable vars')
  }
  # score variables
  if(verbose) {
    print(paste("scoring treatments",date()))
  }
  sw <- .outSampleXform(outcomename,zoY,
                        zC,zTarget,
                        weights,
                        minFraction,smFactor,
                        rareCount,rareSig,maxMissing,
                        collarProb,
                        scale,doCollar,returnXFrame)
  if(is.null(parallelCluster)) {
    # print("design serial")
    xFrames <- lapply(workList,sw)
  } else {
    # print("design parallel")
    xFrames <- parallel::parLapply(parallelCluster,workList,sw)
  }
  xFrames <- Filter(Negate(is.null),xFrames)
  prepedRows <- vapply(xFrames,function(f) nrow(f),numeric(1))
  xFrames <- xFrames[prepedRows>=max(prepedRows)]
  xFrame <- do.call(cbind,lapply(xFrames,function(d) {
    d[,setdiff(colnames(d),outcomename),drop=FALSE]
    }))
  xFrame[[outcomename]] <- xFrames[[1]][[outcomename]]
  xFrames <- c()
  nmMap <- getNewVarNames(treatments)
  treatedVarNames <- vapply(nmMap,function(p) {p$new},character(1))
  origVarNames <- vapply(nmMap,function(p) {p$orig},character(1))
  # now score variables
  vset <- intersect(treatedVarNames,setdiff(colnames(xFrame),outcomename))
  scrW <- .buildScores(xFrame,vset,outcomename,!is.null(zC))
  if(is.null(parallelCluster)) {
     sFrames <- lapply(treatments,scrW)
  } else {
     sFrames <- parallel::parLapply(parallelCluster,treatments,scrW)
  }
  sFrames <- Filter(Negate(is.null),sFrames)
  sFrame <- do.call(rbind,sFrames)
  varMoves <- sFrame$varMoves
  names(varMoves) <- sFrame$varName
  sig <- sFrame$sig
  names(sig) <- sFrame$varName
  plan <- list(treatments=treatments,
               vars=vset,
               varMoves=varMoves,
               sig=sig,
               scoreFrame=sFrame,
               nmMap=nmMap,
               outcomename=outcomename,
               meanY=.wmean(zoY,weights),
               ndat=length(zoY))
  class(plan) <- 'treatmentplan'
  if(returnXFrame) {
    plan[['xframe']] <- xFrame
  }
  skippedVars <- setdiff(varlist,
                         c(outcomename,unique(plan$scoreFrame$origName)))
  plan$skippedVars <- skippedVars
  if(length(skippedVars)>0) {
    print(paste("WARNING skipped vars:",paste(skippedVars,collapse=", ")))
  }
  if(verbose) {
    print(paste("have treatment plan",date()))
  }
  plan
}


.checkArgs <- function(dframe,varlist,outcomename,...) {
  args <- list(...)
  if(missing(dframe)||(!is.data.frame(dframe))||(nrow(dframe)<0)||(ncol(dframe)<=0)) {
    stop("dframe must be a non-empty data frame")
  }
  if(missing(varlist)||(!is.character(varlist))||(length(varlist)<1)) {
    stop("varlist must be a non-empty character vector")
  }
  if(missing(outcomename)||(!is.character(outcomename))||(length(outcomename)!=1)) {
    stop("outcomename must be a length 1 character vector")
  }
  if(length(args)!=0) {
    nm <- setdiff(paste(names(args),collapse=", "),'')
    nv <- length(args)-length(nm)
    stop(paste("unexpected arguments",nm,"(and",nv,"unexpected values)"))
  }
}




# build all treatments for a data frame to predict a categorical outcome


#' designTreatmentsC 
#' 
#' Function to design variable treatments for binary prediction of a
#' categorical outcome.  Data frame is assumed to have only atomic columns
#' except for dates (which are converted to numeric).
#' 
#' The main fields are mostly vectors with names (all with the same names in the same order):
#' 
#' - vars : (character array without names) names of variables (in same order as names on the other diagnostic vectors)
#' - varMoves : logical TRUE if the variable varied during hold out scoring, only variables that move will be in the treated frame
#' - #' - sig : an estimate signficance of effect
#'
#' See the vtreat vignette for a bit more detail and a worked example.
#'
#' @param dframe Data frame to learn treatments from (training data).
#' @param varlist Names of columns to treat (effective variables).
#' @param outcomename Name of column holding outcome variable.
#' @param outcometarget Value/level of outcome to be considered "success"
#' @param ... no additional arguments, declared to forced named binding of later arguments
#' @param weights optional training weights for each row
#' @param minFraction optional minimum frequency a categorical level must have to be converted to an indicator column.
#' @param smFactor optional smoothing factor for impact coding models.
#' @param rareCount optional integer, supress direct effects of level of this count or less.
#' @param rareSig optional integer, supress direct effects of level of this signficance or less.
#' @param maxMissing optional maximum fraction (by data weight) of a categorical variable that are allowed before switching from indicators to impact coding.
#' @param collarProb what fraction of the data (pseudo-probability) to collar data at (<0.5).
#' @param returnXFrame optional if TRUE return out of sample transformed frame.
#' @param scale logical optional controls scaling for scoring and returnXFrame 
#' @param doCollar logical optional controls collaring for scoring and returnXFrame
#' @param verbose if TRUE print progress.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow
#' @return treatment plan (for use with prepare)
#' @seealso \code{\link{prepare}} \code{\link{designTreatmentsN}} 
#' @examples
#' 
#' dTrainC <- data.frame(x=c('a','a','a','b','b','b'),
#'    z=c(1,2,3,4,5,6),
#'    y=c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))
#' dTestC <- data.frame(x=c('a','b','c',NA),
#'    z=c(10,20,30,NA))
#' treatmentsC <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE)
#' dTrainCTreated <- prepare(treatmentsC,dTrainC,pruneSig=0.99)
#' dTestCTreated <- prepare(treatmentsC,dTestC,pruneSig=0.99)
#' 
#' @export
designTreatmentsC <- function(dframe,varlist,outcomename,outcometarget,
                              ...,
                              weights=c(),
                              minFraction=0.02,smFactor=0.0,
                              rareCount=2,rareSig=0.3,maxMissing=0.04,
                              collarProb=0.00,
                              returnXFrame=FALSE,scale=FALSE,doCollar=TRUE,
                              verbose=TRUE,
                              parallelCluster=NULL) {
  .checkArgs(dframe=dframe,varlist=varlist,outcomename=outcomename,...)
   zoY <- ifelse(dframe[[outcomename]]==outcometarget,1.0,0.0)
  .designTreatmentsX(dframe,varlist,outcomename,zoY,
                     dframe[[outcomename]],outcometarget,
                     weights,
                     minFraction,smFactor,
                     rareCount,rareSig,maxMissing,
                     collarProb,
                     returnXFrame,scale,doCollar,
                     verbose,
                     parallelCluster)
}

# build all treatments for a data frame to predict a numeric outcome


#' designTreatmentsN 
#' 
#' Function to design variable treatments for binary prediction of a
#' numeric outcome.  Data frame is assumed to have only atomic columns
#' except for dates (which are converted to numeric).
#' Note: each column is processed independently of all others.
#' 
#' The main fields are mostly vectors with names (all with the same names in the same order):
#' 
#' - vars : (character array without names) names of variables (in same order as names on the other diagnostic vectors)
#' - varMoves : logical TRUE if the variable varied during hold out scoring, only variables that move will be in the treated frame
#' - sig : an estimate signficance of effect
#'
#' See the vtreat vignette for a bit more detail and a worked example.
#' 
#' @param dframe Data frame to learn treatments from (training data).
#' @param varlist Names of columns to treat (effective variables).
#' @param outcomename Name of column holding outcome variable.
#' @param ... no additional arguments, declared to forced named binding of later arguments
#' @param weights optional training weights for each row
#' @param minFraction optional minimum frequency a categorical level must have to be converted to an indicator column.
#' @param smFactor optional smoothing factor for impact coding models.
#' @param rareCount optional integer, supress direct effects of level of this count or less.
#' @param rareSig optional integer, supress direct effects of level of this signficance or less.
#' @param maxMissing optional maximum fraction (by data weight) of a categorical variable that are allowed before switching from indicators to impact coding.
#' @param collarProb what fraction of the data (pseudo-probability) to collar data at (<0.5).
#' @param returnXFrame optional if TRUE return out of sample transformed frame.
#' @param scale logical optional controls scaling for scoring and returnXFrame 
#' @param doCollar logical optional controls collaring for scoring and returnXFrame
#' @param verbose if TRUE print progress.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow
#' @return treatment plan (for use with prepare)
#' @seealso \code{\link{prepare}} \code{\link{designTreatmentsC}} 
#' @examples
#' 
#' dTrainN <- data.frame(x=c('a','a','a','a','b','b','b'),
#'     z=c(1,2,3,4,5,6,7),y=c(0,0,0,1,0,1,1))
#' dTestN <- data.frame(x=c('a','b','c',NA),
#'     z=c(10,20,30,NA))
#' treatmentsN = designTreatmentsN(dTrainN,colnames(dTrainN),'y')
#' dTrainNTreated <- prepare(treatmentsN,dTrainN,pruneSig=0.99)
#' dTestNTreated <- prepare(treatmentsN,dTestN,pruneSig=0.99)
#' 
#' @export
designTreatmentsN <- function(dframe,varlist,outcomename,
                              ...,
                              weights=c(),
                              minFraction=0.02,smFactor=0.0,
                              rareCount=2,rareSig=0.3,maxMissing=0.04,
                              collarProb=0.00,
                              returnXFrame=FALSE,scale=FALSE,doCollar=TRUE,
                              verbose=TRUE,
                              parallelCluster=NULL) {
  .checkArgs(dframe=dframe,varlist=varlist,outcomename=outcomename,...)
  ycol <- dframe[[outcomename]]
  .designTreatmentsX(dframe,varlist,outcomename,ycol,
                     c(),c(),
                     weights,
                     minFraction,smFactor,
                     rareCount,rareSig,maxMissing,
                     collarProb,
                     returnXFrame,scale,doCollar,
                     verbose,
                     parallelCluster)
}



# apply treatments and restrict to useful variables
# copies over y if present


#' prepare 
#' 
#' Use a treatment plan to prepare a data frame for analysis.  The
#' resulting frame will have new effective variables that are numeric
#' and free of NaN/NA.  If the outcome column is present it will be copied over.
#' The intent is that these frames are compatible with more machine learning
#' techniques, and avoid a lot of corner cases (NA,NaN, novel levels, too many levels).
#' Note: each column is processed independently of all others.
#' 
#' @param treatmentplan Plan built by designTreantmentsC() or designTreatmentsN()
#' @param dframe Data frame to be treated
#' @param pruneSig suppress variables with significance above this level
#' @param ... no additional arguments, declared to forced named binding of later arguments
#' @param scale optional if TRUE replace numeric variables with regression ("move to outcome-scale").
#' @param doCollar optional if TRUE collar numeric variables by cutting off after a tail-probability specified by collarProb during treatment design.
#' @param varRestriction optional list of treated variable names to restrict to
#' @return treated data frame (all columns numeric, without NA,NaN)
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow
#' @seealso \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}}
#' @examples
#' 
#' dTrainN <- data.frame(x=c('a','a','a','a','b','b','b'),
#'     z=c(1,2,3,4,5,6,7),y=c(0,0,0,1,0,1,1))
#' dTestN <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))
#' treatmentsN = designTreatmentsN(dTrainN,colnames(dTrainN),'y')
#' dTrainNTreated <- prepare(treatmentsN,dTrainN,1.0)
#' dTestNTreated <- prepare(treatmentsN,dTestN,1.0)
#' 
#' dTrainC <- data.frame(x=c('a','a','a','b','b','b'),
#'     z=c(1,2,3,4,5,6),y=c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))
#' dTestC <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))
#' treatmentsC <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE)
#' dTrainCTreated <- prepare(treatmentsC,dTrainC,1.0)
#' dTestCTreated <- prepare(treatmentsC,dTestC,1.0)
#' 
#' 
#' @export
prepare <- function(treatmentplan,dframe,pruneSig,
                    ...,
                    scale=FALSE,doCollar=TRUE,
                    varRestriction=c(),
                    parallelCluster=NULL) {
  .checkArgs(dframe=dframe,varlist=c('x'),outcomename=c('x'),...)
  if(!requireNamespace("parallel",quietly=TRUE)) {
    parallelCluster <- NULL
  }
  if(class(treatmentplan)!='treatmentplan') {
    stop("treatmentplan must be of class treatmentplan")
  }
  if(!is.data.frame(dframe)) {
    stop("dframe must be a data frame")
  }
  if(nrow(dframe)<=0) {
    stop("no rows")
  }
  usableVars <- treatmentplan$vars
  if(!is.null(treatmentplan$varMoves)) {
    usableVars <- intersect(usableVars,names(treatmentplan$varMoves)[treatmentplan$varMoves])
  }
  if((!is.null(treatmentplan$sig)) &&(!is.null(pruneSig))) {
    canUse <- treatmentplan$sig<=pruneSig
    usableVars <- intersect(usableVars,names(treatmentplan$sig)[canUse])
  }
  if(!is.null(varRestriction)) {
     usableVars <- intersect(usableVars,varRestriction)
  }
  if(length(usableVars)<=0) {
    stop('no usable vars')
  }
  treated <- .vtreatList(treatmentplan$treatments,dframe,usableVars,scale,doCollar,
                         parallelCluster)
  # copy outcome over if it is present
  if(treatmentplan$outcomename %in% colnames(dframe)) {
    treated[[treatmentplan$outcomename]] <- dframe[[treatmentplan$outcomename]]
  }
  treated
}

