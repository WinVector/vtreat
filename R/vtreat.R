# variable treatments type def: list { origvar, newvars, f(col,args), args, treatmentName, scales } can share orig var



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
#' print(as.character(getNewVarNames(treatmentsN$treatments)))
#' print(as.character(getNewVarNames(treatmentsN$treatments,c('x'))))
#' 
#' @export
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
          names[[j]] <- ni
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
  # TODO: use parralelCluster
  gs <- lapply(toProcess,procWorker)
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
#' @seealso \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}} \code{\link{getNewVarNames}}
#' @export
#' 
vorig <- function(x) { x$origvar }


#'
#' New treated variable names from a treatmentplan$treatment item.
#' @param x vtreatment item
#' @seealso \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}} \code{\link{getNewVarNames}}
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
  model <- lm.wfit(lmatx,lmaty,weights)
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
     cuts <- as.numeric(quantile(xcol[!napositions],probs=c(collarProb,1-collarProb)))
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


# return categorical indicators
.catInd <- function(col,args,doCollar) {
  origna <- is.na(col)
  col <- paste('x',as.character(col))
  col[origna] <- 'NA'
  nres <- length(args$tracked)
  vals <- vector('list',nres)
  sum <- rep(0,length(col))
  for(j in 1:nres) {
    vi <- ifelse(col==args$tracked[j],1.0,0.0) 
    vals[[j]] <- vi
    sum <- sum + vi
  }
  if(nres>1) {
     for(ri in which(sum==0)) { 
        # For novel levels put fraction of time each level was on in original data
        for(j in 1:nres) {
           vals[[j]][[ri]] <- args$dist[[j]]
        }
     }
  }
  vals
}

# build categorical indicators
.mkCatInd <- function(origVarName,vcolin,ynumeric,minFraction,maxMissing,weights) {
  origColClass <- class(vcolin)
  origna <- is.na(vcolin)
  vcol <- paste('x',as.character(vcolin))
  vcol[origna] <- 'NA'
  counts <- tapply(weights,vcol,sum)
  totMass <- sum(counts)
  tracked <- names(counts)[counts/totMass>=minFraction]
  counts <- counts[tracked]
  missingMass <- 1 - sum(counts)/totMass
  if(missingMass>maxMissing) {
    return(c())
  }
  dist <- as.numeric(counts/sum(counts))
  treatment <- list(origvar=origVarName,origColClass=origColClass,
                    newvars=make.names(paste(origVarName,'lev',tracked,sep="_"),unique=TRUE),
                    f=.catInd,
                    args=list(tracked=tracked,dist=dist),
                    treatmentName='Categoric Indicators')
  class(treatment) <- 'vtreatment'
  pred <- treatment$f(vcolin,treatment$args)
  nvar <- length(pred)
  treatment$scales <- list('a'=rep(1.0,nvar),'b'=rep(0.0,nvar))  
  for(j in 1:nvar) {
    scales <- .getScales(pred[[j]],ynumeric,weights)
    treatment$scales$a[j] <- scales$a
    treatment$scales$b[j] <- scales$b
  }
  treatment
}


# apply a numeric impact model
# replace level with .wmean(x|category) - .wmean(x)
.catNum <- function(col,args,doCollar) {
  origna <- is.na(col)
  col <- paste('x',as.character(col))  # R can't use empty string as a key
  col[origna] <- 'NA' 
  novel <- !(col %in% names(args$scores))
  keys <- col
  keys[novel] <- names(args$scores)[[1]]   # just to prevent bad lookups
  pred <- as.numeric(args$scores[keys]) 
  # mean delta impact averaged over all possibilities, should be zero in scaled mode, mean dist in unscaled
  pred[novel] <- args$novelvalue  
  pred
}

# build a numeric impact model
# see: http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/
.mkCatNum <- function(origVarName,vcolin,rescol,smFactor,weights) {
  origColClass <- class(vcolin)
  origna <- is.na(vcolin)
  vcol <- paste('x',as.character(vcolin))   # R can't use empty string as a key
  vcol[origna] <- 'NA'
  baseMean <- .wmean(rescol,weights)
  num <- tapply(rescol*weights,vcol,sum)
  den <- tapply(weights,vcol,sum)
  scores <- (num+smFactor*baseMean)/(den+smFactor)-baseMean
  novelvalue <- sum(scores*den)/sum(den)
  scores <- as.list(scores)
  treatment <- list(origvar=origVarName,origColClass=origColClass,
                    newvars=make.names(paste(origVarName,'catN',sep='_')),
                    f=.catNum,
                    args=list(scores=scores,novelvalue=novelvalue),
                    treatmentName='Scalable Impact Code')
  pred <- treatment$f(vcolin,treatment$args)
  class(treatment) <- 'vtreatment'
  treatment$scales <- .getScales(pred,rescol,weights)
  treatment
}



# apply a classification impact model
# replace level with log(.wmean(x|category)/.wmean(x))
.catBayes <- function(col,args,doCollar) {
  origna <- is.na(col)
  col <- paste('x',as.character(col))   # R can't use empty string as a key
  col[origna] <- 'NA' 
  novel <- !(col %in% names(args$logLift))
  keys <- col
  keys[novel] <- names(args$logLift)[[1]]  # just to prevent bad lookups
  pred <- as.numeric(args$logLift[keys]) 
  pred[novel] <- args$novelvalue  # mean delta impact averaged over all possibilities, should be zero in scaled mode, mean dist in unscaled
  pred
}

# build a classification impact model
# see: http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/
.mkCatBayes <- function(origVarName,vcolin,rescol,resTarget,smFactor,weights) {
  origColClass <- class(vcolin)
  origna <- is.na(vcolin)
  vcol <- paste('x',as.character(vcolin))  # R can't use empty string as a key
  vcol[origna] <- 'NA'
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
  # fall back for novel levels, use average response of model during training
  den <- tapply(weights,vcol,sum)
  novelvalue <- sum(logLift*den)/sum(den)
  logLift <- as.list(logLift)
  treatment <- list(origvar=origVarName,origColClass=origColClass,
                    newvars=make.names(paste(origVarName,'catB',sep='_')),
                    f=.catBayes,
                    args=list(logLift=logLift,novelvalue=novelvalue),
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
      holdout = { meanH <- hold1OutMeans(y,weights); sum(weights*(y-meanH)^2) },
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
  for(i in 1:n) {
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
  for(i in 1:n) {
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
     sig <- pf(Fstat,1,n-2,lower.tail=F)
  }
  list(rsq=pRsq,sig=sig)
}




# y = TRUE/FALSE
# py = probilities
.deviance <- function(y,py) {
  -2.0*(sum(log(py[y]))+sum(log(1-py[!y])))
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
    model <- glm(as.formula('y~x'),
                 data=tf,
                 family=binomial(link='logit'),
                 weights=weights)
    if(model$converged) {
      df.null <- n
      df.model <- n-2
      deldf <- df.null - df.model
      null.dev <- .deviance(tf$y,numeric(n)+mean(tf$y))
      py <- predict(model,type='response',
                    newdata=tf)
      resid.dev <- .deviance(tf$y,py)
      pseudoR2 <- 1.0 - resid.dev/null.dev
      sig <- 1.0
      if(resid.dev<null.dev) {
         delDev <- null.dev - resid.dev
         sig <- pchisq(delDev,deldf,lower.tail=F)
      }
      return(list(pRsq=pseudoR2,sig=sig))
    }
  },
  error=function(e){})
  return(list(pRsq=0.0,sig=1.0))
}





# TODO: pivot warnings/print out of here
.varScorer <- function(treatedZoY,treatedZC,zTarget,
                       treatedWeights,scoreRows,verbose) {
  force(treatedZoY)
  force(treatedZC)
  force(zTarget)
  force(treatedWeights)
  force(scoreRows)
  force(verbose)
  nScoreRows <- length(treatedZoY)
  function(spair) {
    ti <- spair$ti
    xcolOrig <- spair$xcolOrig
    xcolClean <- .cleanColumn(xcolOrig[scoreRows],nScoreRows)
    if(is.null(xcolClean)) {
      stop(paste('column',ti$origvar,'is not a type/class vtreat can work with (',class(xcolOrig),')'))
    }
    if(verbose) {
      print(paste("score variable(s)",ti$newvars,"(derived from",ti$origvar,")",date()))
    }
    subF <- .vtreatA(ti,xcolClean,TRUE,TRUE)
    varMoves <- list()
    for(nv in colnames(subF)) {
      varMoves[[nv]] <- .has.range.cn(subF[[nv]])
    }
    nms <- names(varMoves)[as.logical(varMoves)]
    subScores <- c()
    catScores <- c()
    pRsq <- c()
    pSig <- c()
    cRsq <- c()
    cSig <- c()
    if(length(nms>0)) {
      subScores <- lapply(nms,
                          function(c) pressStatOfBestLinearFit(subF[[c]],
                                                               treatedZoY,
                                                               treatedWeights,
                                                               'total'))
      names(subScores) <- nms
      pRsq <- vapply(subScores,function(z) z$rsq,numeric(1))
      pSig <- vapply(subScores,function(z) z$sig,numeric(1))
      cRsq <- c()
      cSig <- c()
      if(!is.null(treatedZC)) {
        catScores <- lapply(nms,
                            function(c) catScore(subF[[c]],
                                                  treatedZC,zTarget,
                                                  treatedWeights))
        names(catScores) <- nms
        cRsq <- vapply(catScores,function(z) z$pRsq,numeric(1))
        cSig <- vapply(catScores,function(z) z$sig,numeric(1))
      }
    }
    list(ti=ti,varMoves=varMoves,
         pRsq=pRsq,
         pSig=pSig,
         cRsq=cRsq,
         cSig=cSig)
  }
}



# TODO: pivot warnings/print out of here
# design a treatment for a single variables
# bind a bunch of variables, so we pass exactly what we need to sub-processes
.varDesigner <- function(zoY,
                         zC,zTarget,
                         weights,
                         minFraction,smFactor,maxMissing,
                         collarProb,
                         trainRows,origRowCount,
                         verbose) {
  force(zoY)
  force(zC)
  force(zTarget)
  force(weights)
  force(minFraction)
  force(smFactor)
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
          ti <- .mkCatInd(v,vcol,zoY,minFraction,maxMissing,weights)
          acceptTreatment(ti)
          if(is.null(ti)||(length(unique(vcol))>2)) {  # make an impactmodel if catInd construction failed or there are more than 2 levels
            if(!is.null(zC)) {  # in categorical mode
              ti <- .mkCatBayes(v,vcol,zC,zTarget,smFactor,weights)
              acceptTreatment(ti)      
            }
            if(is.null(zC)) { # is numeric mode
              ti <- .mkCatNum(v,vcol,zoY,smFactor,weights)
              acceptTreatment(ti)
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




# build all treatments for a data frame to predict a given outcome
.designTreatmentsX <- function(dframe,varlist,outcomename,zoY,
                               zC,zTarget,
                               weights,
                               minFraction,smFactor,maxMissing,
                               collarProb,
                               scoreVars,maxScoreSize,
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
  # first build the treatments we will return to the user
  worker <- .varDesigner( zoY,
                          zC,zTarget,
                          weights,
                          minFraction,smFactor,maxMissing,
                          collarProb,
                          1:nrow(dframe),nrow(dframe),
                          verbose)
  if(is.null(parallelCluster)) {
    # print("design serial")
    treatments <- lapply(workList,worker)
  } else {
    # print("design parallel")
    treatments <- parallel::parLapply(parallelCluster,workList,worker)
  }
  treatments <- unlist(treatments,recursive=FALSE)
  treatedVarNames <- as.character(getNewVarNames(treatments))
  # now (optinally) score variables
  varMoves <- c()
  PRESSRsquared <- c()
  psig <- c()
  catPRSquared <- c()
  csig <- c()
  sig <- c()
  if (scoreVars) {  # see if we can afford to score on disjoint rows
    evalTrainRows <- 1:nrow(dframe)
    scoreRows <- integer(0)
    # Note: we are sampling according to indices (not weights), 
    # so this can have a bit higher variance 
    # than a weight-driven sample.
    if(nrow(dframe)>100) {  # large case, go for disjoint
      repeat {
        evalTrainRows <- sort(sample.int(nrow(dframe),size=floor(0.75*nrow(dframe))))
        # technically need to check that y is varying to gaurantee we have good sample
        # with high probability we get on of each, so shouldn't repeat often (if at all)
        if(min(zoY[evalTrainRows])<max(zoY[evalTrainRows])) {
          break
        }
      }
      scoreRows <- setdiff(1:nrow(dframe),evalTrainRows)
      if(length(scoreRows)>maxScoreSize) {
        scoreRows <- sample(scoreRows,size=maxScoreSize)
      }
    } else {  # small case, allow overlap
      if(nrow(dframe)<=maxScoreSize) {
        scoreRows <- 1:nrow(dframe)
      } else {
        scoreRows <- sort(sample.int(nrow(dframe),size=maxScoreSize))
      }
    }
    if(length(evalTrainRows)==nrow(dframe)) {
      evalTreatments <- treatments
    } else {
      # now build treatments we will use to estimate scores (try to make them disjoint)
      # from test rows if we have enough data
      worker <- .varDesigner( zoY[evalTrainRows],
                              zC[evalTrainRows],zTarget,
                              weights[evalTrainRows],
                              minFraction,smFactor,maxMissing,
                              collarProb,
                              evalTrainRows,nrow(dframe),
                              verbose)
      if(is.null(parallelCluster)) {
        # print("design serial")
        evalTreatments <- lapply(workList,worker)
      } else {
        # print("design parallel")
        evalTreatments <- parallel::parLapply(parallelCluster,workList,worker)
      }
      evalTreatments <- unlist(evalTreatments,recursive=FALSE)
    }
    # get scores for our treatments, using the eval treatments
    # there is a risk of name-mismatch (as each may be a different set)
    # these are going to be rare low-utility variables (due to non-variation
    # or levels not showing up), so give them a useless score.
    varMoves <- logical(length(treatedVarNames))
    names(varMoves) <- treatedVarNames
    PRESSRsquared <- numeric(length(treatedVarNames)) + NA
    names(PRESSRsquared) <- treatedVarNames
    psig <- numeric(length(treatedVarNames)) + NA
    names(psig) <- treatedVarNames
    if(!is.null(zC)) {
      catPRSquared <- numeric(length(treatedVarNames)) + NA
      names(catPRSquared) <- treatedVarNames
      csig <- numeric(length(treatedVarNames)) + NA
      names(csig) <- treatedVarNames
    }
    sig <- numeric(length(treatedVarNames)) + 1.0
    names(sig) <- treatedVarNames
    if(!is.null(zC)) {
      catPRSquared <- numeric(length(treatedVarNames))
      names(catPRSquared) <- treatedVarNames
    }
    if(verbose) {
      print(paste("scoring columns",date()))
    }
    treatedZoY <- zoY[scoreRows]
    treatedZC <- c()
    if(!is.null(zC)) {
      treatedZC <- zC[scoreRows]
    }
    treatedWeights <- weights[scoreRows]
    # In building the workList don't transform any variables (such as making
    # row selections), only select columns out of frame.  This prevents
    # data growth prior to doing the work.
    workList <- list()
    for(ti in evalTreatments) {
      workList[[length(workList)+1]] <- list(ti=ti,
                                             xcolOrig=dframe[[ti$origvar]])
    }
    worker <- .varScorer(treatedZoY,treatedZC,zTarget,
                         treatedWeights,scoreRows,verbose) 
    if(is.null(parallelCluster)) {
      # print("score serial")
      scoreList <- lapply(workList,worker)
    } else {
      # print("score parallel")
      scoreList <- parallel::parLapply(parallelCluster,workList,worker)
    }
    for(wpair in scoreList) {
      subMoves <- wpair$varMoves
      for(nv in names(subMoves)) {
        if(nv %in% treatedVarNames) {
          varMoves[[nv]] <- subMoves[[nv]]
          if(varMoves[[nv]]) {
            PRESSRsquared[[nv]] <- wpair$pRsq[[nv]]
            psig[[nv]] <- wpair$pSig[[nv]]
            sig[[nv]] <- psig[[nv]]
            if(!is.null(catPRSquared)) {
              catPRSquared[[nv]] <- wpair$cRsq[[nv]]
              csig[[nv]] <- wpair$cSig[[nv]]
              sig[[nv]] <- csig[[nv]]
            }
          }
        }
      }
    }
  }
  plan <- list(treatments=treatments,
               vars=treatedVarNames,
               varMoves=varMoves,
               PRESSRsquared=PRESSRsquared,psig=psig,
               catPRSquared=catPRSquared,csig=csig,
               sig=sig,
               outcomename=outcomename,
               meanY=.wmean(zoY,weights),ndat=length(zoY))
  if(!is.null(catPRSquared)) {
    plan[['catPseudoRSquared']] <- catPRSquared
  }
  class(plan) <- 'treatmentplan'
  if(verbose) {
    print(paste("have treatment plan",date()))
  }
  plan
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
#' - varMoves : logical TRUE if the variable varied during training, only variables that move will be in the treated frame
#' - PRESSRsquared : a PRESS-held out R-squared of a linear fit from each variable to the y-value.  Scores of zero and below are very bad, scores near one are very good.
#' - catPseudoRSquared : the pseudo-Rsquared (deviance ratio) of each variable in turn logisticly regressed against the categorical y target.  Similar ot the PRESSRsquared this attempts to be a hold-out statistic
#'
#' See the vtreat vignette for a bit more detail and a worked example.
#'
#' @param dframe Data frame to learn treatments from (training data).
#' @param varlist Names of columns to treat (effective variables).
#' @param outcomename Name of column holding outcome variable.
#' @param outcometarget Value/level of outcome to be considered "success"
#' @param weights optional training weights for each row
#' @param minFraction optional minimum frequency a categorical level must have to be converted to an indicator column.
#' @param smFactor optional smoothing factor for impact coding models.
#' @param maxMissing optional maximum fraction (by data weight) of a categorical variable that are allowed before switching from indicators to impact coding.
#' @param collarProb what fraction of the data (pseudo-probability) to collar data at (<0.5).
#' @param scoreVars optional if TRUE attempt to estimate individual variable utility.
#' @param maxScoreSize optional maximum size for treated variable scoring frame
#' @param verbose if TRUE print progress.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow
#' @return treatment plan (for use with prepare)
#' @seealso \code{\link{prepare}} \code{\link{designTreatmentsN}} \code{\link{getNewVarNames}}
#' @examples
#' 
#' dTrainC <- data.frame(x=c('a','a','a','b','b','b'),
#'    z=c(1,2,3,4,5,6),
#'    y=c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))
#' dTestC <- data.frame(x=c('a','b','c',NA),
#'    z=c(10,20,30,NA))
#' treatmentsC <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE)
#' dTrainCTreated <- prepare(treatmentsC,dTrainC)
#' dTestCTreated <- prepare(treatmentsC,dTestC)
#' 
#' @export
designTreatmentsC <- function(dframe,varlist,outcomename,outcometarget,
                              weights=c(),
                              minFraction=0.02,smFactor=0.0,maxMissing=0.04,
                              collarProb=0.00,
                              scoreVars=TRUE,maxScoreSize=100000L,
                              verbose=TRUE,
                              parallelCluster=NULL) {
   zoY <- ifelse(dframe[[outcomename]]==outcometarget,1.0,0.0)
  .designTreatmentsX(dframe,varlist,outcomename,zoY,
                     dframe[[outcomename]],outcometarget,
                     weights,
                     minFraction,smFactor,maxMissing,
                     collarProb,
                     scoreVars,maxScoreSize,
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
#' - varMoves : logical TRUE if the variable varied during training, only variables that move will be in the treated frame
#' - PRESSRsquared : a PRESS-held out R-squared of a linear fit from each variable to the y-value.  Scores of zero and below are very bad, scores near one are very good.
#'
#' See the vtreat vignette for a bit more detail and a worked example.
#' 
#' @param dframe Data frame to learn treatments from (training data).
#' @param varlist Names of columns to treat (effective variables).
#' @param outcomename Name of column holding outcome variable.
#' @param weights optional training weights for each row
#' @param minFraction optional minimum frequency a categorical level must have to be converted to an indicator column.
#' @param smFactor optional smoothing factor for impact coding models.
#' @param maxMissing optional maximum fraction (by data weight) of a categorical variable that are allowed before switching from indicators to impact coding.
#' @param collarProb what fraction of the data (pseudo-probability) to collar data at (<0.5).
#' @param scoreVars optional if TRUE attempt to estimate individual variable utility.
#' @param maxScoreSize optional maximum size for treated variable scoring frame
#' @param verbose if TRUE print progress.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow
#' @return treatment plan (for use with prepare)
#' @seealso \code{\link{prepare}} \code{\link{designTreatmentsC}} \code{\link{getNewVarNames}}
#' @examples
#' 
#' dTrainN <- data.frame(x=c('a','a','a','a','b','b','b'),
#'     z=c(1,2,3,4,5,6,7),y=c(0,0,0,1,0,1,1))
#' dTestN <- data.frame(x=c('a','b','c',NA),
#'     z=c(10,20,30,NA))
#' treatmentsN = designTreatmentsN(dTrainN,colnames(dTrainN),'y')
#' dTrainNTreated <- prepare(treatmentsN,dTrainN)
#' dTestNTreated <- prepare(treatmentsN,dTestN)
#' 
#' @export
designTreatmentsN <- function(dframe,varlist,outcomename,
                              weights=c(),
                              minFraction=0.02,smFactor=0.0,maxMissing=0.04,
                              collarProb=0.00,
                              scoreVars=TRUE,maxScoreSize=1000000L,
                              verbose=TRUE,
                              parallelCluster=NULL) {
  ycol <- dframe[[outcomename]]
  .designTreatmentsX(dframe,varlist,outcomename,ycol,
                     c(),c(),
                              weights,
                              minFraction,smFactor,maxMissing,
                              collarProb,
                              scoreVars,maxScoreSize,
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
  scale=FALSE,doCollar=TRUE,
  varRestriction=c(),
  parallelCluster=NULL) {
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

