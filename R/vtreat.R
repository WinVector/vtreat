# variable treatments type def: list { origvar, newvars, f(col,args), args, treatmentName, scales } can share orig var


.vtreatA <- function(vtreat,xcol,scale) {
  if(length(class(xcol))!=1) { # defend against POSIXt types
    xcol <- as.numeric(xcol)
  }
  dout <- as.data.frame(vtreat$f(xcol,vtreat$args),stringsAsFactors=FALSE)
  colnames(dout) <- vtreat$newvars
  if(scale) {
    for(j in seq_along(vtreat$scales$a)) {
      dout[[j]] <- dout[[j]]*vtreat$scales$a[[j]] + vtreat$scales$b[[j]]
    }
  }
  dout
}


.getNewVarNames <- function(treatments) {
  resCount <- 0
  for(ti in treatments) {
     resCount <- resCount + length(ti$newvars)
  }
  names <- vector('list',resCount)
  j <- 1
  for(ti in treatments) {
     for(ni in ti$newvars) {
        names[[j]] <- ni
        j <- j + 1
     }
  }
  names
}

.vtreatList <- function(treatments,dframe,scale) {
  colNames <- .getNewVarNames(treatments)
  cols <- vector('list',length(colNames))
  names(cols) <- colNames
  j <- 1
  for(ti in treatments) {
     for(ci in .vtreatA(ti,dframe[[ti$origvar]],scale)) {
        cols[[j]] <- ci
        j <- j + 1
     }
  }
  as.data.frame(cols,stringsAsFactors=FALSE)
}

#'
#' Original variable name.
#' @param x vtreatment item.
#' @param ... additional args (to match general signature)
#' 
vorig <- function(x,...) UseMethod('vorig',x)

#'
#' Original variable name.
#' @param x vtreatment item.
#' @param ... additional args (to match general signature).
#' 
vorig.vtreatment <- function(x,...) { x$origvar }

#'
#' New treated variable names.
#' @param x vtreatment item.
#' @param ... additional args (to match general signature).
#' 
vnames <- function(x,...) UseMethod('vnames',x)

#'
#' New treated variable names.
#' @param x vtreatment item
#' @param ... additional args (to match general signature).
#' 
vnames.vtreatment <- function(x,...) { x$newvars }

#'
#' Display treatment plan.
#' @param vtreat treatment plan
#' @param ... additional args (to match general signature).
#' 
show.vtreatment <- function(vtreat,...) { paste(
  'vtreat \'',vtreat$treatmentName,
  '\'(\'',vtreat$origvar,'\'->\'',
  paste(vtreat$newvars,collapse='\',\''),
  '\')',sep='') }

#'
#' Print treatment plan.
#' @param x treatmet plan
#' @param ... additional args (to match general signature).
#' 
print.vtreatment <- function(x,...) { print(show.vtreatment(x),...) }




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
.passThrough <- function(col,args) {
  treated <- as.numeric(col)
  treated[.is.bad(treated)] <- args$nadist
  treated
}

.mkPassThrough <- function(origVarName,xcol,ycol,weights) {
  xcol <- as.numeric(xcol)
  napositions <- .is.bad(xcol)
  nna <- sum(napositions)
  if(nna>=length(xcol)) {
    return(c())
  }
  nadist <- .wmean(xcol[!napositions],weights[!napositions])
  xcol[napositions] <- nadist
  if(max(xcol)<=min(xcol)) {
    return(c())
  }
  treatment <- list(origvar=origVarName,newvars=make.names(paste(origVarName,'clean',sep='_')),
                    f=.passThrough,
                    args=list(nadist=nadist),
                    treatmentName='Scalable pass through')
  class(treatment) <- 'vtreatment'
  treatment$scales <- .getScales(xcol,ycol,weights)
  treatment
}




# return if a variable is NA
.isNA <- function(col,args) {
  treated <- ifelse(.is.bad(col),1.0,0.0)
  treated
}

.mkIsNA <- function(origVarName,xcol,ynumeric,weights) {
  badIDX <- .is.bad(xcol)
  nna <- sum(badIDX)
  if((nna<=0)||(nna>=length(xcol))) {
    return(c())
  }
  if(.wmean(ynumeric[badIDX],weights[badIDX])==.wmean(ynumeric[!badIDX],weights[!badIDX])) {
    return(c())
  }
  treatment <- list(origvar=origVarName,newvars=make.names(paste(origVarName,'isBAD',sep='_')),
                    f=.isNA,
                    args=list(),
                    treatmentName='is.bad')
  class(treatment) <- 'vtreatment'
  treatment$scales <- .getScales(ifelse(badIDX,1.0,0.0),ynumeric,weights)
  treatment
}


# return categorical indicators
.catInd <- function(col,args) {
  origna <- is.na(col)
  col <- paste('x',as.character(col))
  col[origna] <- 'NA'
  nres <- length(args$tracked)
  vals <- vector('list',nres)
  sum <- rep(0,length(col))
  for(j in 1:nres) {
    vi <- ifelse(col==args$tracked[j],1.0,0.0) 
    vals[[j]] <- vi
    sum = sum + vi
  }
  if(nres>1) {
     for(ri in which(sum==0)) { # For novel levels put fraction of time each level was on in original data
        for(j in 1:nres) {
           vals[[j]][[ri]] = args$dist[[j]]
        }
     }
  }
  vals
}

# build categorical indicators
.mkCatInd <- function(origVarName,vcolin,ynumeric,minFraction,maxMissing,weights) {
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
  nind <- length(tracked)
  treatment <- list(origvar=origVarName,newvars=make.names(paste(origVarName,'lev',tracked,sep="_"),unique=TRUE),
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
# replace level with .wmean(x|categor) - .wmean(x)
.catNum <- function(col,args) {
  origna <- is.na(col)
  col <- paste('x',as.character(col)) # R can't use empty string as a key
  col[origna] <- 'NA' 
  novel <- !(col %in% names(args$scores))
  keys <- col
  keys[novel] <- names(args$scores)[[1]]  # just to prevent bad lookups
  pred <- as.numeric(args$scores[keys]) 
  pred[novel] <- args$novelvalue  # mean delta impact avergaed over all possibilities, should be zero in scaled mode, mean dist in unscaled
  pred
}

# build a numeric impact model
# see: http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/
.mkCatNum <- function(origVarName,vcolin,rescol,smFactor,weights) {
  origna <- is.na(vcolin)
  vcol <- paste('x',as.character(vcolin)) # R can't use empty string as a key
  vcol[origna] <- 'NA'
  baseMean <- .wmean(rescol,weights)
  num <- tapply(rescol*weights,vcol,sum)
  den <- tapply(weights,vcol,sum)
  scores <- (num+smFactor*baseMean)/(den+smFactor)-baseMean
  novelvalue <- sum(scores*den)/sum(den)
  scores <- as.list(scores)
  treatment <- list(origvar=origVarName,newvars=make.names(paste(origVarName,'catN',sep='_')),
                    f=.catNum,
                    args=list(scores=scores,novelvalue=novelvalue),
                    treatmentName='Scalable Impact Code')
  pred <- treatment$f(vcolin,treatment$args)
  class(treatment) <- 'vtreatment'
  treatment$scales <- .getScales(pred,rescol,weights)
  treatment
}







# weighted PRESS statistic of a weighted mean
# so in this case it is sum((y_i - meanAllBut(y,i))^) where mean is computed of all but the i'th datum
# y numeric, no NAs/NULLS
# weights numeric, non-negative, no NAs/NULLs at least two positive positions
# all vectors same length
#'
#' Return a vector of length(y) where the i-th entry is the weighted mean 
#' of all but the i-th y.  Usefull for normalizing PRESS style statistics.
#' @param y values to average (should not have NAs).
#' @param weights data weighing (should not have NAs, be non-negative and not all zero).
hold1OutMeans <- function(y,weights) {
  # get per-datum hold-1 out grand means
  sumY <- sum(y*weights)
  sumW <- sum(weights)
  meanP <- (sumY - y*weights)/(sumW - weights)
  meanP[is.na(meanP)] <- 0.5
  meanP
}


# y: numeric vector no null/NAs
# w: numeric vector same length as y, no negative/null/NAs at least 2 position non-zer
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


#' Compute the PRESS statistic of a 1-variable linear model
#' @param x numeric (no NAs/NULLs) effective variable
#' @param y numeric (no NAs/NULLs) outcome variable
#' @param weights numeric, non-negative, no NAs/NULLs at least two positive positions
#' @param normalizationStrat 'none': no normalization (traditional PRESS), 'total': divide by total variation, 'holdout': divide by 1-hold out variation (PRESS-line, larger than total variation)
#' @return PRESS statistic of model y ~ a*x + b divided by pressStatOfBestConstant(y,weights)
pressStatOfBestLinearFit <- function(x,y,weights,normalizationStrat='total') {
  n <- length(x)
  if(n<=1) {
    return(0.0)
  }
  if(!.has.range.cn(x)) {
    return(1.0)
  }
  error <- 0.0
  # get per-datum hold-1 out grand means (used for smoothing and fallback)
  meanP <- hold1OutMeans(y,weights)
  a <- matrix(data=0,nrow=2,ncol=2)
  a[1,1] = 1.0e-5
  a[2,2] = 1.0e-5
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
    ye <- meanP[i] # const fn solution, for fallback
    tryCatch(
      ye <- sum(solve(aM,bM) * c(1,xi)),
      warning = function(w) {},
      error = function(e) {})
    error <- error + wi*(yi-ye)^2
  }
  meanY <- .wmean(y,weights)
  eConst <- .PRESSnormalization(normalizationStrat,y,weights)
  error/eConst
}

#' Compute the PRESS statistic a single categorical model.   Tries to prevent some of the test/train leakage in scoring
#' (so apply this directly to a categorical variable, and don't score an impact coded varaible).
#' @param vcolin character 
#' @param y numeric vectors (no NAs/NULLs)
#' @param weights numeric, non-negative, no NAs/NULLs at least two positive positions
#' @param normalizationStrat 'none': no normalization (traditional PRESS), 'total': divide by total variation, 'holdout': divide by 1-hold out variation (PRESS-line, larger than total variation)
#' @param smoothingTerm scalar >= 0
#' @return PRESS statistic of model y ~ x divided by pressStatOfBestConstant(y,weights)
pressStatOfCategoricalVariable <- function(vcolin,y,weights,normalizationStrat='total',smoothingTerm=0.5) {
  n <- length(vcolin)
  if(n<=1) {
    return(0.0)
  }
  if(!.has.range(vcolin)) {
    return(1.0)
  }
  # get per-datum hold-1 out grand means (used for smoothing and fallback)
  meanP <- hold1OutMeans(y,weights)
  origna <- is.na(vcolin)
  vcol <- paste('x',as.character(vcolin)) # R can't use empty string as a key
  vcol[origna] <- 'NA'
  num <- tapply(y*weights,vcol,sum) 
  den <- tapply(weights,vcol,sum)
  preds <- (num[vcol] - y*weights + smoothingTerm*meanP)/(den[vcol] - weights + smoothingTerm)
  valid <- !is.na(preds)
  if(sum(valid)<=0) {
    return(1.0)
  }
  if(sum(valid)<n) {
     # hold-1 out grand mean predictions
     preds[!valid] <- meanP[!valid]
  }
  error <- sum(weights*(y-preds)^2)
  meanY <- .wmean(y,weights)
  eConst <- .PRESSnormalization(normalizationStrat,y,weights)
  error/eConst
}


# score list of columns related to numeric outcome
.scoreColumnsN <- function(treatedFrame,yValues,weights,exclude,normalizationStrat) {
  nms <- setdiff(colnames(treatedFrame),exclude)
  scores <- vapply(nms,
         function(c) pressStatOfBestLinearFit(treatedFrame[[c]],yValues,weights,normalizationStrat),
         double(1))
  names(scores) <- nms
  scores
}



# build all treatments for a data frame to predict a given outcome
.designTreatmentsX <- function(dframe,varlist,outcomename,zoY,
                              weights,
                              minFraction,smFactor,maxMissing,
                              scoreVars,verbose) {
  varlist <- setdiff(varlist,outcomename)
  if(is.null(weights)) {
    weights <- rep(1.0,dim(dframe)[[1]])
  } else {
    goodPosns <- ifelse(.is.bad(weights),FALSE,weights>0.0)
    dframe <- dframe[goodPosns,]
    weights <- weights[goodPosns]
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
  treatments <- list()
  cvarScores <- list()
  for(v in varlist) {
    if(verbose) {
      print(paste('design var',v,date()))
    }
    vcol <- dframe[[v]]
    colclass = class(vcol)
    if(length(colclass)!=1) { # defend against POSIXt types
      vcol <- as.numeric(vcol)
      colclass <- class(vcol)
    }
    if(.has.range(vcol)) {
      if((colclass=='numeric') || (colclass=='integer')) {
        ti <- .mkPassThrough(v,vcol,zoY,weights)
        if(!is.null(ti)) {
          treatments[[length(treatments)+1]] <- ti
        }
        ti <- .mkIsNA(v,vcol,zoY,weights)
        if(!is.null(ti)) {
          treatments[[length(treatments)+1]] <- ti
        }
      } else {
        ti <- .mkCatInd(v,vcol,zoY,minFraction,maxMissing,weights)
        if(is.null(ti)) {
          ti <- .mkCatNum(v,vcol,zoY,smFactor,weights)
        }
        if(!is.null(ti)) {
          treatments[[length(treatments)+1]] <- ti
          if (scoreVars) {
             cvarScores[ti$newvars[[1]]] <- pressStatOfCategoricalVariable(vcol,zoY,weights) # assumes only one newvar
          }
        }
      }
    }
  }
  treatedVarNames <- .getNewVarNames(treatments)
  varMoves <- c()
  varScores <- c()
  PRESSRsquared <- c()
  if (scoreVars) {
     if(verbose) {
        print(paste("treat frame",date()))
     }
     treated <- .vtreatList(treatments,dframe,TRUE)
     if(verbose) {
        print(paste("score frame",date()))
     }
     varMoves <- vapply(colnames(treated),function(c) { .has.range.cn(treated[[c]]) },logical(1))
     names(varMoves) <- colnames(treated)
     varScores <- rep(1.0,length(varMoves))
     names(varScores) <- colnames(treated)
     varScores[names(cvarScores)] <- as.numeric(cvarScores)
     additionalScores <- .scoreColumnsN(treated,zoY,weights,union(names(cvarScores),names(varMoves)[!varMoves]),'total')
     varScores[names(additionalScores)] <- additionalScores
     treatedVarNames <- names(varScores)
     PRESSRsquared <- 1-varScores
  }
  plan <- list(treatments=treatments,
               vars=treatedVarNames,
               varScores=varScores,PRESSRsquared=PRESSRsquared,
               varMoves=varMoves,
               outcomename=outcomename,
               meanY=.wmean(zoY,weights),ndat=length(zoY))
  class(plan) <- 'treatmentplan'
  plan
}



# build all treatments for a data frame to predict a categorical outcome


#' designTreatmentsC 
#' 
#' Function to design variable treatments for binary prediction of a
#' categorical outcome.  Data frame is assumed to have only atomic columns
#' except for dates (which are converted to numeric).
#' 
#' @param dframe Data frame to learn treatments from (training data).
#' @param varlist Names of columns to treat (effetive variables).
#' @param outcomename Name of column holding outcome variable.
#' @param outcometarget Value/level of outcome to be considered "success"
#' @param weights optional training weights for each row
#' @param minFraction optional minimum frequency a categorical level must have to be converted to an indicator column.
#' @param smFactor optional smoothing factor for impact coding models.
#' @param maxMissing optional maximum fraction (by data weight) of a categorical variable that are allowed before switching from indicators to impact coding.
#' @param scoreVars optional if TRUE attempt to estimate individual variable utility.
#' @param verbose if TRUE print progress.
#' @return treatment plan (for use with prepare)
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
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
designTreatmentsC <- function(dframe,varlist,outcomename,outcometarget,
                              weights=c(),
                              minFraction=0.02,smFactor=0.0,maxMissing=0.04,
                              scoreVars=TRUE,verbose=TRUE) {
   zoY <- ifelse(dframe[[outcomename]]==outcometarget,1.0,0.0)
  .designTreatmentsX(dframe,varlist,outcomename,zoY,
                              weights,
                              minFraction,smFactor,maxMissing,
                              scoreVars,verbose)
}

# build all treatments for a data frame to predict a numeric outcome


#' designTreatmentsN 
#' 
#' Function to design variable treatments for binary prediction of a
#' numeric outcome.  Data frame is assumed to have only atomic columns
#' except for dates (which are converted to numeric).
#' 
#' @param dframe Data frame to learn treatments from (training data).
#' @param varlist Names of columns to treat (effetive variables).
#' @param outcomename Name of column holding outcome variable.
#' @param weights optional training weights for each row
#' @param minFraction optional minimum frequency a categorical level must have to be converted to an indicator column.
#' @param smFactor optional smoothing factor for impact coding models.
#' @param maxMissing optional maximum fraction (by data weight) of a categorical variable that are allowed before switching from indicators to impact coding.
#' @param scoreVars optional if TRUE attempt to estimate individual variable utility.
#' @param verbose if TRUE print progress.
#' @return treatment plan (for use with prepare)
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
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
designTreatmentsN <- function(dframe,varlist,outcomename,
                              weights=c(),
                              minFraction=0.02,smFactor=0.0,maxMissing=0.04,
                              scoreVars=TRUE,verbose=TRUE) {
   ycol <- dframe[[outcomename]]
  .designTreatmentsX(dframe,varlist,outcomename,ycol,
                              weights,
                              minFraction,smFactor,maxMissing,
                              scoreVars,verbose)
}



# safe logit transform
.logit <- function(x,epsilon) {
  x <- pmin(pmax(as.numeric(x),epsilon),1.0-epsilon)
  x <- log(x/(1.0-x))
  x[.is.bad(x)] <- 0.0
  x
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
#' 
#' @param treatmentplan Plan built by designTreantmentsC() or designTreatmentsN()
#' @param dframe Data frame to be treated
#' @param pruneLevel optional supress variables with varScore below this threshold.
#' @param scale optional if TRUE replace numeric variables with regression ("move to outcome-scale").
#' @param logitTransform if TRUE and scale is also TRUE, then logit transform probabilities.
#' @return treated data frame (all columns numeric, without NA,NaN)
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @examples
#' 
#' dTrainN <- data.frame(x=c('a','a','a','a','b','b','b'),
#'     z=c(1,2,3,4,5,6,7),y=c(0,0,0,1,0,1,1))
#' dTestN <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))
#' treatmentsN = designTreatmentsN(dTrainN,colnames(dTrainN),'y')
#' dTrainNTreated <- prepare(treatmentsN,dTrainN)
#' dTestNTreated <- prepare(treatmentsN,dTestN)
#' 
#' dTrainC <- data.frame(x=c('a','a','a','b','b','b'),
#'     z=c(1,2,3,4,5,6),y=c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))
#' dTestC <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))
#' treatmentsC <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE)
#' dTrainCTreated <- prepare(treatmentsC,dTrainC)
#' dTestCTreated <- prepare(treatmentsC,dTestC)
#' 
#' 
#' 
prepare <- function(treatmentplan,dframe,pruneLevel=0.99,scale=FALSE,logitTransform=FALSE) {
  treated <- .vtreatList(treatmentplan$treatments,dframe,scale)
  usableVars <- treatmentplan$vars
  if(!is.null(treatmentplan$varMoves)) {
    usableVars <- intersect(usableVars,names(treatmentplan$varMoves)[treatmentplan$varMoves])
  }
  if(!is.null(treatmentplan$varScores)) {
    usableVars <- intersect(usableVars,names(treatmentplan$varScores)[treatmentplan$varScores>0])
    if(!is.null(pruneLevel)) {
      usableVars <- intersect(usableVars,names(treatmentplan$varScores)[treatmentplan$varScores<=pruneLevel])
    }
  }
  treated <- treated[,usableVars,drop=FALSE]
  if(logitTransform&&scale) {
    epsilon <- 1.0/treatmentplan$ndat
    for(c in colnames(treated)) {
      treated[[c]] <- .logit(treated[[c]]+treatmentplan$meanY,epsilon)
    }
  }
  if(treatmentplan$outcomename %in% colnames(dframe)) {
    treated[[treatmentplan$outcomename]] <- dframe[[treatmentplan$outcomename]]
  }
  treated
}

