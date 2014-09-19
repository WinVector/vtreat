
# variable treatments type def: list { origvar, newvars, f(col,args), args, treatmentName, scales } can share orig var


.vtreatA <- function(x,...) UseMethod('.vtreatA',x)
.vtreatA.vtreatment <- function(vtreat,xcol,scale) {
  if(length(class(xcol))!=1) { # defend against POSIXt types
    xcol <- as.numeric(xcol)
  }
  dout <- as.data.frame(vtreat$f(xcol,vtreat$args),stringsAsFactors=FALSE)
  colnames(dout) <- vtreat$newvars
  if(scale) {
    for(j in 1:length(vtreat$scales$a)) {
      dout[,j] <- dout[,j]*vtreat$scales$a[j] + vtreat$scales$b[j]
    }
  }
  dout
}

.vtreatList <- function(treatments,dframe,scale) {
  cols <- c()
  if(length(treatments)>=1) {
    cols <- .vtreatA(treatments[[1]],dframe[,treatments[[1]]$origvar],scale)
    if(length(treatments)>=2) {
      for(i in 2:length(treatments)) {
        cols <- cbind(cols,.vtreatA(treatments[[i]],dframe[,treatments[[i]]$origvar],scale))
      }
    }
  }
  as.data.frame(cols,stringsAsFactors=FALSE)
}

# try and neaten up vtreatment class a bit
vorig <- function(x,...) UseMethod('vorig',x)
vorig.vtreatment <- function(vtreat) { vtreat$origvar }
vnames <- function(x,...) UseMethod('vnames',x)
vnames.vtreatment <- function(vtreat) { vtreat$newvars }
show.vtreatment <- function(vtreat,...) { paste(
  'vtreat \'',vtreat$treatmentName,
  '\'(\'',vtreat$origvar,'\'->\'',
  paste(vtreat$newvars,collapse='\',\''),
  '\')',sep='') }
print.vtreatment <- function(vtreat,...) { print(show.vtreatment(vtreat),...) }




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
  vals <- matrix(data=0,nrow=length(col),ncol=length(args$tracked))
  sum <- rep(0,length(col))
  for(j in 1:length(args$tracked)) {
    vi <- ifelse(col==args$tracked[j],1.0,0.0) 
    vals[,j] <- vi
    sum = sum + vi
  }
  for(ri in which(sum==0)) { # For novel levels put fraction of time each level was on in original data
    for(j in 1:length(args$tracked)) {
      vals[ri,j] = args$dist[j]
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
  nvar <- (dim(pred)[[2]])
  treatment$scales <- list('a'=rep(1.0,nvar),'b'=rep(0.0,nvar))  
  for(j in 1:nvar) {
    scales <- .getScales(pred[,j],ynumeric,weights)
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
.hold1OutMeans <- function(y,weights) {
  # get per-datum hold-1 out grand means
  sumY <- sum(y*weights)
  sumW <- sum(weights)
  meanP <- (sumY - y*weights)/(sumW - weights)
  meanP[is.na(meanP)] <- 0.5
  meanP
}

# compute the PRESS statistic of 
# x,y: numeric vectors (no NAs/NULLs)
# weights numeric, non-negative, no NAs/NULLs at least two positive positions
# all vectors same length
# return PRESS statistic of model y ~ a*x + b divided by pressStatOfBestConstant(y,weights)
pressStatOfBestLinearFit <- function(x,y,weights) {
  n <- length(x)
  if(n<=1) {
    return(0.0)
  }
  if(!.has.range.cn(x)) {
    return(1.0)
  }
  error <- 0.0
  # get per-datum hold-1 out grand means (used for smoothing and fallback)
  meanP <- .hold1OutMeans(y,weights)
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
  eConst <- sum(weights*(y-meanY)^2)
  error/eConst
}

# compute the PRESS statistic of 
# vcol: character 
# y: numeric vectors (no NAs/NULLs)
# x: general categorical
# weights numeric, non-negative, no NAs/NULLs at least two positive positions
# all vectors same length
# smoothingTerm scalar >= 0
# return PRESS statistic of model y ~ x divided by pressStatOfBestConstant(y,weights)
pressStatOfCategoricalVariable <- function(vcolin,y,weights,smoothingTerm=0.5) {
  n <- length(vcolin)
  if(n<=1) {
    return(0.0)
  }
  if(!.has.range(vcolin)) {
    return(1.0)
  }
  # get per-datum hold-1 out grand means (used for smoothing and fallback)
  meanP <- .hold1OutMeans(y,weights)
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
  eConst <- sum(weights*(y-meanY)^2)
  error/eConst
}



# check if a variable is at all useful
.scoreVN <- function(x,y,weights) {
  pressStatOfBestLinearFit(x,y,weights)
}

# check if a variable is at all useful
.scoreVC <- function(x,y,weights) {
  pressStatOfBestLinearFit(x,ifelse(y,1.0,0.0),weights)
}

# score list of columns related to numeric outcome
.scoreColumnsN <- function(treatedFrame,yValues,weights,exclude) {
  sapply(setdiff(colnames(treatedFrame),exclude),
         function(c) .scoreVN(treatedFrame[,c],yValues,weights))
}

# score list of columns related to a categorical outcome
.scoreColumnsC <- function(treatedFrame,yValues,weights,exclude) {
  sapply(setdiff(colnames(treatedFrame),exclude),
         function(c) .scoreVC(treatedFrame[,c],yValues,weights))
}



# build all treatments for a data frame to predict a categorical outcome
designTreatmentsC <- function(dframe,varlist,outcomename,outcometarget,
                              weights=c(),
                              minFraction=0.02,smFactor=0.0,maxMissing=0.04) {
  varlist <- setdiff(varlist,outcomename)
  if(is.null(weights)) {
    weights <- rep(1.0,dim(dframe)[[1]])
  } else {
    goodPosns <- ifelse(.is.bad(weights),FALSE,weights>0.0)
    dframe <- dframe[goodPosns,]
    weights <- weights[goodPosns]
  }
  treatments <- list()
  ycol <- dframe[,outcomename]==outcometarget
  if(length(ycol)<=0) {
    stop("no non-zero weighted rows")
  }
  if(sum(.is.bad(ycol))>0) {
    stop("outcome variable had NAs")
  }
  if(!.has.range.cn(ycol)) {
    stop("outcome variable was a constant")
  }
  zoY <- ifelse(ycol,1.0,0.0)
  if((sum(zoY)<=0)||(sum(zoY)>=length(zoY))) {
    stop("outcome variable doesn't vary with respect to target")
  }
  cvarScores <- list()
  for(v in varlist) {
    vcol <- dframe[,v]
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
          cvarScores[ti$newvars[[1]]] <- pressStatOfCategoricalVariable(vcol,zoY,weights) # assumes only one newvar
        }
      }
    }
  }
  treated <- .vtreatList(treatments,dframe,TRUE)
  varMoves <- sapply(colnames(treated),function(c) { .has.range.cn(treated[,c]) })
  adjRsquared <- sapply(colnames(treated),function(c) { summary(lm(zoY~treated[,c]))$adj.r.squared })
  Rsquared <- sapply(colnames(treated),function(c) { summary(lm(zoY~treated[,c]))$r.squared })
  varScores <- append(.scoreColumnsC(treated,ycol,weights,names(cvarScores)),cvarScores)[colnames(treated)]
  plan <- list(treatments=treatments,
               vars=names(varScores),
               varScores=varScores,PRESSRsquared=lapply(varScores,function(x) 1-x),
               Rsquared=Rsquared,adjRsquared=adjRsquared,
               varMoves=varMoves,
               outcomename=outcomename,
               meanY=.wmean(zoY,weights),ndat=length(zoY))
  class(plan) <- 'treatmentplan'
  plan
}

# build all treatments for a data frame to predict a numeric outcome
designTreatmentsN <- function(dframe,varlist,outcomename,
                              weights=c(),
                              minFraction=0.02,smFactor=0.0,maxMissing=0.04) {
  varlist <- setdiff(varlist,outcomename)
  if(is.null(weights)) {
    weights <- rep(1.0,dim(dframe)[[1]])
  } else {
    goodPosns <- ifelse(.is.bad(weights),FALSE,weights>0.0)
    dframe <- dframe[goodPosns,]
    weights <- weights[goodPosns]
  }
  treatments <- list()
  ycol <- dframe[,outcomename]
  if(length(ycol)<=0) {
    stop("no non-zero weighted rows")
  }
  if(!((class(ycol)=='numeric') || (class(ycol)=='integer'))) {
    stop("outcome must be numeric or integer")
  }
  if(sum(.is.bad(ycol))>0) {
    stop("outcome variable had NAs/NaNs/infinites")
  }
  if(!.has.range.cn(ycol)) {
    stop("outcome variable was a constant")
  }
  cvarScores <- list()
  for(v in varlist) {
    vcol <- dframe[,v]
    colclass <- class(vcol)
    if(length(colclass)!=1) { # defend against POSIXt types
      vcol <- as.numeric(vcol)
      colclass <- class(vcol)
    }
    if(.has.range(vcol)) {
      if((colclass=='numeric') || (colclass=='integer')) {
        ti <- .mkPassThrough(v,vcol,ycol,weights)
        if(!is.null(ti)) {
          treatments[[length(treatments)+1]] <- ti
        }
        ti <- .mkIsNA(v,vcol,ycol,weights)
        if(!is.null(ti)) {
          treatments[[length(treatments)+1]] <- ti
        }
      } else {
        ti <- .mkCatInd(v,vcol,ycol,minFraction,maxMissing,weights)
        if(is.null(ti)) {
          ti <- .mkCatNum(v,vcol,ycol,smFactor,weights)
        }
        if(!is.null(ti)) {
          treatments[[length(treatments)+1]] <- ti
          cvarScores[ti$newvars[[1]]] <- pressStatOfCategoricalVariable(vcol,ycol,weights) # assumes only one newvar
        }
      }
    }
  }
  treated <- .vtreatList(treatments,dframe,TRUE)
  varMoves <- sapply(colnames(treated),function(c) { .has.range.cn(treated[,c]) })
  adjRsquared <- sapply(colnames(treated),function(c) { summary(lm(ycol~treated[,c]))$adj.r.squared })
  Rsquared <- sapply(colnames(treated),function(c) { summary(lm(ycol~treated[,c]))$r.squared })
  varScores <- append(.scoreColumnsN(treated,ycol,weights,names(cvarScores)),cvarScores)[colnames(treated)]
  plan <- list(treatments=treatments,
               vars=names(varScores),
               varScores=varScores,PRESSRsquared=lapply(varScores,function(x) 1-x),
               Rsquared=Rsquared,adjRsquared=adjRsquared,
               varMoves=varMoves,
               outcomename=outcomename,
               meanY=.wmean(ycol,weights),ndat=length(ycol))
  class(plan) <- 'treatmentplan'
  plan
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
prepare <- function(treatmentplan,dframe,pruneLevel=0.99,scale=FALSE,logitTransform=FALSE) {
  treated <- .vtreatList(treatmentplan$treatments,dframe,scale)
  usableVars <- treatmentplan$vars
  usableVars <- intersect(usableVars,names(treatmentplan$varMoves)[treatmentplan$varMoves])
  usableVars <- intersect(usableVars,names(treatmentplan$varScores)[treatmentplan$varScores>0])
  if(!is.null(pruneLevel)) {
    usableVars <- intersect(usableVars,names(treatmentplan$varScores)[treatmentplan$varScores<=pruneLevel])
  }
  treated <- treated[,usableVars,drop=FALSE]
  if(logitTransform) {
    epsilon <- 1.0/treatmentplan$ndat
    for(c in colnames(treated)) {
      treated[,c] <- .logit(treated[,c]+treatmentplan$meanY,epsilon)
    }
  }
  if(treatmentplan$outcomename %in% colnames(dframe)) {
    treated[,treatmentplan$outcomename] <- dframe[,treatmentplan$outcomename]
  }
  treated
}

