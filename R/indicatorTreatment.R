
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

.mkCatInd_a <- function(origVarName,vcolin,ynumeric,zC,zTarget,minFraction,levRestriction,weights,catScaling) {
  tracked <- levRestriction$tracked
  if(length(tracked)<=0) {
    return(c())
  }
  newVarNames <- make.names(paste(origVarName,'lev',tracked,sep="_"),unique=TRUE)
  treatment <- list(origvar=origVarName,
                    newvars=newVarNames,
                    f=.catInd,
                    args=list(tracked=tracked,
                              levRestriction=levRestriction),
                    treatmentName='Categoric Indicators',
                    treatmentCode='lev',
                    needsSplit=FALSE,
                    extraModelDegrees=0)
  class(treatment) <- 'vtreatment'
  pred <- treatment$f(vcolin,treatment$args)
  treatment$pred <- pred
  treatment
}


.mkCatNworker <- function(newVarNames, pred, ynumeric, weights) {
  force(newVarNames)
  force(pred)
  force(ynumeric)
  force(weights)
  function(j) {
    linScore(newVarNames[[j]], pred[[j]], ynumeric, weights)
  }
}

.mkCatCworker <-  function(newVarNames, pred, zC, zTarget, weights) {
  force(newVarNames)
  force(pred)
  force(zC)
  force(zTarget)
  force(weights)
  function(j) {
    catScore(newVarNames[[j]], pred[[j]], zC, zTarget, weights)
  }
}

.mkCatInd_scales <- function(treatment,
                             ynumeric, zC, zTarget,
                             weights, catScaling,
                             parallelCluster) {
  newVarNames <- treatment$newvars
  pred <- treatment$pred
  treatment$pred <- NULL
  treatment$scales <- NULL
  if(length(newVarNames)>0) {
    if((!catScaling)||(is.null(zC))) {
      worker <- .mkCatNworker(newVarNames, pred, ynumeric, weights) 
    } else {
      worker <- .mkCatCworker(newVarNames, pred, zC, zTarget, weights)
    }
    scaleList <- plapply(seq_len(length(newVarNames)),
                         worker,
                         parallelCluster)
    treatment$scales <- .rbindListOfFrames(scaleList)
  }
  treatment
}

# build categorical indicators
.mkCatInd <- function(origVarName,
                      vcolin,
                      ynumeric, zC, zTarget,
                      minFraction, levRestriction,
                      weights,
                      catScaling,
                      parallelCluster) {
  treatment <- .mkCatInd_a(origVarName,vcolin,ynumeric,zC,zTarget,minFraction,levRestriction,weights,catScaling)
  treatment <- .mkCatInd_scales(treatment,
                                ynumeric, zC, zTarget,
                                weights, catScaling,
                                parallelCluster = parallelCluster)
  treatment
}

