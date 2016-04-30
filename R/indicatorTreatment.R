
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
.mkCatInd <- function(origVarName,vcolin,ynumeric,minFraction,levRestriction,weights) {
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
  newVarNames <- make.names(paste(origVarName,'lev',tracked,sep="_"),unique=TRUE)
  treatment <- list(origvar=origVarName,
                    newvars=newVarNames,
                    f=.catInd,
                    args=list(tracked=tracked,
                              levRestriction=levRestriction),
                    treatmentName='Categoric Indicators',
                    treatmentCode='lev',
                    needsSplit=FALSE)
  class(treatment) <- 'vtreatment'
  pred <- treatment$f(vcolin,treatment$args)
  scaleList <- lapply(seq_len(length(newVarNames)),
                      function(j) {
                        linScore(newVarNames[[j]],pred[[j]],ynumeric,weights)
                      })
  treatment$scales <- .rbindListOfFrames(scaleList)
  treatment
}

