
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
  treatment <- list(origvar=origVarName,origColClass=origColClass,
                    newvars=make.names(paste(origVarName,'lev',tracked,sep="_"),unique=TRUE),
                    f=.catInd,
                    args=list(tracked=tracked,
                              levRestriction=levRestriction),
                    treatmentName='Categoric Indicators',
                    treatmentCode='lev')
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

