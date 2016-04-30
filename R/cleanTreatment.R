

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
  if(is.na(nadist)) {
    nadist <- 0
  }
  xcol[napositions] <- nadist
  if(max(xcol)<=min(xcol)) {
    return(c())
  }
  newVarName <- make.names(paste(origVarName,'clean',sep='_'))
  treatment <- list(origvar=origVarName,
                    newvars=newVarName,
                    f=.passThrough,
                    args=list(nadist=nadist,cuts=cuts),
                    treatmentName='Scalable pass through',
                    treatmentCode='clean',
                    needsSplit=FALSE)
  class(treatment) <- 'vtreatment'
  treatment$scales <- linScore(newVarName,xcol,ycol,weights)
  treatment
}
