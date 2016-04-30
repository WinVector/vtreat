

# apply a prevalence fact
.catP <- function(col,args,doCollar) {
  col <- .preProcCat(col,NULL)
  novel <- !(col %in% names(args$scores))
  keys <- col
  pred <- numeric(length(col))
  if(length(args$scores)>0) {
    keys[novel] <- names(args$scores)[[1]]   # just to prevent bad lookups
    pred <- as.numeric(args$scores[keys]) 
  }
  pred[novel] <- 0.0 
  pred
}

# build a prevalence fact
.mkCatP <- function(origVarName,vcolin,zoY,zC,zTarget,levRestriction,weights) {
  vcol <- .preProcCat(vcolin,c())
  num <- tapply(weights,vcol,sum)
  den <- sum(weights)
  scores <- num/den
  scores <- as.list(scores)
  newVarName <- make.names(paste(origVarName,'catP',sep='_'))
  treatment <- list(origvar=origVarName,
                    newvars=newVarName,
                    f=.catP,
                    args=list(scores=scores,
                              levRestriction=levRestriction),
                    treatmentName='Prevalence Code',
                    treatmentCode='catP',
                    needsSplit=TRUE)
  pred <- treatment$f(vcolin,treatment$args)
  if(!.has.range.cn(pred)) {
    return(NULL)
  }
  class(treatment) <- 'vtreatment'
  treatment$scales <- linScore(newVarName,pred,zoY,weights)
  treatment
}

.jackknifeCatP <- function(vcolin,weights) {
  vcol <- .preProcCat(vcolin,c())
  num <- tapply(weights,vcol,sum)
  den <- sum(weights)
  # # re-vectorize by example rows and Jacknife by pulling self out
  num <- num[vcol] - weights
  den <- pmax(rep(den,length(num)) - weights,1.0e-3)
  scores <- num/den
  scores
}
