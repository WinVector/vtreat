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
.mkCatP <- function(origVarName,vcolin,rescol,levRestriction,weights) {
  origColClass <- class(vcolin)
  vcol <- .preProcCat(vcolin,c())
  num <- tapply(weights,vcol,sum)
  den <- sum(weights)
  scores <- num/den
  scores <- as.list(scores)
  treatment <- list(origvar=origVarName,origColClass=origColClass,
                    newvars=make.names(paste(origVarName,'catP',sep='_')),
                    f=.catP,
                    args=list(scores=scores,
                              levRestriction=levRestriction),
                    treatmentName='Prevalence Code',
                    treatmentCode='catP',
                    needsSplit=TRUE)
  pred <- treatment$f(vcolin,treatment$args)
  class(treatment) <- 'vtreatment'
  treatment$scales <- .getScales(pred,rescol,weights)
  treatment
}

