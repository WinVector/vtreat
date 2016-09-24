

# apply a prevalence fact
.catP <- function(col,args,doCollar) {
  col <- .preProcCat(col,NULL)
  unhandledNovel <- !(col %in% names(args$scores))
  keys <- col
  pred <- numeric(length(col))
  if(length(args$scores)>0) {
    keys[unhandledNovel] <- names(args$scores)[[1]]   # just to prevent bad lookups
    pred <- as.numeric(args$scores[keys]) 
  }
  pred[unhandledNovel] <- 0.0 
  pred
}

# build a prevalence fact
.mkCatP <- function(origVarName,vcolin,zoY,zC,zTarget,levRestriction,weights,catScaling) {
  vcol <- .preProcCat(vcolin,c())
  extraModelDegrees <- max(0,length(unique(vcolin))-1)
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
                    needsSplit=TRUE,
                    extraModelDegrees=extraModelDegrees)
  pred <- treatment$f(vcolin,treatment$args)
  if(!.has.range.cn(pred)) {
    return(NULL)
  }
  class(treatment) <- 'vtreatment'
  if((!catScaling)||(is.null(zC))) {
    treatment$scales <- linScore(newVarName,pred,zoY,weights)
  } else {
    treatment$scales <- catScore(newVarName,pred,zC,zTarget,weights)
  }
  treatment
}
