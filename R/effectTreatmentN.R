

# apply a numeric impact model
# replace level with .wmean(x|category) - .wmean(x)
.catNum <- function(col,args,doCollar) {
  col <- .preProcCat(col,args$levRestriction)
  unhandledNovel <- !(col %in% names(args$scores))
  keys <- col
  pred <- numeric(length(col))
  if(length(args$scores)>0) {
    keys[unhandledNovel] <- names(args$scores)[[1]]   # just to prevent bad lookups
    pred <- as.numeric(args$scores[keys]) 
  }
  # mean delta impact averaged over all possibilities, should be zero in scaled mode, mean dist in unscaled
  pred[unhandledNovel] <- 0.0 
  pred
}

# build a numeric impact model
# see: http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/
.mkCatNum <- function(origVarName,vcolin,rescol,smFactor,levRestriction,weights) {
  vcol <- .preProcCat(vcolin,levRestriction)
  extraModelDegrees <- max(0,length(unique(vcolin))-1)
  baseMean <- .wmean(rescol,weights)
  num <- tapply(rescol*weights,vcol,sum)
  den <- tapply(weights,vcol,sum)
  scores <- as.list((num+smFactor*baseMean)/(den+smFactor)-baseMean)
  scores <- scores[names(scores)!='zap'] # don't let zap code
  newVarName <- make.names(paste(origVarName,'catN',sep='_'))
  treatment <- list(origvar=origVarName,
                    newvars=newVarName,
                    f=.catNum,
                    args=list(scores=scores,
                              levRestriction=levRestriction),
                    treatmentName='Scalable Impact Code',
                    treatmentCode='catN',
                    needsSplit=TRUE,
                    extraModelDegrees=extraModelDegrees)
  pred <- treatment$f(vcolin,treatment$args)
  if(!.has.range.cn(pred)) {
    return(NULL)
  }
  class(treatment) <- 'vtreatment'
  treatment$scales <- linScore(newVarName,pred,rescol,weights)
  treatment
}
