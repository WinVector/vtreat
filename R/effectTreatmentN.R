

# apply a numeric impact model
# replace level with .wmean(x|category) - .wmean(x)
.catNum <- function(col,args,doCollar) {
  col <- .preProcCat(col,args$levRestriction)
  novel <- !(col %in% names(args$scores))
  keys <- col
  pred <- numeric(length(col))
  if(length(args$scores)>0) {
    keys[novel] <- names(args$scores)[[1]]   # just to prevent bad lookups
    pred <- as.numeric(args$scores[keys]) 
  }
  # mean delta impact averaged over all possibilities, should be zero in scaled mode, mean dist in unscaled
  pred[novel] <- 0.0 
  pred
}

# build a numeric impact model
# see: http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/
.mkCatNum <- function(origVarName,vcolin,rescol,smFactor,levRestriction,weights) {
  origColClass <- class(vcolin)
  vcol <- .preProcCat(vcolin,levRestriction)
  baseMean <- .wmean(rescol,weights)
  num <- tapply(rescol*weights,vcol,sum)
  den <- tapply(weights,vcol,sum)
  scores <- as.list((num+smFactor*baseMean)/(den+smFactor)-baseMean)
  scores <- scores[names(scores)!='zap'] # don't let zap code
  treatment <- list(origvar=origVarName,origColClass=origColClass,
                    newvars=make.names(paste(origVarName,'catN',sep='_')),
                    f=.catNum,
                    args=list(scores=scores,
                              levRestriction=levRestriction),
                    treatmentName='Scalable Impact Code',
                    treatmentCode='catN',
                    needsSplit=TRUE)
  pred <- treatment$f(vcolin,treatment$args)
  class(treatment) <- 'vtreatment'
  treatment$scales <- .getScales(pred,rescol,weights)
  treatment
}
