
# apply a deviation fact
# replace level with deviance (could add other summaries such as median)
.catD <- function(col,args,doCollar) {
  pred <- numeric(length(col))
  if(length(args$scorable)>0) {
    col <- .preProcCat(col,args$levRestriction)
    unhandledNovel <- !(col %in% args$scorable)
    keys <- col
    if(length(args$scores)>0) {
      keys[unhandledNovel] <- args$scorable[[1]]   # just to prevent bad lookups
      pred <- as.numeric(args$scores[keys]) 
    }
    pred[unhandledNovel] <- args$unhandledNovelCode # assume large deviation on unseen levels
  }
  pred
}

# build a deviation fact
# see: http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/
.mkCatD <- function(origVarName,vcolin,rescol,smFactor,levRestriction,weights) {
  vcol <- .preProcCat(vcolin,levRestriction)
  extraModelDegrees <- max(0,length(unique(vcolin))-1)
  num <- tapply(rescol*weights,vcol,sum)
  den <- tapply(weights,vcol,sum)
  scorable <- setdiff(names(den)[den>=2],'zap')
  if(length(scorable)<=0) {
    return(NULL)
  }
  condMean <- as.list(num/den)
  resids <- rescol-as.numeric(condMean[vcol])
  scores <- sqrt(tapply(resids*resids*weights,vcol,sum)/pmax(den-1,1))
  unhandledNovelCode <- 1.0
  if(length(scorable)>0) {
    unhandledNovelCode <- max(scores[scorable])
  }
  scores <- as.list(scores)
  scores <- scores[names(scores)!='zap'] # don't let zap code
  newVarName <- make.names(paste(origVarName,'catD',sep='_'))
  treatment <- list(origvar=origVarName,
                    newvars=newVarName,
                    f=.catD,
                    args=list(scores=scores,
                              scorable=scorable,
                              unhandledNovelCode=unhandledNovelCode,
                              levRestriction=levRestriction),
                    treatmentName='Deviation Fact',
                    treatmentCode='catD',
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



