

# apply a classification impact model
# replace level with log(.wmean(x|category)/.wmean(x))
.catBayes <- function(col,args,doCollar) {
  col <- .preProcCat(col,args$levRestriction)
  novel <- !(col %in% names(args$logLift))
  keys <- col
  pred <- numeric(length(col))
  if(length(args$logLift)>0) {
    keys[novel] <- names(args$logLift)[[1]]  # just to prevent bad lookups
    pred <- as.numeric(args$logLift[keys]) 
  }
  pred[novel] <- 0.0
  pred
}

.logit <- function(x) {
  log(x/(1-x))
}

# build a classification impact model
# see: http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/
.mkCatBayes <- function(origVarName,vcolin,rescol,resTarget,smFactor,levRestriction,weights,catScaling) {
  vcol <- .preProcCat(vcolin,levRestriction)
  smFactor <- max(smFactor,1.0e-3)
  # T/F is true false of the quantity to be predicted
  # C is the feature we are looking at
  nT <- sum(as.numeric(rescol==resTarget)*weights)  # weighted sum of true examples
  nF <- sum(as.numeric(rescol!=resTarget)*weights)  # weighted sum of false examples
  nCandT <- tapply(as.numeric(rescol==resTarget)*weights,vcol,sum)  # weighted sum of true examples for a given C (vector)
  nCandF <- tapply(as.numeric(rescol!=resTarget)*weights,vcol,sum)  # weighted sum of false examples for a give C (vector)
  probT <- nT/(nT+nF)   # unconditional probabilty target is true
  pCgivenT <- (nCandT+probT*smFactor)/(nT+probT*smFactor)   # probability of a given evidence C, condition on outcome=T
  pCgivenF <- (nCandF+(1.0-probT)*smFactor)/(nF+(1.0-probT)*smFactor)  # probability of a given evidence C, condition on outcome=F
  pTgivenCunnorm <- pCgivenT*probT      # Bayes law, corret missing a /pC term (which we will normalize out)
  pFgivenCunnorm <- pCgivenF*(1-probT)  # Bayes law, corret missing a /pC term (which we will normalize out)
  pTgivenC <- pTgivenCunnorm/(pTgivenCunnorm+pFgivenCunnorm)
  # logLift <- log(pTgivenC/probT)  # log probability ratio (so no effect is coded as zero)
  logLift <- .logit(pTgivenC) - .logit(probT)  # logit probabilty
  logLift <- as.list(logLift)
  logLift <- logLift[names(logLift)!='zap']  # don't let zap group code
  # fall back for novel levels, use zero impact
  newVarName <- make.names(paste(origVarName,'catB',sep='_'))
  treatment <- list(origvar=origVarName,
                    newvars=newVarName,
                    f=.catBayes,
                    args=list(logLift=logLift,
                              levRestriction=levRestriction),
                    treatmentName='Bayesian Impact Code',
                    treatmentCode='catB',
                    needsSplit=TRUE,
                    extraModelDegrees=max(0,length(logLift)-1))
  pred <- treatment$f(vcolin,treatment$args)
  if(!.has.range.cn(pred)) {
    return(NULL)
  }
  class(treatment) <- 'vtreatment'
  if(!catScaling) {
    treatment$scales <- linScore(newVarName,pred,as.numeric(rescol==resTarget),weights)
  } else {
    treatment$scales <- catScore(newVarName,pred,rescol,resTarget,weights)
  }
  treatment
}
