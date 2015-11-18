

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

# build a classification impact model
# see: http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/
.mkCatBayes <- function(origVarName,vcolin,rescol,resTarget,smFactor,levRestriction,weights) {
  origColClass <- class(vcolin)
  vcol <- .preProcCat(vcolin,levRestriction)
  smFactor <- max(smFactor,1.0e-3)
  # T/F is true false of the quantity to be predicted
  # C is the feature we are looking at
  nT <- sum(as.numeric(rescol==resTarget)*weights)  # weighted sum of true examples
  nF <- sum(as.numeric(rescol!=resTarget)*weights)  # weighted sum of false examples
  probT <- nT/(nT+nF)   # unconditional probabilty target is true
  nCandT <- tapply(as.numeric(rescol==resTarget)*weights,vcol,sum)  # weighted sum of true examples for a given C (vector)
  nCandF <- tapply(as.numeric(rescol!=resTarget)*weights,vcol,sum)  # weighted sum of false examples for a give C (vector)
  pCgivenT <- (nCandT+probT*smFactor)/(nT+probT*smFactor)   # probability of a given evidence C, condition on outcome=T
  pCgivenF <- (nCandF+(1.0-probT)*smFactor)/(nF+(1.0-probT)*smFactor)  # probability of a given evidence C, condition on outcome=F
  pTgivenCunnorm <- pCgivenT*probT      # Bayes law, corret missing a /pC term (which we will normalize out)
  pFgivenCunnorm <- pCgivenF*(1-probT)  # Bayes law, corret missing a /pC term (which we will normalize out)
  pTgivenC <- pTgivenCunnorm/(pTgivenCunnorm+pFgivenCunnorm)
  logLift <- log(pTgivenC/probT)  # log probability ratio (so no effect is coded as zero)
  logLift <- as.list(logLift)
  logLift <- logLift[names(logLift)!='zap']  # don't let zap group code
  # fall back for novel levels, use zero impact
  treatment <- list(origvar=origVarName,origColClass=origColClass,
                    newvars=make.names(paste(origVarName,'catB',sep='_')),
                    f=.catBayes,
                    args=list(logLift=logLift,
                              levRestriction=levRestriction),
                    treatmentName='Bayesian Impact Code',
                    treatmentCode='catB',
                    needsSplit=TRUE)
  pred <- treatment$f(vcolin,treatment$args)
  class(treatment) <- 'vtreatment'
  treatment$scales <- .getScales(pred,as.numeric(rescol==resTarget),weights)
  treatment
}

