

# apply a classification impact model
# replace level with logit(P[y==target|level]) - logit(P[y==target])
.catBayes <- function(col,args,doCollar) {
  col <- .preProcCat(col,args$levRestriction)
  unhandledNovel <- !(col %in% names(args$conditionalScore))
  keys <- col
  pred <- numeric(length(col))
  if(length(args$conditionalScore)>0) {
    keys[unhandledNovel] <- names(args$conditionalScore)[[1]]  # just to prevent bad lookups
    pred <- as.numeric(args$conditionalScore[keys]) 
  }
  pred[unhandledNovel] <- 0.0
  pred
}


#' @export
as_rquery.vtreat_cat_Bayes <- function(tstep, 
                                       ...,
                                       var_restriction) {
  if(!requireNamespace("rquery", quietly = TRUE)) {
    stop("vtreat::as_rquery.vtreat_cat_Bayes treatmentplan requires the rquery package")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::as_rquery.vtreat_cat_Bayes")
  if((!is.null(var_restriction)) && (!(tstep$newvars %in% var_restriction))) {
    return(NULL)
  }
  args <- tstep$args
  rquery_code_categorical(colname = tstep$origvar, 
                          resname = tstep$newvars,
                          coding_levels = names(args$conditionalScore),
                          effect_values = args$conditionalScore,
                          levRestriction = args$levRestriction,
                          default_value = 0.0)
}


# build a classification impact model
# see: https://win-vector.com/2012/07/23/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/
.mkCatBayes <- function(origVarName,vcolin,rescol,resTarget,smFactor,levRestriction,weights,catScaling) {
  vcol <- .preProcCat(vcolin,levRestriction)
  extraModelDegrees <- max(0,length(unique(vcolin))-1)
  epsilon <- 1.0e-6
  smFactor <- max(smFactor,1.0e-4)
  # T/F is true false of the quantity to be predicted
  # C is the feature we are looking at
  nT <- sum(as.numeric(rescol==resTarget)*weights)  # weighted sum of true examples
  nF <- sum(as.numeric(rescol!=resTarget)*weights)  # weighted sum of false examples
  nCandT <- tapply(as.numeric(rescol==resTarget)*weights,vcol,sum)  # weighted sum of true examples for a given C (vector)
  nCandF <- tapply(as.numeric(rescol!=resTarget)*weights,vcol,sum)  # weighted sum of false examples for a give C (vector)
  probT <- pmax(epsilon,pmin(1-epsilon,nT/(nT+nF)))   # unconditional probabilty target is true
  pCgivenT <- (nCandT+smFactor)/(nT+smFactor) # probability of a given evidence C, condition on outcome=T
  pCgivenF <- (nCandF+smFactor)/(nF+smFactor) # probability of a given evidence C, condition on outcome=F
  pTgivenCunnorm <- pCgivenT*probT      # Bayes law, corret missing a /pC term (which we will normalize out)
  pFgivenCunnorm <- pCgivenF*(1-probT)  # Bayes law, corret missing a /pC term (which we will normalize out)
  pTgivenC <- pTgivenCunnorm/(pTgivenCunnorm+pFgivenCunnorm)
  # conditionalScore <- log(pTgivenC/probT)  # log probability ratio (so no effect is coded as zero)
  conditionalScore <- .logit(pTgivenC) - .logit(probT)  # logit probabilty
  conditionalScore <- as.list(conditionalScore)
  conditionalScore <- conditionalScore[names(conditionalScore)!='zap']  # don't let zap group code
  # fall back for novel levels, use zero impact
  newVarName <- vtreat_make_names(paste(origVarName,'catB',sep='_'))
  treatment <- list(origvar=origVarName,
                    newvars=newVarName,
                    f=.catBayes,
                    args=list(conditionalScore=conditionalScore,
                              levRestriction=levRestriction),
                    treatmentName='Bayesian Impact Code',
                    treatmentCode='catB',
                    needsSplit=TRUE,
                    extraModelDegrees=extraModelDegrees)
  pred <- treatment$f(vcolin,treatment$args)
  if(!.has.range.cn(pred)) {
    return(NULL)
  }
  class(treatment) <- c('vtreat_cat_Bayes', 'vtreatment')
  if(!catScaling) {
    treatment$scales <- linScore(newVarName,pred,as.numeric(rescol==resTarget),weights)
  } else {
    treatment$scales <- catScore(newVarName,pred,rescol,resTarget,weights)
  }
  if(treatment$scales$a <= 0) {
    return(NULL) # fitting a noise effect
  }
  treatment
}
