
# @param v character variable name
# @param vcol character variable levels
# @param zoY numeric outcome as numeric
# @param zC NULL if regression, target outcomes if classification
# @param zTarget postitive character target if classification
# @param weights row/example weights
exampleCoder <- function(v,vcol,zoY,zC,zTarget,weights) {
  if(is.null(zC)) {
    d <- data.frame(x = vcol,
                    y = zoY,
                    stringsAsFactors = FALSE)
    m <- lm(y~x, data=d, weights=weights)
    p <- stats::predict(m, newdata=d)
    return(p)
  } else {
    d <- data.frame(x = vcol,
                    y = zC==zTarget,
                    stringsAsFactors = FALSE)
    m <- glm(y~x, data=d, weights = weights, family=binomial)
    p <- stats::predict(m, newdata=d, type='link')
    return(p)
  }
}

# apply a classification impact model
# replace level with stored code
.customCode <- function(col,args) {
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

# @para customCode code name
# @param coder user supplied variable re-coder
# @param v variable name
# @param vcolin data column, character
# @param zoY outcome column as numeric
# @param zC if classification outcome column as character
# @param zTarge if classification target class
# @param weights per-row weights
makeCustomCoder <- function(customCode,coder, 
                            v,vcolin,zoY,zC,zTarget,weights,catScaling)  {
  levRestriction <- NULL
  vcol <- .preProcCat(vcolin,levRestriction)
  if(is.null(weights)) {
    weights <- rep(1.0, length(vcol))
  }
  extraModelDegrees <- max(0,length(unique(vcolin))-1)
  scores <- NULL
  tryCatch(
    scores <- coder(v,vcol,zoY,zC,zTarget,weights),
    error = function(e) { warning(e) }
  )
  if(is.null(scores) || (!is.numeric(scores)) || (length(scores)!=length(vcol))) {
    scores <- rep(0.0, length(vcol))
  } else {
    # shift scores to be mean zero with respect to weights
    scores <- scores -  sum(scores*weights)/sum(weights)
  }
  d <- data.frame(x = vcol,
                  pred = scores)
  agg <- aggregate(pred~x, data=d, mean)
  conditionalScore <- as.list(as.numeric(agg$pred))
  names(conditionalScore) <- as.character(agg$x)
  conditionalScore <- conditionalScore[names(conditionalScore)!='zap']  # don't let zap group code
  newVarName <- make.names(paste(v, customCode, sep='_'))
  treatment <- list(origvar=v,
                    newvars=newVarName,
                    f=.customCode,
                    args=list(conditionalScore=conditionalScore,
                              levRestriction=levRestriction),
                    treatmentName=paste('Custom Code:', customCode),
                    treatmentCode=customCode,
                    needsSplit=TRUE,
                    extraModelDegrees=extraModelDegrees)
  pred <- treatment$f(vcolin,treatment$args)
  if(!.has.range.cn(pred)) {
    return(NULL)
  }
  class(treatment) <- 'vtreatment'
  if(!catScaling) {
    treatment$scales <- linScore(newVarName,pred,as.numeric(zC==zTarget),weights)
  } else {
    treatment$scales <- catScore(newVarName,pred,zC,zTarget,weights)
  }
  treatment
}