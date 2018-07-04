

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
  pred[unhandledNovel] <- args$rare_score
  pred
}


as_rquery.vtreat_cat_p <- function(tstep, 
                                     ...,
                                     var_restriction) {
  if(!requireNamespace("rquery", quietly = TRUE)) {
    stop("vtreat::as_rquery.vtreat_cat_p treatmentplan requires the rquery package")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::as_rquery.vtreat_cat_p")
  if((!is.null(var_restriction)) && (!(tstep$newvars %in% var_restriction))) {
    return(NULL)
  }
  args <- tstep$args
  rquery_code_categorical(colname = tstep$origvar, 
                          resname = tstep$newvars,
                          coding_levels = names(args$scores),
                          effect_values = args$scores,
                          levRestriction = args$levRestriction,
                          default_value = args$rare_score)
}



# build a prevalence fact
.mkCatP <- function(origVarName,vcolin,zoY,zC,zTarget,levRestriction,weights,catScaling) {
  vcol <- .preProcCat(vcolin,c())
  extraModelDegrees <- max(0,length(unique(vcolin))-1)
  num <- tapply(weights,vcol,sum)
  den <- sum(weights)
  scores <- num/den
  scores <- as.list(scores)
  newVarName <- vtreat_make_names(paste(origVarName,'catP',sep='_'))
  treatment <- list(origvar=origVarName,
                    newvars=newVarName,
                    f=.catP,
                    args=list(scores=scores,
                              levRestriction=levRestriction,
                              rare_score = 0.5/den),
                    treatmentName='Prevalence Code',
                    treatmentCode='catP',
                    needsSplit=TRUE,
                    extraModelDegrees=extraModelDegrees)
  pred <- treatment$f(vcolin,treatment$args)
  if(!.has.range.cn(pred)) {
    return(NULL)
  }
  class(treatment) <- c('vtreat_cat_p','vtreatment')
  if((!catScaling)||(is.null(zC))) {
    treatment$scales <- linScore(newVarName,pred,zoY,weights)
  } else {
    treatment$scales <- catScore(newVarName,pred,zC,zTarget,weights)
  }
  treatment
}
