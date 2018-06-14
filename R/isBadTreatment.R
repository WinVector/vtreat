
# return if a variable is NA
.isBAD <- function(col,args,doCollar) {
  treated <- ifelse(.is.bad(col),1.0,0.0)
  treated
}

as_rquery.vtreat_is_bad <- function(tstep, 
                                          ...) {
  if(!requireNamespace("rquery", quietly = TRUE)) {
    stop("vtreat::as_rquery.vtreat_is_bad treatmentplan requires the rquery package")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::as_rquery.vtreat_is_bad")
  args <- tstep$args
  list(
    optree_generators = list(
      function(d) {
        rquery::extend_se(d, 
                          tstep$newvars %:=% paste0("ifelse(is.na(", tstep$origvar, "), ", 1, ", ", 0, ")"))
      }),
    tables = list()
  )
}

.mkIsBAD <- function(origVarName,xcol,ynumeric,zC,zTarget,weights,catScaling) {
  badIDX <- .is.bad(xcol)
  nna <- sum(badIDX)
  if((nna<=0)||(nna>=length(xcol))) {
    return(c())
  }
  newVarName <- make.names(paste(origVarName,'isBAD',sep='_'))
  treatment <- list(origvar=origVarName,
                    newvars=newVarName,
                    f=.isBAD,
                    args=list(),
                    treatmentName='is.bad',
                    treatmentCode='isBAD',
                    needsSplit=FALSE,
                    extraModelDegrees=0)
  class(treatment) <- c('vtreat_is_bad', 'vtreatment')
  if((!catScaling)||(is.null(zC))) {
    treatment$scales <- linScore(newVarName,ifelse(badIDX,1.0,0.0),ynumeric,weights)
  } else {
    treatment$scales <- catScore(newVarName,ifelse(badIDX,1.0,0.0),zC,zTarget,weights)
  }
  treatment
}
