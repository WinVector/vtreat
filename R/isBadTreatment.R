
# return if a variable is NA
.isBAD <- function(col,args,doCollar) {
  treated <- ifelse(.is.bad(col),1.0,0.0)
  treated
}

.mkIsBAD <- function(origVarName,xcol,ynumeric,weights) {
  origColClass <- class(xcol)
  badIDX <- .is.bad(xcol)
  nna <- sum(badIDX)
  if((nna<=0)||(nna>=length(xcol))) {
    return(c())
  }
  treatment <- list(origvar=origVarName,origColClass=origColClass,
                    newvars=make.names(paste(origVarName,'isBAD',sep='_')),
                    f=.isBAD,
                    args=list(),
                    treatmentName='is.bad',
                    treatmentCode='isBAD')
  class(treatment) <- 'vtreatment'
  treatment$scales <- .getScales(ifelse(badIDX,1.0,0.0),ynumeric,weights)
  treatment
}
