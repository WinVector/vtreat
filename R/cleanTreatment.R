

# pass a variable through (removing NAs) (should only by used for numerics)
.passThrough <- function(col,args,doCollar) {
  treated <- as.numeric(col)
  treated[.is.bad(treated)] <- args$nadist
  if(doCollar) {
    treated[treated<args$cuts[[1]]] <- args$cuts[[1]]
    treated[treated>args$cuts[[2]]] <- args$cuts[[2]]
  }
  treated
}

as_rquery.vtreat_pass_through <- function(tstep, 
                                          ...) {
  if(!requireNamespace("rquery", quietly = TRUE)) {
    stop("vtreat::as_rquery.vtreat_pass_through treatmentplan requires the rquery package")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::as_rquery.vtreat_pass_through")
  args <- tstep$args
  list(
    exprs = tstep$newvars %:=% paste0("ifelse(is.na(", tstep$origvar, "), ", args$nadist, ", ", tstep$origvar, ")"),
    optree_generators = list(),
    tables = list()
  )
}


.mkPassThrough <- function(origVarName,xcol,ycol,zC,zTarget,weights,collarProb,catScaling) {
  xcol <- as.numeric(xcol)
  napositions <- .is.bad(xcol)
  nna <- sum(napositions)
  if(nna>=length(xcol)) {
    return(c())
  }
  if(collarProb>0.0) {
    cuts <- as.numeric(stats::quantile(xcol[!napositions],
                                       probs=c(collarProb,1-collarProb)))
  } else {
    cuts <- c(min(xcol[!napositions]),max(xcol[!napositions]))
  }
  nadist <- .wmean(xcol[!napositions],weights[!napositions])
  if(is.na(nadist)) {
    nadist <- 0
  }
  xcol[napositions] <- nadist
  if(max(xcol)<=min(xcol)) {
    return(c())
  }
  newVarName <- vtreat_make_names(paste(origVarName,'clean',sep='_'))
  treatment <- list(origvar=origVarName,
                    newvars=newVarName,
                    f=.passThrough,
                    args=list(nadist=nadist,cuts=cuts),
                    treatmentName='Scalable pass through',
                    treatmentCode='clean',
                    needsSplit=FALSE,
                    extraModelDegrees=0)
  class(treatment) <- c('vtreat_pass_through', 'vtreatment')
  if((!catScaling)||(is.null(zC))) {
    treatment$scales <- linScore(newVarName,xcol,ycol,weights)
  } else {
    treatment$scales <- catScore(newVarName,xcol,zC,zTarget,weights)
  }
  treatment
}
