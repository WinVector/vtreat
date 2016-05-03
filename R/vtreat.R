# variable treatments type def: list { origvar, newvars, f(col,args), args, treatmentName, scales } can share orig var

# Would be nice to have: Jackknifed GLM significance

#' @importFrom stats aggregate anova as.formula binomial fisher.test glm lm lm.wfit pchisq pf quantile
#' @importFrom utils packageVersion
NULL




#'
#' Original variable name from a treatmentplan$treatment item.
#' @param x vtreatment item.
#' @seealso \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}} 
#' @export
#' 
vorig <- function(x) { x$origvar }


#'
#' New treated variable names from a treatmentplan$treatment item.
#' @param x vtreatment item
#' @seealso \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}} 
#' @export
vnames <- function(x) { x$newvars }

#'
#' Display treatment plan.
#' @param x treatment plan
#' @param ... additional args (to match general signature).
#' @export
format.vtreatment <- function(x,...) { paste(
  'vtreat \'',x$treatmentName,
  '\'(\'',x$origvar,'\'(',x$origType,',',x$origClass,')->',
  x$convertedColClass,'->\'',
  paste(x$newvars,collapse='\',\''),
  '\')',sep='') }

#'
#' Print treatmentplan.
#' @param x treatmentplan
#' @param ... additional args (to match general signature).
#' @seealso \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}} \code{\link{prepare}}
#' @export
print.vtreatment <- function(x,...) { 
  print(format.vtreatment(x),...) 
}









#' Build all treatments for a data frame to predict a categorical outcome.
#' 
#' Function to design variable treatments for binary prediction of a
#' categorical outcome.  Data frame is assumed to have only atomic columns
#' except for dates (which are converted to numeric).
#' 
#' The main fields are mostly vectors with names (all with the same names in the same order):
#' 
#' - vars : (character array without names) names of variables (in same order as names on the other diagnostic vectors)
#' - varMoves : logical TRUE if the variable varied during hold out scoring, only variables that move will be in the treated frame
#' - #' - sig : an estimate significance of effect
#'
#' See the vtreat vignette for a bit more detail and a worked example.
#'
#' @param dframe Data frame to learn treatments from (training data), must have at least 1 row.
#' @param varlist Names of columns to treat (effective variables).
#' @param outcomename Name of column holding outcome variable. dframe[[outcomename]] must be only finite non-missing values.
#' @param outcometarget Value/level of outcome to be considered "success",  and there must be a cut such that dframe[[outcomename]]==outcometarget at least twice and dframe[[outcomename]]!=outcometarget at least twice.
#' @param ... no additional arguments, declared to forced named binding of later arguments
#' @param weights optional training weights for each row
#' @param minFraction optional minimum frequency a categorical level must have to be converted to an indicator column.
#' @param smFactor optional smoothing factor for impact coding models.
#' @param rareCount optional integer, suppress direct effects of level of this count or less.
#' @param rareSig optional numeric, suppress direct effects of level of this significance value greater.  Set to one to turn off effect.
#' @param collarProb what fraction of the data (pseudo-probability) to collar data at (<0.5).
#' @param ncross optional scalar >=2 number of cross validation splits use in rescoring complex variables.
#' @param verbose if TRUE print progress.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow
#' @return treatment plan (for use with prepare)
#' @seealso \code{\link{prepare}} \code{\link{designTreatmentsN}} 
#' @examples
#' 
#' dTrainC <- data.frame(x=c('a','a','a','b','b','b'),
#'    z=c(1,2,3,4,5,6),
#'    y=c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))
#' dTestC <- data.frame(x=c('a','b','c',NA),
#'    z=c(10,20,30,NA))
#' treatmentsC <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE)
#' dTrainCTreated <- prepare(treatmentsC,dTrainC,pruneSig=0.99)
#' dTestCTreated <- prepare(treatmentsC,dTestC,pruneSig=0.99)
#' 
#' @export
designTreatmentsC <- function(dframe,varlist,outcomename,outcometarget,
                              ...,
                              weights=c(),
                              minFraction=0.02,smFactor=0.0,
                              rareCount=0,rareSig=1,
                              collarProb=0.00,
                              ncross=3,
                              verbose=TRUE,
                              parallelCluster=NULL) {
  
  .checkArgs(dframe=dframe,varlist=varlist,outcomename=outcomename,...)
  if(!(outcomename %in% colnames(dframe))) {
    stop("outcomename must be a column name of dframe")
  }
  zoY <- ifelse(dframe[[outcomename]]==outcometarget,1.0,0.0)
  if(min(zoY)>=max(zoY)) {
    stop("dframe[[outcomename]]==outcometarget must vary")
  }
  treatments <- .designTreatmentsX(dframe,varlist,outcomename,zoY,
                                   dframe[[outcomename]],outcometarget,
                                   weights,
                                   minFraction,smFactor,
                                   rareCount,rareSig,
                                   collarProb,
                                   ncross,
                                   verbose,
                                   parallelCluster)
  treatments$outcomeTarget <- outcometarget
  treatments$outcomeType <- 'Binary'
  treatments$vtreatVersion <- packageVersion('vtreat')
  treatments
}




#' build all treatments for a data frame to predict a numeric outcome
#' 
#' Function to design variable treatments for binary prediction of a
#' numeric outcome.  Data frame is assumed to have only atomic columns
#' except for dates (which are converted to numeric).
#' Note: each column is processed independently of all others.
#' 
#' The main fields are mostly vectors with names (all with the same names in the same order):
#' 
#' - vars : (character array without names) names of variables (in same order as names on the other diagnostic vectors)
#' - varMoves : logical TRUE if the variable varied during hold out scoring, only variables that move will be in the treated frame
#' - sig : an estimate significance of effect
#'
#' See the vtreat vignette for a bit more detail and a worked example.
#' 
#' @param dframe Data frame to learn treatments from (training data), must have at least 1 row.
#' @param varlist Names of columns to treat (effective variables).
#' @param outcomename Name of column holding outcome variable. dframe[[outcomename]] must be only finite non-missing values and there must be a cut such that dframe[[outcomename]] is both above the cut at least twice and below the cut at least twice.
#' @param ... no additional arguments, declared to forced named binding of later arguments
#' @param weights optional training weights for each row
#' @param minFraction optional minimum frequency a categorical level must have to be converted to an indicator column.
#' @param smFactor optional smoothing factor for impact coding models.
#' @param rareCount optional integer, suppress direct effects of level of this count or less.
#' @param rareSig optional numeric, suppress direct effects of level of this significance value greater.  Set to one to turn off effect.
#' @param collarProb what fraction of the data (pseudo-probability) to collar data at (<0.5).
#' @param ncross optional scalar >=2 number of cross validation splits use in rescoring complex variables.
#' @param verbose if TRUE print progress.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow
#' @return treatment plan (for use with prepare)
#' @seealso \code{\link{prepare}} \code{\link{designTreatmentsC}} 
#' @examples
#' 
#' dTrainN <- data.frame(x=c('a','a','a','a','b','b','b'),
#'     z=c(1,2,3,4,5,6,7),y=c(0,0,0,1,0,1,1))
#' dTestN <- data.frame(x=c('a','b','c',NA),
#'     z=c(10,20,30,NA))
#' treatmentsN = designTreatmentsN(dTrainN,colnames(dTrainN),'y')
#' dTrainNTreated <- prepare(treatmentsN,dTrainN,pruneSig=0.99)
#' dTestNTreated <- prepare(treatmentsN,dTestN,pruneSig=0.99)
#' 
#' @export
designTreatmentsN <- function(dframe,varlist,outcomename,
                              ...,
                              weights=c(),
                              minFraction=0.02,smFactor=0.0,
                              rareCount=0,rareSig=1,
                              collarProb=0.00,
                              ncross=3,
                              verbose=TRUE,
                              parallelCluster=NULL) {
  .checkArgs(dframe=dframe,varlist=varlist,outcomename=outcomename,...)
  if(!(outcomename %in% colnames(dframe))) {
    stop("outcomename must be a column name of dframe")
  }
  ycol <- dframe[[outcomename]]
  if(min(ycol)>=max(ycol)) {
    stop("dframe[[outcomename]] must vary")
  }
  treatments <- .designTreatmentsX(dframe,varlist,outcomename,ycol,
                     c(),c(),
                     weights,
                     minFraction,smFactor,
                     rareCount,rareSig,
                     collarProb,
                     ncross,
                     verbose,
                     parallelCluster)
  treatments$outcomeType <- 'Numeric'
  treatments$vtreatVersion <- packageVersion('vtreat')
  treatments
}




#' Design variable treatments with no outcome variable.
#' 
#' Data frame is assumed to have only atomic columns
#' except for dates (which are converted to numeric).
#' Note: each column is processed independently of all others.
#' 
#' The main fields are mostly vectors with names (all with the same names in the same order):
#' 
#' - vars : (character array without names) names of variables (in same order as names on the other diagnostic vectors)
#' - varMoves : logical TRUE if the variable varied during hold out scoring, only variables that move will be in the treated frame
#'
#' See the vtreat vignette for a bit more detail and a worked example.
#' 
#' @param dframe Data frame to learn treatments from (training data), must have at least 1 row.
#' @param varlist Names of columns to treat (effective variables).
#' @param ... no additional arguments, declared to forced named binding of later arguments
#' @param weights optional training weights for each row
#' @param rareCount optional integer, suppress direct effects of level of this count or less.
#' @param collarProb what fraction of the data (pseudo-probability) to collar data at (<0.5).
#' @param verbose if TRUE print progress.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow
#' @return treatment plan (for use with prepare)
#' @seealso \code{\link{prepare}} \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}} 
#' @examples
#' 
#' dTrainZ <- data.frame(x=c('a','a','a','a','b','b',NA,'e','e'),
#'     z=c(1,2,3,4,5,6,7,NA,9))
#' dTestZ <- data.frame(x=c('a','x','c',NA),
#'     z=c(10,20,30,NA))
#' treatmentsZ = designTreatmentsZ(dTrainZ,colnames(dTrainZ),
#'   rareCount=0)
#' dTrainZTreated <- prepare(treatmentsZ,dTrainZ,pruneSig=1)
#' dTestZTreated <- prepare(treatmentsZ,dTestZ,pruneSig=1)
#' 
#' @export
designTreatmentsZ <- function(dframe,varlist,
                              ...,
                              weights=c(),
                              rareCount=0,
                              collarProb=0.00,
                              verbose=TRUE,
                              parallelCluster=NULL) {
  outcomename='ZZZZNonCol'
  dframe[[outcomename]] <- 0
  .checkArgs(dframe=dframe,varlist=varlist,outcomename=outcomename,...)
  ycol <- dframe[[outcomename]]
  treatments <- .designTreatmentsX(dframe,varlist,outcomename,ycol,
                     c(),c(),
                     weights,
                     1.0,smFactor=0,
                     rareCount,rareSig=1,
                     collarProb,
                     3,
                     verbose,
                     parallelCluster)
  treatments$outcomeType <- 'None'
  treatments$vtreatVersion <- packageVersion('vtreat')
  treatments
}




#' Apply treatments and restrict to useful variables.
#' 
#' Use a treatment plan to prepare a data frame for analysis.  The
#' resulting frame will have new effective variables that are numeric
#' and free of NaN/NA.  If the outcome column is present it will be copied over.
#' The intent is that these frames are compatible with more machine learning
#' techniques, and avoid a lot of corner cases (NA,NaN, novel levels, too many levels).
#' Note: each column is processed independently of all others.  Also copies over outcome if present.
#' 
#' @param treatmentplan Plan built by designTreantmentsC() or designTreatmentsN()
#' @param dframe Data frame to be treated
#' @param pruneSig suppress variables with significance above this level
#' @param ... no additional arguments, declared to forced named binding of later arguments
#' @param scale optional if TRUE replace numeric variables with single variable model regressions ("move to outcome-scale").  These have mean zero and (for varaibles with signficant less than 1) slope 1 when regressed against outcome.
#' @param doCollar optional if TRUE collar numeric variables by cutting off after a tail-probability specified by collarProb during treatment design.
#' @param varRestriction optional list of treated variable names to restrict to
#' @return treated data frame (all columns numeric, without NA,NaN)
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow
#' @seealso \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}}
#' @examples
#' 
#' dTrainN <- data.frame(x=c('a','a','a','a','b','b','b'),
#'     z=c(1,2,3,4,5,6,7),y=c(0,0,0,1,0,1,1))
#' dTestN <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))
#' treatmentsN = designTreatmentsN(dTrainN,colnames(dTrainN),'y')
#' dTrainNTreated <- prepare(treatmentsN,dTrainN,1.0)
#' dTestNTreated <- prepare(treatmentsN,dTestN,1.0)
#' 
#' dTrainC <- data.frame(x=c('a','a','a','b','b','b'),
#'     z=c(1,2,3,4,5,6),y=c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))
#' dTestC <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))
#' treatmentsC <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE)
#' dTrainCTreated <- prepare(treatmentsC,dTrainC,1.0)
#' dTestCTreated <- prepare(treatmentsC,dTestC,1.0)
#' 
#' 
#' @export
prepare <- function(treatmentplan,dframe,pruneSig,
                    ...,
                    scale=FALSE,doCollar=TRUE,
                    varRestriction=c(),
                    parallelCluster=NULL) {
  .checkArgs1(dframe=dframe,...)
  if(class(treatmentplan)!='treatmentplan') {
    stop("treatmentplan must be of class treatmentplan")
  }
  vtreatVersion <- packageVersion('vtreat')
  if(is.null(treatmentplan$vtreatVersion) ||
     (treatmentplan$vtreatVersion!=vtreatVersion)) {
    warning(paste('treatments desined with vtreat version',
               treatmentplan$vtreatVersion,
               'and preparing data.frame with vtreat version',
               vtreatVersion))
  }
  if(!is.data.frame(dframe)) {
    stop("dframe must be a data frame")
  }
  if(nrow(dframe)<=0) {
    stop("no rows")
  }
  if(treatmentplan$outcomeType=='None') {
    pruneSig <- NULL
  }
  usable <- treatmentplan$scoreFrame$varMoves
  if(!is.null(pruneSig)) {
    usable <- usable & (treatmentplan$scoreFrame$sig<=pruneSig)
  }
  usableVars <- treatmentplan$scoreFrame$varName[usable]
  if(!is.null(varRestriction)) {
     usableVars <- intersect(usableVars,varRestriction)
  }
  if(length(usableVars)<=0) {
    stop('no usable vars')
  }
  for(ti in treatmentplan$treatments) {
    if(length(intersect(ti$newvars,usableVars))>0) {
      newType <- typeof(dframe[[ti$origvar]])
      newClass <- class(dframe[[ti$origvar]])
      if((ti$origType!=newType) || (ti$origClass!=newClass)) {
        warning(paste('variable',ti$origvar,'expected type/class',
                   ti$origType,ti$origClass,
                   'saw ',newType,newClass))
                   
      }
    }
  }
  treated <- .vtreatList(treatmentplan$treatments,dframe,usableVars,scale,doCollar,
                         parallelCluster)
  # copy outcome over if it is present
  if(treatmentplan$outcomename %in% colnames(dframe)) {
    treated[[treatmentplan$outcomename]] <- dframe[[treatmentplan$outcomename]]
  }
  treated
}



#' Run categorical cross-frame experiment.
#' 
#' Builds a \code{\link{designTreatmentsC}} treatment plan and a data frame prepared 
#' from \code{dframe} that is "cross" in the sense each row is treated using a treatment
#' plan built from a subset of dframe disjoint from the given row.
#' The goal is to try to and supply a method of breaking nested model bias other than splitting
#' into calibration, training, test sets.
#' 
#'
#' @param dframe Data frame to learn treatments from (training data), must have at least 1 row.
#' @param varlist Names of columns to treat (effective variables).
#' @param outcomename Name of column holding outcome variable. dframe[[outcomename]] must be only finite non-missing values.
#' @param outcometarget Value/level of outcome to be considered "success",  and there must be a cut such that dframe[[outcomename]]==outcometarget at least twice and dframe[[outcomename]]!=outcometarget at least twice.
#' @param ... no additional arguments, declared to forced named binding of later arguments
#' @param weights optional training weights for each row
#' @param minFraction optional minimum frequency a categorical level must have to be converted to an indicator column.
#' @param smFactor optional smoothing factor for impact coding models.
#' @param rareCount optional integer, suppress direct effects of level of this count or less.
#' @param rareSig optional numeric, suppress direct effects of level of this significance value greater.  Set to one to turn off effect.
#' @param collarProb what fraction of the data (pseudo-probability) to collar data at (<0.5).
#' @param scale optional if TRUE replace numeric variables with regression ("move to outcome-scale").
#' @param doCollar optional if TRUE collar numeric variables by cutting off after a tail-probability specified by collarProb during treatment design.
#' @param ncross optional scalar>=2 number of cross-validation rounds to design.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow
#' @seealso \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}} \code{\link{prepare}}
#' @return list with treatments and crossFrame
#' @examples
#' 
#' set.seed(23525)
#' zip <- paste('z',1:100)
#' N = 200
#' d <- data.frame(zip=sample(zip,N,replace=TRUE),
#'                 zip2=sample(zip,20,replace=TRUE),
#'                 y=runif(N))
#' del <- runif(length(zip))
#' names(del) <- zip
#' d$y <- d$y + del[d$zip2]
#' d$yc <- d$y>=mean(d$y)
#' cC <- mkCrossFrameCExperiment(d,c('zip','zip2'),'yc',TRUE,
#'   rareCount=2,rareSig=0.9)
#' cor(as.numeric(cC$crossFrame$yc),cC$crossFrame$zip_catB)  # poor
#' cor(as.numeric(cC$crossFrame$yc),cC$crossFrame$zip2_catB) # better
#' treatments <- cC$treatments
#' dTrainV <- cC$crossFrame
#' 
#' @export
mkCrossFrameCExperiment <- function(dframe,varlist,
                                    outcomename,outcometarget,
                                    ...,
                                    weights=c(),
                                    minFraction=0.02,smFactor=0.0,
                                    rareCount=0,rareSig=1,
                                    collarProb=0.00,
                                    scale=FALSE,doCollar=TRUE,
                                    ncross=3,
                                    parallelCluster=NULL) {
  .checkArgs(dframe=dframe,varlist=varlist,outcomename=outcomename,...)
  if(!is.data.frame(dframe)) {
    stop("dframe must be a data frame")
  }
  if(collarProb>=0.5) {
    stop("collarProb must be < 0.5")
  }
  if(nrow(dframe)<1) {
    stop("most have rows")
  }
  if(!(outcomename %in% colnames(dframe))) {
    stop("outcomename must be a column name of dframe")
  }
  if(is.null(weights)) {
    weights <- rep(1.0,nrow(dframe))
  }
  treatments <- designTreatmentsC(dframe,varlist,outcomename,outcometarget,
                                  weights=weights,
                                  minFraction=minFraction,smFactor=smFactor,
                                  rareCount=rareCount,rareSig=rareSig,
                                  collarProb=collarProb,
                                  verbose=FALSE,
                                  parallelCluster=parallelCluster)
  zC <- dframe[[outcomename]]
  zoY <- ifelse(zC==outcometarget,1,0)
  newVarsS <- treatments$scoreFrame$varName[treatments$scoreFrame$varMoves]
  crossDat <- .mkCrossFrame(dframe,varlist,newVarsS,outcomename,zoY,
                            zC,outcometarget,
                            weights,
                            minFraction,smFactor,
                            rareCount,rareSig,
                            collarProb,
                            FALSE,
                            scale,doCollar,
                            ncross,
                            parallelCluster)
  list(treatments=treatments,crossFrame=crossDat$crossFrame)
}


#' Run numeric cross frame experiment.
#' 
#' Builds a \code{\link{designTreatmentsC}} treatment plan and a data frame prepared 
#' from \code{dframe} that is "cross" in the sense each row is treated using a treatment
#' plan built from a subset of dframe disjoint from the given row.
#' The goal is to try to and supply a method of breaking nested model bias other than splitting
#' into calibration, training, test sets.
#'  
#' @param dframe Data frame to learn treatments from (training data), must have at least 1 row.
#' @param varlist Names of columns to treat (effective variables).
#' @param outcomename Name of column holding outcome variable. dframe[[outcomename]] must be only finite non-missing values and there must be a cut such that dframe[[outcomename]] is both above the cut at least twice and below the cut at least twice.
#' @param ... no additional arguments, declared to forced named binding of later arguments
#' @param weights optional training weights for each row
#' @param minFraction optional minimum frequency a categorical level must have to be converted to an indicator column.
#' @param smFactor optional smoothing factor for impact coding models.
#' @param rareCount optional integer, suppress direct effects of level of this count or less.
#' @param rareSig optional numeric, suppress direct effects of level of this significance value greater.  Set to one to turn off effect.
#' @param collarProb what fraction of the data (pseudo-probability) to collar data at (<0.5).
#' @param scale optional if TRUE replace numeric variables with regression ("move to outcome-scale").
#' @param doCollar optional if TRUE collar numeric variables by cutting off after a tail-probability specified by collarProb during treatment design.
#' @param ncross optional scalar>=2 number of cross-validation rounds to design.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow
#' @return treatment plan (for use with prepare)
#' @seealso \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}} \code{\link{prepare}}
#' @examples
#' 
#' set.seed(23525)
#' zip <- paste('z',1:100)
#' N = 200
#' d <- data.frame(zip=sample(zip,N,replace=TRUE),
#'                 zip2=sample(zip,N,replace=TRUE),
#'                 y=runif(N))
#' del <- runif(length(zip))
#' names(del) <- zip
#' d$y <- d$y + del[d$zip2]
#' d$yc <- d$y>=mean(d$y)
#' cN <- mkCrossFrameNExperiment(d,c('zip','zip2'),'y',
#'    rareCount=2,rareSig=0.9)
#' cor(cN$crossFrame$y,cN$crossFrame$zip_catN)  # poor
#' cor(cN$crossFrame$y,cN$crossFrame$zip2_catN) # better
#' treatments <- cN$treatments
#' dTrainV <- cN$crossFrame
#' 
#' @export
mkCrossFrameNExperiment <- function(dframe,varlist,outcomename,
                                    ...,
                                    weights=c(),
                                    minFraction=0.02,smFactor=0.0,
                                    rareCount=0,rareSig=1,
                                    collarProb=0.00,
                                    scale=FALSE,doCollar=TRUE,
                                    ncross=3,
                                    parallelCluster=NULL) {
  .checkArgs(dframe=dframe,varlist=varlist,outcomename=outcomename,...)
  if(!is.data.frame(dframe)) {
    stop("dframe must be a data frame")
  }
  if(collarProb>=0.5) {
    stop("collarProb must be < 0.5")
  }
  if(nrow(dframe)<1) {
    stop("most have rows")
  }
  if(!(outcomename %in% colnames(dframe))) {
    stop("outcomename must be a column name of dframe")
  }
  if(is.null(weights)) {
    weights <- rep(1.0,nrow(dframe))
  }
  treatments <- designTreatmentsN(dframe,varlist,outcomename,
                                  weights=weights,
                                  minFraction=minFraction,smFactor=smFactor,
                                  rareCount=rareCount,rareSig=rareSig,
                                  collarProb=collarProb,
                                  verbose=FALSE,
                                  parallelCluster=parallelCluster)
  zC <- NULL
  zoY <- dframe[[outcomename]]
  newVarsS <- treatments$scoreFrame$varName[treatments$scoreFrame$varMoves]
  crossDat <- .mkCrossFrame(dframe,varlist,newVarsS,outcomename,zoY,
                            zC,NULL,
                            weights,
                            minFraction,smFactor,
                            rareCount,rareSig,
                            collarProb,
                            FALSE,
                            scale,doCollar,
                            ncross,
                            parallelCluster)
  list(treatments=treatments,crossFrame=crossDat$crossFrame)
}


