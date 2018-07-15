# variable treatments type def: list { origvar, newvars, f(col,args), args, treatmentName, scales } can share orig var



#' vtreat: A Statistically Sound 'data.frame' Processor/Conditioner
#'
#' A 'data.frame' processor/conditioner that prepares real-world data for predictive modeling in a statistically sound manner.
#' 'vtreat' prepares variables so that data has fewer exceptional cases, making
#' it easier to safely use models in production. Common problems 'vtreat' defends
#' against: 'Inf', 'NA', too many categorical levels, rare categorical levels, and new
#' categorical levels (levels seen during application, but not during training).
#' 'vtreat::prepare' should be used as you would use 'model.matrix'.
#'
#'
#'For more information:
#' \itemize{
#'   \item \code{vignette('vtreat', package='vtreat')}
#'   \item \code{vignette(package='vtreat')}
#'   \item Website: \url{https://github.com/WinVector/vtreat} }
#'
#' @docType package
#' @name vtreat
NULL


#' @importFrom stats aggregate anova as.formula binomial chisq.test fisher.test glm lm lm.wfit pchisq pf quantile
#' @importFrom utils packageVersion
NULL




#'
#' Original variable name from a treatmentplan$treatment item.
#' @param x vtreatment item.
#' @seealso \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}} \code{\link{designTreatmentsZ}}
#' @export
#' 
vorig <- function(x) { x$origvar }


#'
#' New treated variable names from a treatmentplan$treatment item.
#' @param x vtreatment item
#' @seealso \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}} \code{\link{designTreatmentsZ}}
#' @export
vnames <- function(x) { x$newvars }

#'
#' Display treatment plan.
#' @param x treatment plan
#' @param ... additional args (to match general signature).
#' @export
format.vtreatment <- function(x, ...) { 
  paste(
    'vtreat \'',x$treatmentName,
    '\'(\'',x$origvar,'\'(',x$origType,',',x$origClass,')->',
    x$convertedColClass,'->\'',
    paste(x$newvars,collapse='\',\''),
    '\')',sep='') 
}

#' @export
as.character.vtreatment <- function (x, ...) {
  format(x, ...)
}

#'
#' Print treatmentplan.
#' @param x treatmentplan
#' @param ... additional args (to match general signature).
#' @seealso \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}} \code{\link{designTreatmentsZ}} \code{\link{prepare}}
#' @export
print.vtreatment <- function(x, ...) { 
  print(format(x), ...) 
}





#' @export
format.treatmentplan <- function(x, ...) { 
  format(x$scoreFrame)
}

#' @export
as.character.treatmentplan <- function (x, ...) {
  format(x, ...)
}

#'
#' Print treatmentplan.
#' @param x treatmentplan
#' @param ... additional args (to match general signature).
#' @seealso \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}} \code{\link{designTreatmentsZ}} \code{\link{prepare}}
#' @export
print.treatmentplan <- function(x, ...) { 
  print(format(x), ...) 
}








#' Build all treatments for a data frame to predict a categorical outcome.
#' 
#' Function to design variable treatments for binary prediction of a
#' categorical outcome.  Data frame is assumed to have only atomic columns
#' except for dates (which are converted to numeric). Note: re-encoding high cardinality
#' categorical variables can introduce undesirable nested model bias, for such data consider
#' using \code{\link{mkCrossFrameCExperiment}}.
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
#' @param rareCount optional integer, allow levels with this count or below to be pooled into a shared rare-level.  Defaults to 0 or off.
#' @param rareSig optional numeric, suppress levels from pooling at this significance value greater.  Defaults to NULL or off.
#' @param collarProb what fraction of the data (pseudo-probability) to collar data at if doCollar is set during \code{\link{prepare}}.
#' @param codeRestriction what types of variables to produce (character array of level codes, NULL means no restriction).
#' @param customCoders map from code names to custom categorical variable encoding functions (please see \url{https://github.com/WinVector/vtreat/blob/master/extras/CustomLevelCoders.md}).
#' @param splitFunction (optional) see vtreat::buildEvalSets .
#' @param ncross optional scalar >=2 number of cross validation splits use in rescoring complex variables.
#' @param forceSplit logical, if TRUE force cross-validated significance calculations on all variables.
#' @param catScaling optional, if TRUE use glm() linkspace, if FALSE use lm() for scaling.
#' @param verbose if TRUE print progress.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow.
#' @param use_parallel logical, if TRUE use parallel methods (when parallel cluster is set).
#' @return treatment plan (for use with prepare)
#' @seealso \code{\link{prepare}} \code{\link{designTreatmentsN}} \code{\link{designTreatmentsZ}} \code{\link{mkCrossFrameCExperiment}}
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
                              rareCount=0,rareSig=NULL,
                              collarProb=0.00,
                              codeRestriction=NULL,
                              customCoders=NULL, 
                              splitFunction=NULL,ncross=3,
                              forceSplit=FALSE,
                              catScaling=FALSE,
                              verbose=TRUE,
                              parallelCluster=NULL,
                              use_parallel= TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::designTreatmentsC")
  .checkArgs(dframe=dframe,varlist=varlist,outcomename=outcomename)
  if(!(outcomename %in% colnames(dframe))) {
    stop("outcomename must be a column name of dframe")
  }
  if(any(is.na(dframe[[outcomename]]))) {
    stop("There are missing values in the outcome column, can not apply designTreatmentsC.")
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
                                   codeRestriction,
                                   customCoders,
                                   splitFunction,ncross,forceSplit,
                                   catScaling,
                                   verbose = verbose,
                                   parallelCluster = parallelCluster,
                                   use_parallel = use_parallel)
  treatments$outcomeTarget <- outcometarget
  treatments$outcomeType <- 'Binary'
  treatments
}




#' build all treatments for a data frame to predict a numeric outcome
#' 
#' Function to design variable treatments for binary prediction of a
#' numeric outcome.  Data frame is assumed to have only atomic columns
#' except for dates (which are converted to numeric).
#' Note: each column is processed independently of all others. 
#' Note: re-encoding high cardinality
#' categorical variables can introduce undesirable nested model bias, for such data consider
#' using \code{\link{mkCrossFrameNExperiment}}.
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
#' @param rareCount optional integer, allow levels with this count or below to be pooled into a shared rare-level.  Defaults to 0 or off.
#' @param rareSig optional numeric, suppress levels from pooling at this significance value greater.  Defaults to NULL or off.
#' @param collarProb what fraction of the data (pseudo-probability) to collar data at if doCollar is set during \code{\link{prepare}}.
#' @param codeRestriction what types of variables to produce (character array of level codes, NULL means no restriction).
#' @param customCoders map from code names to custom categorical variable encoding functions (please see \url{https://github.com/WinVector/vtreat/blob/master/extras/CustomLevelCoders.md}).
#' @param splitFunction (optional) see vtreat::buildEvalSets .
#' @param ncross optional scalar >=2 number of cross validation splits use in rescoring complex variables.
#' @param forceSplit logical, if TRUE force cross-validated significance calculations on all variables.
#' @param verbose if TRUE print progress.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow.
#' @param use_parallel logical, if TRUE use parallel methods (when parallel cluster is set).
#' @return treatment plan (for use with prepare)
#' @seealso \code{\link{prepare}} \code{\link{designTreatmentsC}} \code{\link{designTreatmentsZ}} \code{\link{mkCrossFrameNExperiment}}
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
                              rareCount=0,rareSig=NULL,
                              collarProb=0.00,
                              codeRestriction=NULL,
                              customCoders=NULL,
                              splitFunction=NULL,ncross=3,
                              forceSplit=FALSE,
                              verbose=TRUE,
                              parallelCluster=NULL,
                              use_parallel= TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::designTreatmentsN")
  .checkArgs(dframe=dframe,varlist=varlist,outcomename=outcomename)
  if(!(outcomename %in% colnames(dframe))) {
    stop("outcomename must be a column name of dframe")
  }
  if(any(is.na(dframe[[outcomename]]))) {
    stop("There are missing values in the outcome column, can not apply designTreatmentsN.")
  }
  ycol <- dframe[[outcomename]]
  if(min(ycol)>=max(ycol)) {
    stop("dframe[[outcomename]] must vary")
  }
  catScaling=FALSE
  treatments <- .designTreatmentsX(dframe,varlist,outcomename,ycol,
                     c(),c(),
                     weights,
                     minFraction,smFactor,
                     rareCount,rareSig,
                     collarProb,
                     codeRestriction, customCoders,
                     splitFunction,ncross,forceSplit,
                     catScaling,
                     verbose = verbose,
                     parallelCluster = parallelCluster,
                     use_parallel = use_parallel)
  treatments$outcomeType <- 'Numeric'
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
#' @param minFraction optional minimum frequency a categorical level must have to be converted to an indicator column.
#' @param rareCount optional integer, allow levels with this count or below to be pooled into a shared rare-level.  Defaults to 0 or off.
#' @param collarProb what fraction of the data (pseudo-probability) to collar data at if doCollar is set during \code{\link{prepare}}.
#' @param codeRestriction what types of variables to produce (character array of level codes, NULL means no restriction).
#' @param customCoders map from code names to custom categorical variable encoding functions (please see \url{https://github.com/WinVector/vtreat/blob/master/extras/CustomLevelCoders.md}).
#' @param verbose if TRUE print progress.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow.
#' @param use_parallel logical, if TRUE use parallel methods (if parallel cluster is set).
#' @return treatment plan (for use with prepare)
#' @seealso \code{\link{prepare}} \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}} 
#' @examples
#' 
#' dTrainZ <- data.frame(x=c('a','a','a','a','b','b',NA,'e','e'),
#'     z=c(1,2,3,4,5,6,7,NA,9))
#' dTestZ <- data.frame(x=c('a','x','c',NA),
#'     z=c(10,20,30,NA))
#' treatmentsZ = designTreatmentsZ(dTrainZ, colnames(dTrainZ),
#'   rareCount=0)
#' dTrainZTreated <- prepare(treatmentsZ, dTrainZ)
#' dTestZTreated <- prepare(treatmentsZ, dTestZ)
#' 
#' @export
designTreatmentsZ <- function(dframe,varlist,
                              ...,
                              minFraction=0.0,
                              weights=c(),
                              rareCount=0,
                              collarProb=0.0,
                              codeRestriction=NULL,
                              customCoders=NULL,
                              verbose=TRUE,
                              parallelCluster=NULL,
                              use_parallel= TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::designTreatmentsZ")
  # build a name disjoint from column names
  outcomename <- setdiff(paste('VTREATTEMPCOL',
                               seq_len(ncol(dframe) + length(varlist) + 1), 
                               sep='_'),
                         c(colnames(dframe),varlist))[[1]]
  catScaling <- FALSE
  dframe[[outcomename]] <- 0
  .checkArgs(dframe=dframe,varlist=varlist,outcomename=outcomename)
  ycol <- dframe[[outcomename]]
  treatments <- .designTreatmentsX(dframe,varlist,outcomename,ycol,
                     c(),c(),
                     weights,
                     minFraction,smFactor=0,
                     rareCount,rareSig=1,
                     collarProb,
                     codeRestriction, customCoders, FALSE,
                     NULL,3,
                     catScaling,
                     verbose = verbose,
                     parallelCluster = parallelCluster,
                     use_parallel = use_parallel)
  treatments$outcomeType <- 'None'
  treatments$meanY <- NA
  treatments
}



#' Track unique character values for variables.
#' 
#' Builds lists of observed unique character values of varlist variables from the data frame.
#'  
#' @param dframe Data frame to learn treatments from (training data), must have at least 1 row.
#' @param varlist Names of columns to treat (effective variables).
#' @return named list of values seen.
#' 
#' @seealso \code{\link{prepare}}, \code{\link{novel_value_summary}}
#' 
#' @examples
#' 
#' set.seed(23525)
#' zip <- c(NA, paste('z', 1:100, sep = "_"))
#' N <- 500
#' d <- data.frame(zip = sample(zip, N, replace=TRUE),
#'                 zip2 = sample(zip, N, replace=TRUE),
#'                 y = runif(N))
#' dSample <- d[1:300, , drop = FALSE]
#' tplan <- designTreatmentsN(dSample, 
#'                            c("zip", "zip2"), "y", 
#'                            verbose = FALSE)
#' trackedValues <- track_values(dSample, c("zip", "zip2"))
#' # don't normally want to catch warnings,
#' # doing it here as this is an example 
#' # and must not have unhandled warnings.
#' tryCatch(
#'   prepare(tplan, d, trackedValues = trackedValues),
#'   warning = function(w) { cat(paste(w, collapse = "\n")) })
#' 
#' @export
#' 
track_values <- function(dframe, varlist) {
  observed_values <- lapply(varlist, 
                            function(vi) {
                              unique(as.character(dframe[[vi]]))
                            })
  names(observed_values) <- varlist
  observed_values
}

#' Report new/novel appearances of character values.
#'  
#' @param dframe Data frame to inspect.
#' @param trackedValues optional named list mapping variables to know values, allows warnings upon novel level appearances (see \code{\link{track_values}})
#' @return frame of novel occurrences
#' 
#' @seealso \code{\link{prepare}}, \code{\link{track_values}}
#' 
#' @examples
#' 
#' set.seed(23525)
#' zip <- c(NA, paste('z', 1:10, sep = "_"))
#' N <- 10
#' d <- data.frame(zip = sample(zip, N, replace=TRUE),
#'                 zip2 = sample(zip, N, replace=TRUE),
#'                 y = runif(N))
#' dSample <- d[1:5, , drop = FALSE]
#' trackedValues <- track_values(dSample, c("zip", "zip2"))
#' novel_value_summary(d, trackedValues)
#' 
#' @export
#' 
novel_value_summary <- function(dframe, trackedValues) {
  novel <- data.frame(row_index = 1, column = "", value = "",
                      stringsAsFactors = FALSE)
  novel <- novel[c(), , drop = FALSE]
  novels <- lapply(sort(intersect(names(trackedValues),
                                  colnames(dframe))),
                   function(v) {
                     newstuff <- !(dframe[[v]] %in% trackedValues[[v]])
                     if(sum(newstuff)>0) {
                       idxs <- which(newstuff)
                       vals <- as.character(dframe[[v]][idxs])
                       return(data.frame(row_index = idxs,
                                         column = v,
                                         value = vals,
                                         stringsAsFactors = FALSE))
                     } 
                     NULL
                   })
  novels <- c(list(novel), novels)
  novels <- novels[!is.null(novels)]
  .rbindListOfFrames(novels)
}

#' Apply treatments and restrict to useful variables.
#' 
#' Use a treatment plan to prepare a data frame for analysis.  The
#' resulting frame will have new effective variables that are numeric
#' and free of NaN/NA.  If the outcome column is present it will be copied over.
#' The intent is that these frames are compatible with more machine learning
#' techniques, and avoid a lot of corner cases (NA,NaN, novel levels, too many levels).
#' Note: each column is processed independently of all others.  Also copies over outcome if present.
#' Note: treatmentplan's are not meant for long-term storage, a warning is issued if the version of
#' vtreat that produced the plan differs from the version running \code{prepare()}.
#' 
#' @param treatmentplan Plan built by designTreantmentsC() or designTreatmentsN()
#' @param dframe Data frame to be treated
#' @param ... no additional arguments, declared to forced named binding of later arguments
#' @param pruneSig suppress variables with significance above this level
#' @param scale optional if TRUE replace numeric variables with single variable model regressions ("move to outcome-scale").  These have mean zero and (for variables with significant less than 1) slope 1 when regressed  (lm for regression problems/glm for classification problems) against outcome.
#' @param doCollar optional if TRUE collar numeric variables by cutting off after a tail-probability specified by collarProb during treatment design.
#' @param varRestriction optional list of treated variable names to restrict to
#' @param codeRestriction optional list of treated variable codes to restrict to
#' @param trackedValues optional named list mapping variables to know values, allows warnings upon novel level appearances (see \code{\link{track_values}})
#' @param extracols extra columns to copy.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow.
#' @param use_parallel logical, if TRUE use parallel methods.
#' @return treated data frame (all columns numeric- without NA, NaN)
#' 
#' @seealso \code{\link{mkCrossFrameCExperiment}}, \code{\link{mkCrossFrameNExperiment}}, \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}} \code{\link{designTreatmentsZ}}
#' @examples
#' 
#' dTrainN <- data.frame(x= c('a','a','a','a','b','b','b'),
#'                       z= c(1,2,3,4,5,6,7),
#'                       y= c(0,0,0,1,0,1,1))
#' dTestN <- data.frame(x= c('a','b','c',NA),
#'                      z= c(10,20,30,NA))
#' treatmentsN = designTreatmentsN(dTrainN,colnames(dTrainN), 'y')
#' dTrainNTreated <- prepare(treatmentsN, dTrainN, pruneSig= 0.2)
#' dTestNTreated <- prepare(treatmentsN, dTestN, pruneSig= 0.2)
#' 
#' dTrainC <- data.frame(x= c('a','a','a','b','b','b'),
#'                       z= c(1,2,3,4,5,6),
#'                       y= c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))
#' dTestC <- data.frame(x= c('a','b','c',NA),
#'                      z= c(10,20,30,NA))
#' treatmentsC <- designTreatmentsC(dTrainC, colnames(dTrainC),'y',TRUE)
#' dTrainCTreated <- prepare(treatmentsC, dTrainC, varRestriction= c('z_clean'))
#' dTestCTreated <- prepare(treatmentsC, dTestC, varRestriction= c('z_clean'))
#'
#' dTrainZ <- data.frame(x= c('a','a','a','b','b','b'),
#'                       z= c(1,2,3,4,5,6))
#' dTestZ <- data.frame(x= c('a','b','c',NA),
#'                      z= c(10,20,30,NA))
#' treatmentsZ <- designTreatmentsZ(dTrainZ, colnames(dTrainZ))
#' dTrainZTreated <- prepare(treatmentsZ, dTrainZ, codeRestriction= c('lev'))
#' dTestZTreated <- prepare(treatmentsZ, dTestZ, codeRestriction= c('lev'))
#' 
#' 
#' @export
prepare <- function(treatmentplan, dframe,
                    ...,
                    pruneSig= NULL,
                    scale= FALSE,
                    doCollar= FALSE,
                    varRestriction= NULL,
                    codeRestriction= NULL,
                    trackedValues= NULL,
                    extracols= NULL,
                    parallelCluster= NULL,
                    use_parallel= TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::prepare")
  .checkArgs1(dframe=dframe)
  if(class(treatmentplan)!='treatmentplan') {
    stop("treatmentplan must be of class treatmentplan")
  }
  vtreatVersion <- packageVersion('vtreat')
  if(is.null(treatmentplan$vtreatVersion) ||
     (treatmentplan$vtreatVersion!=vtreatVersion)) {
    warning(paste('treatments designed with vtreat version',
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
  if(!is.null(trackedValues)) {
    for(v in sort(intersect(names(trackedValues),
                            colnames(dframe)))) {
      new_values <- setdiff(dframe[[v]], trackedValues[[v]])
      if(length(new_values)>0) {
        if(length(new_values)>5) {
          vsample <- paste(new_values[1:5], collapse = ", ")
          vsample <- paste0(vsample, ", ...")
        } else {
          vsample <- paste(new_values, collapse = ", ")
        }
        wmsg <- paste0("vtreat::prepare: column \"", v, "\" has ",
                      length(new_values), 
                      " previously unseen values:",
                      vsample, " .")
        warning(wmsg)
      }
    }
  }
  if(treatmentplan$outcomeType=='None') {
    pruneSig <- NULL
  }
  useable <- treatmentplan$scoreFrame$varMoves
  if(!is.null(pruneSig)) {
    useable <- useable & (treatmentplan$scoreFrame$sig<=pruneSig)
  }
  useableVars <- treatmentplan$scoreFrame$varName[useable]
  if(!is.null(varRestriction)) {
    useableVars <- intersect(useableVars,varRestriction)
  }
  if(!is.null(codeRestriction)) {
    hasSelectedCode <- treatmentplan$scoreFrame$code %in% codeRestriction
    useableVars <- intersect(useableVars, 
                            treatmentplan$scoreFrame$varName[hasSelectedCode])
  }
  if(length(useableVars)<=0) {
    stop('no useable vars')
  }
  for(ti in treatmentplan$treatments) {
    if(length(intersect(ti$newvars,useableVars))>0) {
      newType <- typeof(dframe[[ti$origvar]])
      newClass <- paste(class(dframe[[ti$origvar]]))
      if((ti$origType!=newType) || (ti$origClass!=newClass)) {
        warning(paste('variable',ti$origvar,'expected type/class',
                   ti$origType,ti$origClass,
                   'saw ',newType,newClass))
                   
      }
    }
  }
  treated <- .vtreatList(treatmentplan$treatments,dframe,useableVars,scale,doCollar,
                         parallelCluster = parallelCluster,
                         use_parallel = use_parallel)
  # copy outcome and extracols over when present
  for(ci in unique(c(treatmentplan$outcomename, extracols))) {
    if(ci %in% colnames(dframe)) {
      treated[[ci]] <- dframe[[ci]]
    }
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
#' @param rareCount optional integer, allow levels with this count or below to be pooled into a shared rare-level.  Defaults to 0 or off.
#' @param rareSig optional numeric, suppress levels from pooling at this significance value greater.  Defaults to NULL or off.
#' @param collarProb what fraction of the data (pseudo-probability) to collar data at if doCollar is set during \code{\link{prepare}}.
#' @param codeRestriction what types of variables to produce (character array of level codes, NULL means no restriction).
#' @param customCoders map from code names to custom categorical variable encoding functions (please see \url{https://github.com/WinVector/vtreat/blob/master/extras/CustomLevelCoders.md}).
#' @param scale optional if TRUE replace numeric variables with regression ("move to outcome-scale").
#' @param doCollar optional if TRUE collar numeric variables by cutting off after a tail-probability specified by collarProb during treatment design.
#' @param splitFunction (optional) see vtreat::buildEvalSets .
#' @param ncross optional scalar>=2 number of cross-validation rounds to design.
#' @param forceSplit logical, if TRUE force cross-validated significance calculations on all variables.
#' @param catScaling optional, if TRUE use glm() linkspace, if FALSE use lm() for scaling.
#' @param verbose if TRUE print progress.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow.
#' @param use_parallel logical, if TRUE use parallel methods.
#' @return list with treatments and crossFrame
#' 
#' @seealso \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}} \code{\link{prepare}}
#' 
#' @examples
#' 
#' set.seed(23525)
#' zip <- paste('z',1:100)
#' N <- 200
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
                                    codeRestriction=NULL,
                                    customCoders=NULL,
                                    scale=FALSE,doCollar=FALSE,
                                    splitFunction=NULL,ncross=3,
                                    forceSplit = FALSE,
                                    catScaling=FALSE,
                                    verbose= TRUE,
                                    parallelCluster=NULL,
                                    use_parallel = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::mkCrossFrameCExperiment")
  .checkArgs(dframe=dframe,varlist=varlist,outcomename=outcomename)
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
  if(any(is.na(dframe[[outcomename]]))) {
    stop("There are missing values in the outcome column, can not run mkCrossFrameCExperiment.")
  }
  if(is.null(weights)) {
    weights <- rep(1.0,nrow(dframe))
  }
  if(verbose) {
    print(paste("vtreat", 
                packageVersion("vtreat"),
                "start initial treatment design", date()))
  }
  treatments <- designTreatmentsC(dframe,varlist,outcomename,outcometarget,
                                  weights=weights,
                                  minFraction=minFraction,smFactor=smFactor,
                                  rareCount=rareCount,rareSig=rareSig,
                                  collarProb=collarProb,
                                  codeRestriction=codeRestriction,
                                  customCoders=customCoders,
                                  splitFunction=splitFunction,ncross=ncross,
                                  forceSplit = forceSplit,
                                  catScaling=catScaling,
                                  verbose=FALSE,
                                  parallelCluster=parallelCluster,
                                  use_parallel = use_parallel)
  zC <- dframe[[outcomename]]
  zoY <- ifelse(zC==outcometarget,1,0)
  newVarsS <- treatments$scoreFrame$varName[(treatments$scoreFrame$varMoves) &
                                              (treatments$scoreFrame$sig<1)]
  if(verbose) {
    print(paste(" start cross frame work", date()))
  }
  crossDat <- .mkCrossFrame(dframe,treatments,
                            varlist,newVarsS,outcomename,zoY,
                            zC,outcometarget,
                            weights,
                            minFraction,smFactor,
                            rareCount,rareSig,
                            collarProb,
                            codeRestriction,
                            customCoders,
                            scale,doCollar,
                            splitFunction,ncross,
                            catScaling,
                            parallelCluster = parallelCluster,
                            use_parallel = use_parallel)
  crossFrame <- crossDat$crossFrame
  newVarsS <- intersect(newVarsS,colnames(crossFrame))
  goodVars <- newVarsS[vapply(newVarsS,
                              function(v) {
                                min(crossFrame[[v]])<max(crossFrame[[v]])
                              },
                              logical(1))]
  # Make sure scoreFrame and crossFrame are consistent in variables mentioned
  treatments$scoreFrame <- treatments$scoreFrame[treatments$scoreFrame$varName %in% goodVars,]
  crossFrame <- crossFrame[,colnames(crossFrame) %in% c(goodVars,outcomename),drop=FALSE]
  if(verbose) {
    print(paste(" vtreat::mkCrossFrameCExperiment done", date()))
  }
  res <- list(treatments=treatments,
              crossFrame=crossFrame,
              crossWeights=crossDat$crossWeights,
              method=crossDat$method,
              evalSets=crossDat$evalSets)
  class(res) <- "vtreat_cross_frame_experiment"
  res
}


#' Run a numeric cross frame experiment.
#' 
#' Builds a \code{\link{designTreatmentsN}} treatment plan and a data frame prepared 
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
#' @param rareCount optional integer, allow levels with this count or below to be pooled into a shared rare-level.  Defaults to 0 or off.
#' @param rareSig optional numeric, suppress levels from pooling at this significance value greater.  Defaults to NULL or off.
#' @param collarProb what fraction of the data (pseudo-probability) to collar data at if doCollar is set during \code{\link{prepare}}.
#' @param codeRestriction what types of variables to produce (character array of level codes, NULL means no restriction).
#' @param customCoders map from code names to custom categorical variable encoding functions (please see \url{https://github.com/WinVector/vtreat/blob/master/extras/CustomLevelCoders.md}).
#' @param scale optional if TRUE replace numeric variables with regression ("move to outcome-scale").
#' @param doCollar optional if TRUE collar numeric variables by cutting off after a tail-probability specified by collarProb during treatment design.
#' @param splitFunction (optional) see vtreat::buildEvalSets .
#' @param ncross optional scalar>=2 number of cross-validation rounds to design.
#' @param forceSplit logical, if TRUE force cross-validated significance calculations on all variables.
#' @param verbose if TRUE print progress.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow.
#' @param use_parallel logical, if TRUE use parallel methods.
#' @return treatment plan (for use with prepare)
#' @seealso \code{\link{designTreatmentsC}} \code{\link{designTreatmentsN}} \code{\link{prepare}}
#' @examples
#' 
#' set.seed(23525)
#' zip <- paste('z',1:100)
#' N <- 200
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
                                    codeRestriction=NULL,
                                    customCoders=NULL,
                                    scale=FALSE,doCollar=FALSE,
                                    splitFunction=NULL,ncross=3,
                                    forceSplit=FALSE,
                                    verbose= TRUE,
                                    parallelCluster=NULL,
                                    use_parallel = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::mkCrossFrameNExperiment")
  .checkArgs(dframe=dframe,varlist=varlist,outcomename=outcomename)
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
  if(any(is.na(dframe[[outcomename]]))) {
    stop("There are missing values in the outcome column, can not run mkCrossFrameNExperiment.")
  }
  catScaling=FALSE
  if(is.null(weights)) {
    weights <- rep(1.0,nrow(dframe))
  }
  if(verbose) {
    print(paste("vtreat", 
                packageVersion("vtreat"),
                "start initial treatment design", date()))
  }
  treatments <- designTreatmentsN(dframe,varlist,outcomename,
                                  weights=weights,
                                  minFraction=minFraction,smFactor=smFactor,
                                  rareCount=rareCount,rareSig=rareSig,
                                  collarProb=collarProb,
                                  codeRestriction = codeRestriction,
                                  customCoders = customCoders,
                                  splitFunction=splitFunction,ncross=ncross,
                                  forceSplit = forceSplit,
                                  verbose=FALSE,
                                  parallelCluster=parallelCluster,
                                  use_parallel = use_parallel)
  zC <- NULL
  zoY <- dframe[[outcomename]]
  newVarsS <- treatments$scoreFrame$varName[(treatments$scoreFrame$varMoves) &
                                              (treatments$scoreFrame$sig<1)]
  if(verbose) {
    print(paste(" start cross frame work", date()))
  }
  crossDat <- .mkCrossFrame(dframe,treatments,
                            varlist,newVarsS,outcomename,zoY,
                            zC,NULL,
                            weights,
                            minFraction,smFactor,
                            rareCount,rareSig,
                            collarProb,
                            codeRestriction, customCoders,
                            scale,doCollar,
                            splitFunction,ncross,
                            catScaling,
                            parallelCluster = parallelCluster,
                            use_parallel = use_parallel)
  crossFrame <- crossDat$crossFrame
  newVarsS <- intersect(newVarsS,colnames(crossFrame))
  goodVars <- newVarsS[vapply(newVarsS,
                              function(v) {
                                min(crossFrame[[v]])<max(crossFrame[[v]])
                              },
                              logical(1))]
  # Make sure scoreFrame and crossFrame are consistent in variables mentioned
  treatments$scoreFrame <- treatments$scoreFrame[treatments$scoreFrame$varName %in% goodVars,]
  crossFrame <- crossFrame[,colnames(crossFrame) %in% c(goodVars,outcomename),drop=FALSE]
  if(verbose) {
    print(paste(" vtreat::mkCrossFrameNExperiment done", date()))
  }
  res <- list(treatments=treatments,
              crossFrame=crossFrame,
              crossWeights=crossDat$crossWeights,
              method=crossDat$method,
              evalSets=crossDat$evalSets)
  class(res) <- "vtreat_cross_frame_experiment"
  res
}

#' @export
format.vtreat_cross_frame_experiment <- function(x, ...) {
  format(x$treatments)
}

#' @export
print.vtreat_cross_frame_experiment <- function(x, ...) {
  print(format(x))
  invisible(x)
}

