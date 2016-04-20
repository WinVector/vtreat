

#' Build disjoint set partition for out-of sample evaluation.
#' 
#' Return a disjoint partition of seq_len(nRows).  Very useful for any sort of
#' nested model situation (such as data prep, stacking, or super-learning).
#' 
#' @param nRows scalar, number of rows to sample from.
#' @param smallN scalar if nRows<=smallN return a 1-holdout plan (nRows singletons for evaluation).
#' @param ncross scalar if nRows>smallN return a ncross-way cross validation plan (ncross disjoint partition).
#' @return list of disjoint sets such that do.call(union,list) = seq_len(nRows).
#' 
#' @examples
#' 
#' # helper fns
#' # fit models using experiment plan to estimate out of sample behavior
#' fitModelAndApply <- function(trainData,applicaitonData) {
#'    model <- lm(y~x,data=trainData)
#'    predict(model,newdata=applicaitonData)
#' }
#' simualteOutOfSampleTrainEval <- function(d,fitApplyFn) {
#'    eSets <- buildEvalSets(nrow(d))
#'    evals <- lapply(eSets, 
#'       function(ei) { fitApplyFn(d[setdiff(seq_len(nrow(d)),ei),],d[ei,]) })
#'    pred <- numeric(nrow(d))
#'    for(eii in seq_len(length(eSets))) {
#'      pred[eSets[[eii]]] <- evals[[eii]]
#'    }
#'    pred
#' }
#' 
#' # run the experiment
#' set.seed(2352356)
#' # example data
#' d <- data.frame(x=rnorm(5),y=rnorm(5),
#'         outOfSampleEst=NA,inSampleEst=NA)
#'         
#' # fit model on all data
#' d$inSampleEst <- fitModelAndApply(d,d)
#' # compute in-sample R^2 (above zero, falsely shows a 
#' #   relation until we adjust for degrees of freedom)
#' 1-sum((d$y-d$inSampleEst)^2)/sum((d$y-mean(d$y))^2)
#' 
#' d$outOfSampleEst <- simualteOutOfSampleTrainEval(d,fitModelAndApply)
#' # compute out-sample R^2 (not positive, 
#' #  evidence of no relation)
#' 1-sum((d$y-d$outOfSampleEst)^2)/sum((d$y-mean(d$y))^2)
#' 
#' @export
buildEvalSets <- function(nRows,smallN=100,ncross=3) {
  # build a partition plan
  evalSets <- list()
  if(nRows<=1) {
    return(list(seq_len(nRows))) # no split plan possible
  }
  if(nRows<=smallN) {
    # small case, 1-holdout Jackknife style
    evalSets <- as.list(seq_len(nRows))
    return(evalSets)
  }
  #  Try for full k-way cross val
  done = FALSE
  while(!done) {
    groups <- sample.int(ncross,nRows,replace=TRUE)
    if(length(unique(groups))==ncross) {
      done = TRUE
    }
  }
  evalSets <- lapply(seq_len(ncross),function(i) which(groups==i))
  evalSets
}



# make a "cross frame" that is a frame where each treated row was treated only 
# by a treatment plan not involving the given row
.mkCrossFrame <- function(dframe,varlist,newVarsS,outcomename,zoY,
                          zC,zTarget,
                          weights,
                          minFraction,smFactor,
                          rareCount,rareSig,
                          collarProb,
                          impactOnly,
                          scale,doCollar,
                          parallelCluster) {
  verbose <- FALSE
  dsub <- dframe[,c(varlist,outcomename),drop=FALSE]
  # build a partition plan
  evalSets <- buildEvalSets(length(zoY))
  scoreFrame <- vector('list',length(evalSets))
  wtList <- vector('list',length(evalSets))
  rowList <- vector('list',length(evalSets))
  for(ei in evalSets) {
    dsubiEval <- dsub[ei,]
    dsubiBuild <- dsub[-ei,]
    zoYBuild <- zoY[-ei]
    zCBuild <- c()
    if(!is.null(zC)) {
      zCBuild <- zC[-ei]
    }
    wBuild <- weights[-ei]
    ti <- .designTreatmentsXS(dsubiBuild,varlist,outcomename,zoYBuild,
                              zCBuild,zTarget,
                              wBuild,
                              minFraction,smFactor,
                              rareCount,rareSig,
                              collarProb,
                              impactOnly,TRUE,
                              verbose,
                              parallelCluster)
    fi <- .vtreatList(ti,dsubiEval,newVarsS,scale,doCollar,
                      parallelCluster)
    # make sure each frame has the same structure
    for(v in setdiff(newVarsS,colnames(fi))) {
      fi[[v]] <- 0.0
    }
    fi <- fi[,newVarsS,drop=FALSE]
    fi[[outcomename]] <- dsubiEval[[outcomename]]
    scoreFrame[[length(scoreFrame)+1]] <- fi
    wtList[[length(wtList)+1]] <- weights[ei]
    rowList[[length(rowList)+1]] <- ei
  }
  scoreFrame <- do.call(rbind,scoreFrame)
  scoreWeights <- do.call(c,wtList)
  rowList <- do.call(c,rowList)
  if((length(rowList)==nrow(dframe))&&
     all(sort(rowList)==(1:nrow(dframe)))&&
     (!all(rowList==(1:nrow(dframe))))) {
    # undo permuation
    scoreFrame[rowList,] <- scoreFrame
    scoreWeights[rowList] <- scoreWeights[rowList]
  }
  list(crossFrame=scoreFrame,crossWeights=scoreWeights)
}


