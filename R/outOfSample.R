

#' Build disjoint set partition for out-of sample evaluation.
#' 
#' Return a disjoint partition of seq_len(nRows).  Very useful for any sort of
#' nested model situation (such as data prep, stacking, or super-learning).
#' 
#' @param nRows scalar, >=1 number of rows to sample from.
#' @param ncross scalar >=2 if nRows>smallN return a ncross-way cross validation plan (ncross disjoint partition).
#' @param smallN scalar at least 20 if nRows<=smallN return a 1-holdout plan (nRows singletons for evaluation).
#' @return list of lists where the app portion of the sublists is a disjoint partion of seq_len(nRows) and each list as a train portion disjoint from app.
#' 
#' @examples
#' 
#' # use
#' buildEvalSets(200)
#' 
#' # longer example
#' # helper fns
#' # fit models using experiment plan to estimate out of sample behavior
#' fitModelAndApply <- function(trainData,applicaitonData) {
#'    model <- lm(y~x,data=trainData)
#'    predict(model,newdata=applicaitonData)
#' }
#' simulateOutOfSampleTrainEval <- function(d,fitApplyFn) {
#'    eSets <- buildEvalSets(nrow(d))
#'    evals <- lapply(eSets, 
#'       function(ei) { fitApplyFn(d[ei$train,],d[ei$app,]) })
#'    pred <- numeric(nrow(d))
#'    for(eii in seq_len(length(eSets))) {
#'      pred[eSets[[eii]]$app] <- evals[[eii]]
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
#' d$outOfSampleEst <- simulateOutOfSampleTrainEval(d,fitModelAndApply)
#' # compute out-sample R^2 (not positive, 
#' #  evidence of no relation)
#' 1-sum((d$y-d$outOfSampleEst)^2)/sum((d$y-mean(d$y))^2)
#' 
#' @export
buildEvalSets <- function(nRows,ncross=3,smallN=100) {
  if(ncross<2) {
    stop("buildEvalSets: ncross must be at least 2")
  }
  # build a partition plan
  evalSets <- list()
  fullSeq <- seq_len(nRows)
  if(nRows<=1) {
    # no split plan possible
    return(list(list(train=fullSeq,app=fullSeq)))
  }
  if((nRows<=20)||(nRows<=smallN)||(2*ncross>nRows)) {
    # small case, 1-holdout Jackknife style
    return(lapply(fullSeq,
                  function(i) {
                    list(train=fullSeq[-i],app=i)
                  }))
  }
  # know 2*ncross<=nRows
  #  Try for full k-way cross val
  perm <- sample.int(nRows,nRows,replace=FALSE)
  splits <- split(perm,1 + (fullSeq %% ncross))
  evalSets <- lapply(splits,
                     function(appi) { 
                       list(train=setdiff(fullSeq,appi),app=appi)
                     })
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
                          ncross,
                          parallelCluster) {
  verbose <- FALSE
  dsub <- dframe[,c(varlist,outcomename),drop=FALSE]
  # build a partition plan
  evalSets <- buildEvalSets(length(zoY),ncross=ncross)
  crossFrameList <- vector('list',length(evalSets))
  wtList <- vector('list',length(evalSets))
  rList <- vector('list',length(evalSets))
  for(ei in seq_len(length(evalSets))) {
    evalIndices <- evalSets[[ei]]$app
    buildIndices <- evalSets[[ei]]$train
    dsubiEval <- dsub[evalIndices,]
    dsubiBuild <- dsub[buildIndices,]
    zoYBuild <- zoY[buildIndices]
    zCBuild <- c()
    if(!is.null(zC)) {
      zCBuild <- zC[buildIndices]
    }
    wBuild <- weights[buildIndices]
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
    # make sure each frame has the same column structure
    for(v in setdiff(newVarsS,colnames(fi))) {
      fi[[v]] <- 0.0
    }
    fi <- fi[,newVarsS,drop=FALSE]
    fi[[outcomename]] <- dsubiEval[[outcomename]]
    crossFrameList[[ei]] <- fi
    wtList[[ei]] <- weights[evalIndices]
    rList[[ei]] <- evalIndices
  }
  crossFrame <- .rbindListOfFrames(crossFrameList)
  scoreWeights <- unlist(wtList)
  rowList <- unlist(rList)
  if((length(rowList)==nrow(dframe))&&
     all(sort(rowList)==(1:nrow(dframe)))&&
     (!all(rowList==(1:nrow(dframe))))) {
    # undo permuation
    crossFrame[rowList,] <- crossFrame
    scoreWeights[rowList] <- scoreWeights[rowList]
  }
  list(crossFrame=crossFrame,crossWeights=scoreWeights)
}


