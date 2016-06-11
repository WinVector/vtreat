
#' check if splits is a good partition of 1:nRows into ncross groups
#'
#' @param nRows number of rows to partition
#' @param ncross number of sets to partition into
#' @param splits partition to critique
#' @return problem with partition (null if good)
problemWithPartition <- function(nRows,ncross,splits) {
  if(is.null(splits)) {
    return("splits was null")
  }
  if(!is.list(splits)) {
    return("stratified split needs to be a list")
  }
  if(length(splits)!=ncross) {
    return("didn't get requested number of splits")
  }
  support <- sort(Reduce(union,splits))
  if(length(support)!=nRows) {
    return("bad split (missing values)")
  }
  if(!all(support==seq_len(nRows))) {
    return("bad split")
  }
  for(i in seq_len(ncross)) {
    if(length(splits[[i]])<=0) {
      return("empty partition element")
    }
  }
  for(i in seq_len(ncross-1)) {
    for(j in seq(i+1,ncross)) {
      if(length(intersect(splits[[i]],splits[[j]]))!=0) {
        return("non-disjoint splits")
      }
    }
  }
  NULL
}

#' Build disjoint set partition for out-of sample evaluation.
#' 
#' Return a disjoint partition of seq_len(nRows).  Very useful for any sort of
#' nested model situation (such as data prep, stacking, or super-learning).
#' 
#' Also sets attribute "splitmethod" on return value that describes how the split was performed.
#' attr(returnValue,'splitmethod') is one of: 'notsplit' (data was not split; corner cases
#' like single row data sets), 'oneway' (leave one out holdout), 'simplepartition' (a simple
#' partition), or 'userfunction' (user supplied function was actually used).  So any user
#' desired properties (such as stratification on y, or preservation of groups designated by 
#' original data row numbers) may not apply unless you see that 'userfunction' has been
#' used.
#' 
#' The intent is the user partitionFunction only needs to handle "easy cases" 
#' and maintain user invariants. If the user partitionFunction returns NULL,
#' throws, or returns an unacceptable partition then vtreat::buildEvalSets
#' returns its own eval set plan.  The signature of partitionFunction should
#' be partitionFunction(ncross,nRows,dframe,y) where ncross is the number of 
#' pieces we want in the partition, nRows is the number of rows to split,
#' dframe is the original dataframe (useful for any group control variables),
#' and y is a numeric vector representing outcome (useful for outcome stratification).
#' 
#' Note that buildEvalSets may not always return a partition in exceptional cases (such
#' as one row dataframes).
#' 
#' @param nRows scalar, >=1 number of rows to sample from.
#' @param ... no additional arguments, declared to forced named binding of later arguments.
#' @param dframe (optional) original data.frame, passed to user partitionFunction.
#' @param y (optional) numeric vector, outcome variable (possibly to stratify on), passed to user partitionFunction.
#' @param partitionFunction (optional) function taking arguments ncross,nRows,dframe, and y; returning a user desired split.
#' @return list of lists where the app portion of the sub-lists is a disjoint partition of seq_len(nRows) and each list as a train portion disjoint from app.
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
buildEvalSets <- function(nRows,...,
                          dframe=NULL,
                          y=NULL,
                          partitionFunction=NULL,
                          ncross=3) {
  # check args
  args <- list(...)
  if(length(args)!=0) {
    nm <- setdiff(paste(names(args),collapse=", "),'')
    nv <- length(args)-length(nm)
    stop(paste("unexpected arguments",nm,"(and",nv,"unexpected values)"))
  }
  if(ncross<2) {
    stop("vtreat::buildEvalSets must of ncross>=2")
  }
  if(!is.null(y)) {
    if(!is.numeric(y)) {
      stop('vtreat:buildEvalSets must have is.numeric(y)')
    }
    if(length(y)!=nRows) {
      stop('vtreat:buildEvalSets must have length(y)==nRows')
    }
  }
  if(!is.null(dframe)) {
    if(!is.data.frame(dframe)) {
      stop('vtreat:buildEvalSets must have is.data.frame(dframe)')
    }
    if(nrow(dframe)!=nRows) {
      stop('must have nrow(dframe)==nRows')
    }
  }
  splitmethod = 'none'
  splits <- NULL
  # try user partition function
  if(!is.null(partitionFunction)) {
    tryCatch({
      splits <- partitionFunction(ncross=ncross,nRows=nRows,dframe=dframe,y=y)
      if(!is.null(splits)) {
        names(splits) <- NULL
      }
      problem <- problemWithPartition(nRows,ncross,splits)
      if(!is.null(problem)) {
        warning(paste("vtreat::buildEvalSets user partition rejected: ",problem))
        splits <- NULL
      } else {
        splitmethod = 'userfunction'
      }
    },
    error = function(e) warning(paste('vtreat::buildEvalSets caught ',
                          as.character(e),'from user partitionFunction')
    ))
  }
  # deal with it ourselves if we have to
  fullSeq <- seq_len(nRows)
  if(is.null(splits)) {
    # okay, we will partition on our own
    if((nRows<=20)||(2*ncross>nRows)) {
      # one corner case
      if(nRows<=1) {
        # no split plan possible
        r <- list(list(train=fullSeq,app=fullSeq))
        attr(r,'splitmethod') <- 'notsplit'
        return(r)
      } else {
        # small case, 1-holdout Jackknife style
        splitmethod <- 'oneway'
        splits <- as.list(fullSeq)
      }
    } else {
      # know 2*ncross<=nRows
      #  Try for full k-way cross val
      splitmethod = 'simplepartition'
      perm <- sample.int(nRows,nRows,replace=FALSE)
      splits <- split(perm,1 + (fullSeq %% ncross))
      problem <- problemWithPartition(nRows,ncross,splits)
      if(!is.null(problem)) {
        stop(paste("problem with vtreat::buildEvalSets",problem))
      }
    }
  }
  evalSets <- lapply(splits,
                     function(appi) { 
                       list(train=setdiff(fullSeq,appi),app=appi)
                     })
  attr(evalSets,'splitmethod') <- splitmethod
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
                          partitionFunction,ncross,
                          parallelCluster) {
  verbose <- FALSE
  dsub <- dframe[,c(varlist,outcomename),drop=FALSE]
  # build a partition plan
  evalSets <- buildEvalSets(length(zoY),dframe=dframe,y=zoY,
                            partitionFunction=partitionFunction,ncross=ncross)
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


