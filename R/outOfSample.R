
#' read application labels off a split plan.
#'
#' @param nRow number of rows in original data.frame.
#' @param plan split plan
#' @return vector of labels
#' 
#' @seealso \code{\link{kWayCrossValidation}}, \code{\link{kWayStratifiedY}}, and \code{\link{makekWayCrossValidationGroupedByColumn}}
#' 
#' @examples
#' 
#' plan <- kWayStratifiedY(3,2,NULL,NULL)
#' getSplitPlanAppLabels(3,plan)
#' 
#' @export
getSplitPlanAppLabels <- function(nRow,plan) {
  labels <- numeric(nRow)
  for(i in seq_len(length(plan))) {
    labels[plan[[i]]$app] <- i
  }
  labels
}

#' check if appPlan is a good carve-up of 1:nRows into nSplits groups
#'
#' @param nRows number of rows to carve-up
#' @param nSplits number of sets to carve-up into
#' @param appPlan carve-up to critique
#' @param strictCheck logical, if true expect application data to be a carve-up and training data to be a maximal partition and to match nSplits.
#' @return problem with carve-up (null if good)
#' 
#' @seealso \code{\link{kWayCrossValidation}}, \code{\link{kWayStratifiedY}}, and \code{\link{makekWayCrossValidationGroupedByColumn}}
#' 
#' @examples
#' 
#' plan <- kWayStratifiedY(3,2,NULL,NULL)
#' problemAppPlan(3,3,plan,TRUE)
#' 
#' @export
problemAppPlan <- function(nRows,nSplits,appPlan,strictCheck) {
  if(is.null(appPlan)) {
    return("appPlan was null")
  }
  if(!is.list(appPlan)) {
    return("appPlan needs be a list")
  }
  if((strictCheck)&&(nRows>1)&&(!is.null(nSplits))) {
    if(length(appPlan)!=nSplits) {
      return("didn't get requested number of groups in appPlan")
    }
  }
  fullSeq <- seq_len(nRows)
  seen <- c()
  for(i in seq_len(length(appPlan))) {
    si <- appPlan[[i]]
    if(!is.list(si)) {
      return("non list element in app plan")
    }
    ti <- si$train
    if(is.null(ti)) {
      return("missing train slot")
    }
    if(length(setdiff(ti,fullSeq))!=0) {
      return("unexpected symbols in train slot")
    }
    ai <- si$app
    if(is.null(ai)) {
      return("missing app slot")
    }
    if(length(setdiff(ai,fullSeq))!=0) {
      return("unexpected symbols in application slot")
    }
    if(nRows>1) {
      if(length(intersect(ti,ai))!=0) {
        return("train and application slots overlap")
      }
    }
    if(strictCheck) {
      if(length(setdiff(fullSeq,union(ai,ti)))>0) {
        return("non-maximal training set")
      }
      if(length(intersect(seen,ai))!=0) {
        return("repeated application row")
      }
    }
    seen <- union(seen,ai)
  }
  if(strictCheck) {
    if(length(seen)!=nRows) {
      return("not all rows appeared in application")
    }
  }
  NULL
}




#' One way holdout, a splitFunction in the sense of vtreat::buildEvalSets.
#' 
#' Note one way holdout can leak target expected values, so it should not
#' be preferred in nested modeling situations.
#' Also, doesn't respect nSplits.
#' 
#' @param nRows number of rows to split (integer >1).
#' @param nSplits number of groups to split into (ignored).
#' @param dframe original data frame (ignored).
#' @param y numeric outcome variable (ignored).
#' @return split plan
#' 
#' @examples
#' 
#' oneWayHoldout(3,NULL,NULL,NULL)
#' 
#' @export
oneWayHoldout <- function(nRows,nSplits,dframe,y) {
  if(nRows<=1) {
    return(NULL)
  }
  fullSeq <- seq_len(nRows)
  evalSets <- lapply(as.list(fullSeq),
                     function(appi) { 
                       list(train=setdiff(fullSeq,appi),app=appi)
                     })
  attr(evalSets,'splitmethod') <- 'oneway'
  evalSets
}

#' k-fold cross validation, a splitFunction in the sense of vtreat::buildEvalSets
#' 
#' @param nRows number of rows to split (>1).
#' @param nSplits number of groups to split into (>1,<=nRows).
#' @param dframe original data frame (ignored).
#' @param y numeric outcome variable (ignored).
#' @return split plan
#' 
#' @examples
#' 
#' kWayCrossValidation(7,2,NULL,NULL)
#' 
#' @export
kWayCrossValidation <- function(nRows,nSplits,dframe,y) {
  if((nRows<=1)||(nSplits<=1)||(nSplits>nRows)) {
    return(NULL)
  }
  fullSeq <- seq_len(nRows)
  perm <- sample.int(nRows,nRows,replace=FALSE)
  evalSets <- lapply(split(perm,1 + (fullSeq %% nSplits)),
                     function(appi) { 
                       list(train=setdiff(fullSeq,appi),app=appi)
                     })
  names(evalSets) <- NULL
  attr(evalSets,'splitmethod') <- 'kwaycross'
  evalSets
}


#' k-fold cross validation stratified on y, a splitFunction in the sense of vtreat::buildEvalSets
#' 
#' @param nRows number of rows to split (>1)
#' @param nSplits number of groups to split into (<nRows,>1).
#' @param dframe original data frame (ignored).
#' @param y numeric outcome variable try to have equidistributed in each split.
#' @return split plan
#' 
#' @examples
#' 
#' set.seed(23255)
#' d <- data.frame(y=sin(1:100))
#' pStrat <- kWayStratifiedY(nrow(d),5,d,d$y)
#' problemAppPlan(nrow(d),5,pStrat,TRUE)
#' d$stratGroup <- vtreat::getSplitPlanAppLabels(nrow(d),pStrat)
#' pSimple <- kWayCrossValidation(nrow(d),5,d,d$y)
#' problemAppPlan(nrow(d),5,pSimple,TRUE)
#' d$simpleGroup <- vtreat::getSplitPlanAppLabels(nrow(d),pSimple)
#' summary(tapply(d$y,d$simpleGroup,mean))
#' # ggplot(data=d,aes(x=y,color=as.factor(simpleGroup))) + 
#' #   geom_density() + ggtitle('simple grouping')
#' summary(tapply(d$y,d$stratGroup,mean))
#' # ggplot(data=d,aes(x=y,color=as.factor(stratGroup))) + 
#' #   geom_density() + ggtitle('y-stratified grouping')
#' 
#' 
#' # # And you can (and should) use your own functions or libraries.
#' # splitFn <- function(nRows,nSplits,dframe,y) {
#' #      fullSeq <- seq_len(nRows)
#' #      part <- caret::createFolds(y=y,k=nSplits)
#' #      lapply(part,
#' #             function(appi) { 
#' #                 list(train=setdiff(fullSeq,appi),app=appi)
#' #             })
#' # }
#' # pCaret <- splitFn(nrow(d),5,d,d$y)
#' # problemAppPlan(nrow(d),5,pCaret,TRUE)
#' # d$caretGroup <- vtreat::getSplitPlanAppLabels(nrow(d),pCaret)
#' # ggplot(data=d,aes(x=y,color=as.factor(caretGroup))) +
#' #    geom_density() + ggtitle('caret::createFolds grouping')
#' 
#' @export
kWayStratifiedY <- function(nRows,nSplits,dframe,y) {
  if((nRows<=1)||(nSplits<=1)||(nSplits>nRows)) {
    return(NULL)
  }
  if(is.null(y)||(length(unique(y))<=1)) {
    return(kWayCrossValidation(nRows,nSplits,NULL,NULL))
  }
  fullSeq <- seq_len(nRows)
  d <- data.frame(index=fullSeq,y=y)
  # initial permutation in case y has large constant blocks
  d <- d[order(sample.int(nRows,nRows,replace=FALSE)),]
  # order by y
  d <- d[order(d$y),]
  # mix within order segments
  rows_per_split <- ceiling(nRows/nSplits)
  mix_idx <- vector("list", rows_per_split)
  for(si in seq_len(rows_per_split)) {
    leftI <- si*nSplits - (nSplits-1)
    rightI <- min(si*nSplits,nRows)
    widthI <- 1+rightI-leftI
    if(widthI>1) {
      oldIndices <- leftI:rightI
      mix_idx[[si]] <- oldIndices[sample.int(widthI,widthI,replace=FALSE)]
    } else {
      mix_idx[[si]] <- leftI:rightI
    }
  }
  d[unlist(mix_idx),] <- d
  d$group <- (fullSeq %% nSplits) + 1
  carveUp <-  split(d$index,d$group)
  evalSets <- lapply(carveUp,
                     function(appi) { 
                       list(train=setdiff(fullSeq,appi),app=appi)
                     })
  names(evalSets) <- NULL
  attr(evalSets,'splitmethod') <- 'kwaycrossystratified'
  evalSets
}



#' k-fold cross validation stratified with replacement on y, a splitFunction in the sense of vtreat::buildEvalSets .
#' 
#' Build a k-fold cross validation sample where training sets are the same size as the original data,
#' and built by sampling disjoint from test/application sets (sampled with replacement).
#' 
#' @param nRows number of rows to split (>1)
#' @param nSplits number of groups to split into (<nRows,>1).
#' @param dframe original data frame (ignored).
#' @param y numeric outcome variable try to have equidistributed in each split.
#' @return split plan
#' 
#' @examples
#' 
#' set.seed(23255)
#' d <- data.frame(y=sin(1:100))
#' pStrat <- kWayStratifiedYReplace(nrow(d),5,d,d$y)
#' 
#' @export
kWayStratifiedYReplace <- function(nRows,nSplits,dframe,y) {
  if((nRows<=1)||(nSplits<=1)||(nSplits>nRows)) {
    return(NULL)
  }
  if(is.null(y)||(length(unique(y))<=1)) {
    return(kWayCrossValidation(nRows,nSplits,NULL,NULL))
  }
  fullSeq <- seq_len(nRows)
  d <- data.frame(index=fullSeq,y=y)
  # initial permutation in case y has large constant blocks
  d <- d[order(sample.int(nRows,nRows,replace=FALSE)),]
  # order by y
  d <- d[order(d$y),]
  # mix within order segments
  rows_per_split <- ceiling(nRows/nSplits)
  mix_idx <- vector("list", rows_per_split)
  for(si in seq_len(rows_per_split)) {
    leftI <- si*nSplits - (nSplits-1)
    rightI <- min(si*nSplits,nRows)
    widthI <- 1+rightI-leftI
    if(widthI>1) {
      oldIndices <- leftI:rightI
      mix_idx[[si]] <- oldIndices[sample.int(widthI,widthI,replace=FALSE)]
    } else {
      mix_idx[[si]] <- leftI:rightI
    }
  }
  d[unlist(mix_idx),] <- d
  d$group <- (fullSeq %% nSplits) + 1
  carveUp <-  split(d$index,d$group)
  evalSets <- lapply(carveUp,
                     function(appi) { 
                       trainIdxs <- setdiff(fullSeq,appi)
                       si <- sample.int(length(trainIdxs), nRows, replace=TRUE)
                       list(train=trainIdxs[si], app=appi)
                     })
  names(evalSets) <- NULL
  attr(evalSets,'splitmethod') <- 'kwaycrossystratifiedreplace'
  evalSets
}




#' Build a k-fold cross validation splitter, respecting (never splitting) groupingColumn.
#' 
#' @param groupingColumnName name of column to group by.
#' @return splitting function in the sense of vtreat::buildEvalSets.
#' 
#' @examples
#' 
#' d <- data.frame(y=sin(1:100))
#' d$group <- floor(seq_len(nrow(d))/5)
#' splitter <- makekWayCrossValidationGroupedByColumn('group')
#' split <- splitter(nrow(d),5,d,d$y)
#' d$splitLabel <- vtreat::getSplitPlanAppLabels(nrow(d),split)
#' rowSums(table(d$group,d$splitLabel)>0)
#' 
#' @export
makekWayCrossValidationGroupedByColumn <- function(groupingColumnName) {
  force(groupingColumnName) 
  function(nRows,nSplits,dframe,y) {
    if((nRows<=1)||(nSplits<=1)||(nSplits>nRows)) {
      return(NULL)
    }
    d <- data.frame(index=seq_len(nRows),
                    group=as.character(dframe[[groupingColumnName]]),
                    stringsAsFactors=FALSE)
    groups <- sort(unique(d$group))
    groupedPlan <- NULL
    if((!is.null(y))&&(length(unique(y))>1)) { # try for y-stratification
      d$y <- y
      groupedY <- aggregate(y~group,data=d,FUN=mean)$y
      groupedPlan <- kWayStratifiedY(length(groups),nSplits,NULL,groupedY)
    }
    if(is.null(groupedPlan)) {
      groupedPlan <- kWayCrossValidation(length(groups),nSplits,NULL,NULL)
    }
    if(is.null(groupedPlan)) {
      return(NULL)
    }
    splitmethod <- attr(groupedPlan,'splitmethod')
    carveUp <- lapply(groupedPlan,
                        function(gi) {
                          d$index[d$group %in% groups[gi$app]]
                        })
    fullSeq <- seq_len(nRows)
    evalSets <- lapply(carveUp,
                       function(appi) { 
                         list(train=setdiff(fullSeq,appi),app=appi)
                       })
    names(evalSets) <- NULL
    attr(evalSets,'splitmethod') <- paste0(splitmethod,'grouped')
    evalSets
  }
}




#' Build set carve-up for out-of sample evaluation.
#' 
#' Return a carve-up of seq_len(nRows).  Very useful for any sort of
#' nested model situation (such as data prep, stacking, or super-learning).
#' 
#' Also sets attribute "splitmethod" on return value that describes how the split was performed.
#' attr(returnValue,'splitmethod') is one of: 'notsplit' (data was not split; corner cases
#' like single row data sets), 'oneway' (leave one out holdout), 'kwaycross' (a simple
#' partition), 'userfunction' (user supplied function was actually used), or a user specified attribute.
#' Any user
#' desired properties (such as stratification on y, or preservation of groups designated by 
#' original data row numbers) may not apply unless you see that 'userfunction' has been
#' used.
#' 
#' The intent is the user splitFunction only needs to handle "easy cases" 
#' and maintain user invariants. If the user splitFunction returns NULL,
#' throws, or returns an unacceptable carve-up then vtreat::buildEvalSets
#' returns its own eval set plan.  The signature of splitFunction should
#' be splitFunction(nRows,nSplits,dframe,y) where nSplits is the number of 
#' pieces we want in the carve-up, nRows is the number of rows to split,
#' dframe is the original dataframe (useful for any group control variables),
#' and y is a numeric vector representing outcome (useful for outcome stratification).
#' 
#' Note that buildEvalSets may not always return a partition (such
#' as one row dataframes), or if the user split function chooses to make rows eligible for
#' application a different number of times.
#' 
#' @param nRows scalar, >=1 number of rows to sample from.
#' @param ... no additional arguments, declared to forced named binding of later arguments.
#' @param dframe (optional) original data.frame, passed to user splitFunction.
#' @param y (optional) numeric vector, outcome variable (possibly to stratify on), passed to user splitFunction.
#' @param splitFunction (optional) function taking arguments nSplits,nRows,dframe, and y; returning a user desired split.
#' @param nSplits integer, target number of splits.
#' @return list of lists where the app portion of the sub-lists is a disjoint carve-up of seq_len(nRows) and each list as a train portion disjoint from app.
#' 
#' @seealso \code{\link{kWayCrossValidation}}, \code{\link{kWayStratifiedY}}, and \code{\link{makekWayCrossValidationGroupedByColumn}}
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
                          splitFunction=NULL,
                          nSplits=3) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::buildEvalSets")
  # check args
  if(nSplits<2) {
    stop("vtreat::buildEvalSets must have nSplits>=2")
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
  # try user carve-up function
  if(!is.null(splitFunction)) {
    tryCatch({
      evalSets <- splitFunction(nRows=nRows,nSplits=nSplits,dframe=dframe,y=y)
      problem <- problemAppPlan(nRows,nSplits,evalSets,FALSE)
      if(is.null(problem)) {
        if(is.null(attr(evalSets,'splitmethod'))) {
          attr(evalSets,'splitmethod') <- 'userfunction'
        }
        return(evalSets)
      } else {
        warning(paste("vtreat::buildEvalSets user carve-up rejected: ",problem))
      }
    },
    error = function(e) warning(paste('vtreat::buildEvalSets caught ',
                          as.character(e),'from user splitFunction')
    ))
  }
  # deal with it ourselves if we have to
  fullSeq <- seq_len(nRows)
  # okay, we will carve-up on our own
  if(2*nSplits>nRows) {
    # one corner case
    if(nRows<=1) {
      # no split plan possible
      evalSets <- list(list(train=fullSeq,app=fullSeq))
      attr(evalSets,'splitmethod') <- 'notsplit'
    } else {
      # not necissarilly number of splits the user requested
      evalSets <- kWayCrossValidation(nRows=nRows,nSplits= min(nSplits, nRows),dframe=NULL,y=NULL)
      problem <- problemAppPlan(nRows,min(nSplits, nRows),evalSets,TRUE)
      if(!is.null(problem)) {
        stop(paste("problem with vtreat::buildEvalSets",problem))
      }
    }
  } else {
    # know 2*nSplits<=nRows
    if((!is.null(y))&&(max(y)>min(y))) {
      #  Try for full y-stratified k-way cross val
      evalSets <- kWayStratifiedY(nRows=nRows,nSplits=nSplits,dframe=NULL,y=y)
    } else {
      evalSets <- kWayCrossValidation(nRows=nRows,nSplits=nSplits,dframe=NULL,y=NULL)
    }
    problem <- problemAppPlan(nRows,nSplits,evalSets,TRUE)
    if(!is.null(problem)) {
      stop(paste("problem with vtreat::buildEvalSets",problem))
    }
  }
  evalSets
}



# make a "cross frame" that is a frame where each treated row was treated only 
# by a treatment plan not involving the given row
.mkCrossFrame <- function(dframe,
                          referenceTreatments,
                          varlist,newVarsS,outcomename,zoY,
                          zC,zTarget,
                          weights,
                          minFraction,smFactor,
                          rareCount,rareSig,
                          collarProb,
                          codeRestriction,
                          customCoders,
                          scale,doCollar,
                          splitFunction,nSplits,
                          catScaling,
                          ...,
                          parallelCluster = NULL,
                          use_parallel = TRUE,
                          verbose = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::.mkCrossFrame")
  dsub <- dframe[,c(varlist,outcomename),drop=FALSE]
  # build a carve-up plan
  evalSets <- buildEvalSets(length(zoY),dframe=dframe,y=zoY,
                            splitFunction=splitFunction,nSplits=nSplits)
  crossFrameList <- vector('list',length(evalSets))
  wtList <- vector('list',length(evalSets))
  rList <- vector('list',length(evalSets))
  foundCols <- c() # can include outcome
  for(ei in seq_len(length(evalSets))) {
    evalIndices <- evalSets[[ei]]$app
    buildIndices <- evalSets[[ei]]$train
    dsubiEval <- dsub[evalIndices, , drop=FALSE]
    dsubiBuild <- dsub[buildIndices, , drop=FALSE]
    zoYBuild <- zoY[buildIndices]
    zCBuild <- c()
    if(!is.null(zC)) {
      zCBuild <- zC[buildIndices]
    }
    wBuild <- weights[buildIndices]
    ti <- .designTreatmentsXS(
      dframe = dsubiBuild,
      varlist = varlist,
      outcomename = outcomename,
      zoY = zoYBuild,
      zC = zCBuild,
      zTarget = zTarget,
      weights = wBuild,
      minFraction = minFraction,
      smFactor = smFactor,
      rareCount = rareCount,
      rareSig = rareSig,
      collarProb = collarProb,
      codeRestriction = codeRestriction, 
      customCoders = customCoders,
      justWantTreatments = TRUE,
      catScaling = catScaling,
      verbose = verbose,
      parallelCluster = parallelCluster,
      use_parallel = use_parallel)
    fi <- .vtreatList(ti,dsubiEval,newVarsS,
                      scale = scale,
                      doCollar = doCollar,
                      parallelCluster = parallelCluster,
                      use_parallel = use_parallel)
    # fill in missing columns (a data leak potential, but a necessary step)
    droppedColumns <-  setdiff(newVarsS,c(outcomename,colnames(fi)))
    if(length(droppedColumns)>0) {
      repFrame <- NULL
      if(!is.null(referenceTreatments)) {
        repFrame <- prepare(referenceTreatments,dsubiEval,
                            pruneSig=NULL,
                            varRestriction=droppedColumns,
                            scale=scale,doCollar=doCollar,
                            parallelCluster=parallelCluster,
                            use_parallel = use_parallel)
      }
      for(v in droppedColumns) {
        fi[[v]] <- 0.0
        if((!is.null(repFrame)) && (v %in% colnames(repFrame))) {
          fi[[v]] <- repFrame[[v]]
        }
      }
    }
    # make sure each frame has the same column structure (again a data leak)
    fi[[outcomename]] <- dsubiEval[[outcomename]]
    if(ei<=1) {
      foundCols <- colnames(fi)
    } else {
      foundCols <- intersect(foundCols,colnames(fi))
    }
    crossFrameList[[ei]] <- fi
    wtList[[ei]] <- weights[evalIndices]
    rList[[ei]] <- evalIndices
  }
  # make sure each frame has the same column structure
  lostVars <- setdiff(newVarsS,foundCols)
  if(length(lostVars)>0) {
    warning(paste('cross frame procedures lost variables: ',
            paste(lostVars,collapse=', '),
            '(likely did not vary on one data carve-up)'))
  }
  crossFrameList <- lapply(crossFrameList,
                           function(fi) {
                             fi[,foundCols,drop=FALSE]
                           })
  # assemble frame
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
  list(crossFrame=crossFrame,crossWeights=scoreWeights,
       method=attr(evalSets,'splitmethod'),
       evalSets=evalSets,
       foundCols=foundCols,
       lostVars=lostVars)
}


