# build sets for out of sample evaluatin, train on complement of eval
# ensure y varies training set (complement of eval set)
.buildEvalSets <- function(zoY) {
  nRows = length(zoY)
  # build a partition plan
  evalSets <- list()
  if(nRows<=1) {
    return(evalSets) # no plan possible
  }
  if(nRows<=100) {
    # small case, 1-holdout Jackknife style
    evalSets <- as.list(seq_len(nRows))
    return(evalSets)
  }
  #  Try for full k-way cross val
  ncross <- 3
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
  evalSets <- .buildEvalSets(zoY)
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
                              impactOnly,
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

