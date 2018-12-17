



.vtreatA <- function(vtreat,xcol,scale,doCollar) {
  dout <- as.data.frame(vtreat$f(xcol,vtreat$args,doCollar),
                        stringsAsFactors=FALSE)
  colnames(dout) <- vtreat$newvars
  if(scale) {
    for(j in seq_along(vtreat$scales$a)) {
      dout[[j]] <- dout[[j]]*vtreat$scales$a[[j]] + vtreat$scales$b[[j]]
    }
  }
  dout
}


mkVtreatListWorker <- function(scale,doCollar) {
  force(scale)
  force(doCollar)
  function(tpair) {
    ti <- tpair$ti
    xcolOrig <- tpair$xcolOrig
    nRows <- length(xcolOrig)
    xcolClean <- .cleanColumn(xcolOrig,nRows)
    if(is.null(xcolClean)) {
      return(paste('column',ti$origvar,
                   'is not a type/class vtreat can work with (',
                   paste(class(xcolOrig), collapse = " "),')'))
    }
    if(!is.null(ti$convertedColClass)) {
      curColClass <- paste(class(xcolClean), collapse = " ")
      if(curColClass!=ti$convertedColClass) {
        return(paste('column',ti$origvar,'expected to convert to ',
                     ti$convertedColClass,'saw',
                     paste(class(xcolOrig), collapse = " "), curColClass))
      }
    }
    .vtreatA(ti,xcolClean,scale,doCollar)
  }
}

# colNames a subset of treated variable names
.vtreatList <- function(treatments, dframe, colNames, 
                        scale, 
                        doCollar,
                        ...,
                        parallelCluster = NULL,
                        use_parallel = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), 
                          "vtreat:::.vtreatList")
  resCounts <- vapply(treatments,function(tij) { 
    length(intersect(colNames,tij$newvars))
  },numeric(1))
  toProcess <- treatments[resCounts>0]
  toProcessP <- lapply(toProcess,
                      function(ti) {
                        list(ti=ti,xcolOrig=dframe[[ti$origvar]])
                      })
  procWorker <- mkVtreatListWorker(scale,doCollar)
  gs <- plapply(toProcessP, procWorker,
                parallelCluster = parallelCluster,
                use_parallel = use_parallel)
  # pass back first error
  for(gi in gs) {
    if(is.character(gi)) {
      stop(gi)
    }
  }
  # unpack sub-frames into a list of columns
  cols <- vector('list',length(colNames))
  names(cols) <- colNames
  for(ii in seq_len(length(toProcess))) {
    ti <- toProcess[[ii]]
    gi <- gs[[ii]]
    wants <- intersect(colNames,ti$newvars)
    for(vi in wants) {
      cols[[vi]] <- gi[[vi]]
    }
  }
  cols <- Filter(Negate(is.null),cols)
  # corner case, make sure we get the number of rows correct
  if(length(cols)<=0) {
    d <- data.frame(x=numeric(nrow(dframe)),stringsAsFactors=FALSE)
    d[['x']] <- NULL
    return(d)
  }
  as.data.frame(cols,stringsAsFactors=FALSE)
}








# pre-transform categorical column
# convert it to character, convert NA to "NA"
.preProcCat <- function(col, levRestriction) {
  origna <- is.na(col)
  # don't use blank as a key and get into defendable level space
  col <- paste('x',as.character(col))
  col[origna] <- 'NA'
  if(!is.null(levRestriction)) {
    # map rare and novel levels to a new special level "rare"
    rares <- !(col %in% levRestriction$safeLevs)
    col[rares] <- 'rare'
    # remove any levels not eligable for the above treatment
    zaps <- col %in% levRestriction$supressedLevs
    col[zaps] <- 'zap'
  }
  col
}



.mkAOVWorkder <- function(yNumeric,vcol,weights) {
  force(yNumeric)
  force(vcol)
  force(weights)
  function(level) {
    # lm call here is okay, as (vcol==level) only has two possible values
    m <- stats::lm(yNumeric~(vcol==level),weights=weights)
    stats::anova(m)[1,'Pr(>F)']
  }
}

# determine non-rare and significant levels for numeric/regression target
# regression mode
.safeLevelsR <- function(vcolin, 
                         yNumeric, 
                         weights, 
                         minFraction,
                         rareCount, rareSig,
                         ...,
                         parallelCluster = NULL,
                         use_parallel = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), 
                          "vtreat:::.safeLevelsR")
  vcol <- .preProcCat(vcolin,c())
  # first: keep only levels with enough weighted counts
  counts <- tapply(weights,vcol,sum)
  totMass <- sum(counts)
  safeLevs <- names(counts)[counts>rareCount]
  supressedLevs <- character(0)
  # re-code with rare symbol eligable
  vcol <- .preProcCat(vcolin,list(safeLevs=safeLevs,supressedLevs=supressedLevs))
  counts <- tapply(weights,vcol,sum)
  totMass <- sum(counts)
  safeLevs <- names(counts)
  if((length(safeLevs)>0)&&(!is.null(rareSig))&&(rareSig<1)) {
    # second: keep only levels that look significantly different than grand mean
    aovCalc <-.mkAOVWorkder(yNumeric,vcol,weights)
    sigs <- as.numeric(plapply(safeLevs,aovCalc,
                               parallelCluster = parallelCluster,
                               use_parallel = use_parallel))
    supressedLevs <- safeLevs[sigs>rareSig]
  }
  tracked <- names(counts)[counts/totMass>=minFraction] # levels eligable for indicators
  tracked <- setdiff(tracked, supressedLevs)
  tracked <- setdiff(tracked,'zap') # don't let zap group code
  list(safeLevs = safeLevs, 
       supressedLevs = supressedLevs,
       tracked = tracked)
}


.mkCSigWorker <- function(zC,zTarget,vcol,weights) {
  force(zC)
  force(zTarget)
  force(vcol)
  force(weights)
  function(level) {
    #tab <- table(vcol==level,zC==zTarget)  # not weighted
    tab <- .wTable(vcol==level,zC==zTarget,weights)
    if((nrow(tab)<=1)||(ncol(tab)<=1)) {
      return(1.0)
    }
    # tests not quite interchangable, but roughly give us the sorting we want.
    if(min(tab)<=10) {
      stats::fisher.test(tab)$p.value
    } else {
      tryCatch(
        stats::chisq.test(tab)$p.value,
        warning=function(w) {
          stats::fisher.test(tab)$p.value
        }
      )
    }
  }
}

# determine non-rare and significant levels for numeric/regression target
# classification mode
.safeLevelsC <- function(vcolin,
                         zC,zTarget,
                         weights,
                         minFraction,
                         rareCount, rareSig,
                         ...,
                         parallelCluster = NULL,
                         use_parallel = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), 
                          "vtreat:::.safeLevelsC")
  vcol <- .preProcCat(vcolin,c())
  # first: keep only levels with enough weighted counts
  counts <- tapply(weights,vcol,sum)
  totMass <- sum(counts)
  safeLevs <- names(counts)[counts>rareCount]
  supressedLevs <- character(0)
  # re-code with rare symbol eligable
  vcol <- .preProcCat(vcolin,list(safeLevs=safeLevs,supressedLevs=supressedLevs))
  counts <- tapply(weights,vcol,sum)
  totMass <- sum(counts)
  safeLevs <- names(counts)
  if((length(safeLevs)>0)&&(!is.null(rareSig))&&(rareSig<1)) {
    # second: keep only levels that look significantly different than grand mean
    sigCalc <- .mkCSigWorker(zC,zTarget,vcol,weights)
    sigs <- as.numeric(plapply(safeLevs,sigCalc,
                               parallelCluster = parallelCluster,
                               use_parallel = use_parallel))
    supressedLevs <- safeLevs[sigs>rareSig]
  }
  tracked <- names(counts)[counts/totMass>=minFraction] # levels eligable for indicators
  tracked <- setdiff(tracked, supressedLevs)
  tracked <- setdiff(tracked,'zap') # don't let zap group code
  list(safeLevs = safeLevs, 
       supressedLevs = supressedLevs,
       tracked = tracked)
}


.varDesignerW <- function(argv,
                          zoY,
                          zC,
                          zTarget,
                          weights,
                          minFraction,
                          smFactor,
                          rareCount,
                          rareSig,
                          collarProb,
                          codeRestriction,
                          customCoders,
                          catScaling,
                          verbose,
                          nRows,
                          yMoves,
                          codeRestictionWasNULL) {
  v <- argv$v
  vcolOrig <- argv$vcolOrig
  vcol <- argv$vcol
  hasRange <- argv$hasRange
  levRestriction <- argv$levRestriction
  treatments <- list()
  acceptTreatment <- function(ti) {
    if(!is.null(ti)) {
      ti$origType <- typeof(vcolOrig)
      ti$origClass <- paste(class(vcolOrig), collapse = " ")
      ti$convertedColClass <- paste(class(vcol), collapse = " ")
      treatments[[length(treatments)+1]] <<- ti # Deliberate side-effect
    }
  }
  if(is.null(vcol)) {
    warning(paste('column',v,
                  'is not a type/class/value vtreat can work with (',
                  paste(class(vcolOrig), collapse= " "),')'))
  } else {
    colclass <- paste(class(vcol), collapse = " ")
    if(.has.range(vcol)) {
      ti <- NULL
      if((colclass=='numeric') || (colclass=='integer')) {
        for(customCode in names(customCoders)) {
          coder <- customCoders[[customCode]]
          customeCodeV <- base::strsplit(customCode, '.', fixed=TRUE)[[1]]
          codeType <- customeCodeV[[1]]
          codeName <- customeCodeV[[2]]
          if(codeRestictionWasNULL || (codeName %in% codeRestriction)) {
            codeSeq <- NULL
            if(length(customeCodeV)>2) {
              codeSeq <- customeCodeV[seq(3, length(customeCodeV))]
            }
            if('num' %in% codeSeq) {
              if((codeType=='n')==is.null(zC)) {
                ti <- makeCustomCoderNum(codeName, coder, codeSeq, 
                                         v,vcol,zoY,zC,zTarget,weights,catScaling)
                acceptTreatment(ti)
              }
            }
          }
        }
        ti = NULL
        if(codeRestictionWasNULL || ('clean' %in% codeRestriction)) {
          ti <- .mkPassThrough(v,vcol,zoY,zC,zTarget,weights,collarProb,catScaling)
          acceptTreatment(ti)
        }
        if(codeRestictionWasNULL || ('isBAD' %in% codeRestriction)) {
          ti <- .mkIsBAD(v,vcol,zoY,zC,zTarget,weights,catScaling)
          acceptTreatment(ti)
        }
      } else if((colclass=='character') || (colclass=='factor')) {
        # expect character or factor here
        for(customCode in names(customCoders)) {
          coder <- customCoders[[customCode]]
          customeCodeV <- base::strsplit(customCode, '.', fixed=TRUE)[[1]]
          codeType <- customeCodeV[[1]]
          codeName <- customeCodeV[[2]]
          if(codeRestictionWasNULL || (codeName %in% codeRestriction)) {
            codeSeq <- NULL
            if(length(customeCodeV)>2) {
              codeSeq <- customeCodeV[seq(3, length(customeCodeV))]
            }
            if(!('num' %in% codeSeq)) {
              if((codeType=='n')==is.null(zC)) {
                ti <- makeCustomCoder(codeName, coder, codeSeq, 
                                      v,vcol,zoY,zC,zTarget,weights,catScaling)
                acceptTreatment(ti)
              }
            }
          }
        }
        ti = NULL
        if(length(levRestriction$safeLevs)>0) {
          if(codeRestictionWasNULL || ('lev' %in% codeRestriction)) {
            ti <- .mkCatInd_a(v,vcol,zoY,zC,zTarget,minFraction,levRestriction,weights,catScaling)
            if( (!is.null(ti)) && (length(ti$newvars)>0) ) {
              acceptTreatment(ti)
            }
          }
          if(is.null(ti)||(length(unique(vcol))>2)) {  # make an impactmodel if catInd construction failed or there are more than 2 levels
            if(codeRestictionWasNULL || ('catP' %in% codeRestriction)) {
              ti <- .mkCatP(v,vcol,zoY,zC,zTarget,levRestriction,weights,catScaling)
              acceptTreatment(ti)
            }
            if(yMoves) {
              if(!is.null(zC)) {  # in categorical mode
                if(codeRestictionWasNULL || ('catB' %in% codeRestriction)) {
                  ti <- .mkCatBayes(v,vcol,zC,zTarget,smFactor,levRestriction,weights,catScaling)
                  acceptTreatment(ti)
                }
              }
              if(is.null(zC)) { # is numeric mode
                if(codeRestictionWasNULL || ('catN' %in% codeRestriction)) {
                  ti <- .mkCatNum(v,vcol,zoY,smFactor,levRestriction,weights)
                  acceptTreatment(ti)
                }
                if(codeRestictionWasNULL || ('catD' %in% codeRestriction)) {
                  ti <- .mkCatD(v,vcol,zoY,smFactor,levRestriction,weights)
                  acceptTreatment(ti)
                }
              }
            }
          }
        }
      } else {
        warning(paste('variable',v,'has unexpected class:',colclass,
                      ', skipping, (want one of numeric,integer,character,factor)'))
      }  
    }
  }
  treatments
}

# design a treatment for a single variable
# bind a bunch of variables, so we pass exactly what we need to sub-processes
.mkVarDesigner <- function(zoY,
                           zC,zTarget,
                           weights,
                           minFraction,smFactor,rareCount,rareSig,
                           collarProb,
                           codeRestriction, customCoders,
                           catScaling,
                           verbose) {
  force(zoY)
  force(zC)
  force(zTarget)
  force(weights)
  force(minFraction)
  force(smFactor)
  force(rareCount)
  force(rareSig)
  force(collarProb)
  force(codeRestriction)
  force(customCoders)
  force(catScaling)
  force(verbose)
  nRows = length(zoY)
  yMoves <- .has.range.cn(zoY)
  # NULL is an alias for "don't restrict"
  codeRestictionWasNULL <- length(codeRestriction)<=0
  
  function(argv) {
    .varDesignerW(argv = argv,
                  zoY = zoY,
                  zC = zC,
                  zTarget = zTarget,
                  weights = weights,
                  minFraction = minFraction,
                  smFactor = smFactor,
                  rareCount = rareCount,
                  rareSig = rareSig,
                  collarProb = collarProb,
                  codeRestriction = codeRestriction,
                  customCoders = customCoders,
                  catScaling = catScaling,
                  verbose = verbose,
                  nRows = nRows,
                  yMoves = yMoves,
                  codeRestictionWasNULL = codeRestictionWasNULL)
  }
}



.neatenScoreFrame <- function(sFrame) {
  # clean up sFrame a bit
  if(nrow(sFrame)>0) {
     sFrame[['sig']][.is.bad(sFrame[['sig']])] <- 1
  }
  sFrame
}


.scoreCol <- function(varName,nxcol,zoY,zC,zTarget,weights,
                      extraModelDegrees=0) {
  rsq <- 0.0
  sig <- 1.0
  catTarget <- !is.null(zC)
  varMoves <- .has.range.cn(nxcol)
  if(varMoves) {
    yMoves <- .has.range.cn(zoY)
    if(varMoves && yMoves) {
      lstat <- linScore(varName,
                        nxcol,
                        zoY,
                        weights,
                        extraModelDegrees)
      sig <- lstat$sig
      rsq <- lstat$rsq
      if(catTarget) {
        cstat <- catScore(varName,
                          nxcol,
                          zC,zTarget,
                          weights,
                          extraModelDegrees)
        sig <- cstat$sig
        rsq <- cstat$rsq
      }
    }
  }
  scoreFrameij <- data.frame(varName=varName,
                             varMoves=varMoves,
                             rsq=rsq,
                             sig=sig,
                             stringsAsFactors = FALSE)
  .neatenScoreFrame(scoreFrameij)
}



# used in initial scoring of variables
.mkScoreVarWorker <- function(nRows,zoY,zC,zTarget,weights) {
  force(nRows)
  force(zoY)
  force(zC)
  force(zTarget)
  force(weights)
  function(tpair) {
    ti <- tpair$ti
    dfcol <- tpair$dfcol
    origName <- vorig(ti)
    xcolClean <- .cleanColumn(dfcol,nRows)
    fi <- .vtreatA(ti,xcolClean,FALSE,FALSE)
    scoreFrame <- lapply(seq_len(length(vnames(ti))),
                         function(nvi) {
                           nv <- vnames(ti)[[nvi]]
                           .scoreCol(nv,fi[[nv]],zoY,zC,zTarget,weights,
                                     ti$extraModelDegrees) 
                          })
    scoreFrame <- Filter(Negate(is.null),scoreFrame)
    if(length(scoreFrame)<=0) {
      return(NULL)
    }
    sFrame <- .rbindListOfFrames(scoreFrame)
    sFrame$needsSplit <- ti$needsSplit
    sFrame$extraModelDegrees <- ti$extraModelDegrees
    sFrame$origName <- origName
    sFrame$code <- ti$treatmentCode
    sFrame
  }
}


# used in re-scoring needsSplit variables on simulated out of sample
# (cross) frame
.mkScoreColWorker <- function(zoY,zC,zTarget,weights) {
  force(zoY)
  force(zC)
  force(zTarget)
  force(weights)
  function(nvpair) {
    nv <- nvpair$nv
    dfc <- nvpair$dfc
    scoreFrameij <- .scoreCol(nv,dfc,zoY,zC,zTarget,weights)
    scoreFrameij
  }
}






# build all treatments for a data frame to predict a given outcome
.designTreatmentsXS <- function(
  ...,
  dframe, 
  varlist, 
  outcomename, 
  zoY,
  zC,
  zTarget,
  weights,
  minFraction, 
  smFactor,
  rareCount,
  rareSig,
  collarProb,
  codeRestriction, 
  customCoders,
  justWantTreatments,
  catScaling,
  verbose = FALSE,
  parallelCluster = NULL,
  use_parallel = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), 
                          "vtreat:::.designTreatmentsXS")
  if(verbose) {
    print(paste("designing treatments",date()))
  }
  nRows = length(zoY)
  # In building the workList don't transform any variables (such as making
  # row selections), only select columns out of frame.  This prevents
  # data growth prior to doing the work.
  workList <- lapply(varlist,
                     function(v) {
                       levRestriction <- c()
                       hasRange <- FALSE
                       vcolOrig <- dframe[[v]]
                       vcol <- NULL
                       if(length(vcolOrig)!=nRows) {
                         warning(paste("wrong column length",v))
                       } else {
                         vcol <- .cleanColumn(vcolOrig,nRows)
                         if(.has.range(vcol)) {
                           hasRange <- TRUE
                           colclass <- paste(class(vcol), collapse = " ")
                           if((colclass=='character') || (colclass=='factor')) {
                             # expect character or factor here
                             if(!is.null(zC)) {  # in categorical mode
                               levRestriction <- .safeLevelsC(vcol, zC, zTarget,
                                                              weights,
                                                              minFraction,
                                                              rareCount, rareSig,
                                                              parallelCluster = parallelCluster,
                                                              use_parallel = use_parallel)
                             } else {
                               levRestriction <- .safeLevelsR(vcol, zoY,
                                                              weights,
                                                              minFraction,
                                                              rareCount,rareSig,
                                                              parallelCluster = parallelCluster,
                                                              use_parallel = use_parallel)
                             }
                           }
                         }
                       }
                       list(v=v,
                            vcolOrig=vcolOrig,
                            vcol=vcol,
                            hasRange=hasRange,
                            levRestriction=levRestriction
                       )})
  workList <- Filter(function(wi) {wi$hasRange}, workList)
  if(verbose) {
    print(paste(" have initial level statistics", date()))
  }
  # build the treatments we will return to the user
  worker <- .mkVarDesigner(zoY,
                           zC,zTarget,
                           weights,
                           minFraction,smFactor,rareCount,rareSig,
                           collarProb,
                           codeRestriction, customCoders,
                           catScaling,
                           verbose)
  treatments <- plapply(workList, worker,
                        parallelCluster = parallelCluster,
                        use_parallel = use_parallel)
  treatments <- unlist(treatments, recursive=FALSE)
  treatments <- Filter(Negate(is.null), treatments)
  # parallelize on levels for cat ind
  for(i in seq_len(length(treatments))) {
    ti <- treatments[[i]]
    if(ti$treatmentCode == 'lev') {
      ti <- .mkCatInd_scales(ti,
                             zoY, zC, zTarget,
                             weights, catScaling,
                             parallelCluster = parallelCluster,
                             use_parallel = use_parallel)
      treatments[[i]] <- ti;
    }
  }
  if(justWantTreatments) {
    return(treatments)
  }
  if(length(treatments)<=0) {
    stop('no usable vars')
  }
  # score variables
  if(verbose) {
    print(paste(" scoring treatments",date()))
  }
  is_ind <- vapply(treatments,
                   function(ti) { ti$treatmentCode == "lev"},
                   logical(1))
  treatments_ind <- treatments[is_ind]
  treatments_non_ind <- treatments[!is_ind]
  sFrame <- NULL
  if(length(treatments_non_ind)>0) {
    scrW <- .mkScoreVarWorker(nrow(dframe),zoY,zC,zTarget,weights)
    tP <- lapply(treatments_non_ind,
                 function(ti) {
                   list(ti=ti,
                        dfcol=dframe[[vorig(ti)]])
                 })
    sFrames <- plapply(tP, scrW,
                       parallelCluster = parallelCluster,
                       use_parallel = use_parallel)
    sFrames <- Filter(Negate(is.null), sFrames)
    sFrame <- .rbindListOfFrames(sFrames)
  }
  # Finer grain parallelism on indicators (more like .mkScoreColWorker )
  if(length(treatments_ind)>0) {
    swkr <- .mkScoreColWorker(zoY, zC, zTarget, weights)
    sframel <- vector(length(treatments_ind), mode = "list")
    for(ii in seq_len(length(treatments_ind))) {
      ti <- treatments_ind[[ii]]
      origName <- ti$origvar
      dfcol <- dframe[[origName]]
      xcolClean <- .cleanColumn(dfcol, nRows)
      fi <- .vtreatA(ti, xcolClean, FALSE, FALSE)
      newVarsSP <- lapply(ti$newvars,
                          function(nv) {
                            list(nv = nv, dfc = fi[[nv]])
                          })
      sfij <- plapply(newVarsSP, swkr,
                      parallelCluster = parallelCluster,
                      use_parallel = use_parallel)
      sfij <-  .rbindListOfFrames(sfij)
      sfij$needsSplit <- FALSE
      sfij$extraModelDegrees <- 0
      sfij$origName <- origName
      sfij$code <- ti$treatmentCode
      sframel[[ii]] <- sfij
    }
    sframel <- .rbindListOfFrames(sframel)
    sframel <- Filter(Negate(is.null), sframel)
    siFrame <- .rbindListOfFrames(sframel)
    sFrame <- rbind(sFrame, siFrame)
  }
  plan <- list(treatments=treatments,
               scoreFrame=sFrame,
               outcomename=outcomename)
  class(plan) <- 'treatmentplan'
  if(verbose) {
    print(paste("have treatment plan",date()))
  }
  plan
}






# build all treatments for a data frame to predict a given outcome
.designTreatmentsX <- function(
  ...,
  dframe,
  varlist,
  outcomename,
  zoY,
  zC,
  zTarget,
  weights,
  minFraction,
  smFactor,
  rareCount,
  rareSig,
  collarProb,
  codeRestriction, 
  customCoders,
  splitFunction, 
  ncross, 
  forceSplit,
  catScaling,
  verbose = FALSE,
  parallelCluster = NULL,
  use_parallel = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), 
                          "vtreat:::.designTreatmentsX")
  if(!is.data.frame(dframe)) {
    stop("dframe must be a data frame")
  }
  if(collarProb>=0.5) {
    stop("collarProb must be < 0.5")
  }
  if(nrow(dframe)<1) {
    stop("most have rows")
  }
  if(verbose) {
    print(paste("vtreat", 
                packageVersion("vtreat"),
                "inspecting inputs", date()))
  }
  varlist <- setdiff(unique(varlist),outcomename)
  varlist <- intersect(varlist,colnames(dframe))
  varlist <- as.character(varlist)
  if(is.null(weights)) {
    weights <- rep(1.0,nrow(dframe))
  } else {
    if(!is.numeric(weights)) {
      stop("weights need to be numeric")
    }
    if(length(weights)!=nrow(dframe)) {
      stop("must have same number of weights as data frame rows")
    }
    goodPosns <- ifelse(.is.bad(weights),FALSE,weights>0.0)
    dframe <- dframe[goodPosns,,drop=FALSE]
    zoY <- zoY[goodPosns]
    weights <- weights[goodPosns]
    if(!is.null(zC)) {
      zC <- zC[goodPosns]
    }
    # the select goodPosns is duplicating the data frame, so it does cost
    # memory
  }
  if(nrow(dframe)<=0) {
    stop("no rows")
  }
  if(min(weights)<0) {
    stop("negative weights")
  }
  if(sum(weights)<=0) {
    stop("no non-zero weighted rows")
  }
  if(sum(.is.bad(zoY))>0) {
    stop("outcome variable had NAs")
  }
  if(rareCount<0) {
    stop("rarecount must not be negative")
  }
  treatments <- .designTreatmentsXS(
    dframe = dframe,
    varlist = varlist,
    outcomename = outcomename,
    zoY = zoY,
    zC = zC,
    zTarget = zTarget,
    weights = weights,
    minFraction = minFraction,
    smFactor = smFactor,
    rareCount = rareCount,
    rareSig = rareSig,
    collarProb = collarProb,
    codeRestriction = codeRestriction, 
    customCoders = customCoders,
    justWantTreatments = FALSE,
    catScaling = catScaling,
    verbose = verbose,
    parallelCluster = parallelCluster,
    use_parallel = use_parallel)
  treatments$scoreFrame <- treatments$scoreFrame[treatments$scoreFrame$varMoves,]
  if(forceSplit) {
    treatments$scoreFrame$needsSplit <- TRUE
  }
  treatments$vtreatVersion <- packageVersion('vtreat')
  treatments$outcomeType <- 'notmarked'
  treatments$outcomeTarget <- outcomename
  treatments$meanY <- NA
  yMoves <- .has.range.cn(zoY)
  crossMethod = 'Notcross'
  if(yMoves) {
    splitVars <- unique(treatments$scoreFrame$origName[treatments$scoreFrame$needsSplit])
    if(length(splitVars)>0) {
      newVarsS <- treatments$scoreFrame$varName[treatments$scoreFrame$needsSplit &
                                                  treatments$scoreFrame$varMoves]
      if(verbose) {
        print(paste("rescoring complex variables",date()))
      }
      crossData <- .mkCrossFrame(
        dframe = dframe,
        referenceTreatments = treatments,
        varlist = splitVars,
        newVarsS = newVarsS,
        outcomename = outcomename,
        zoY = zoY,
        zC = zC,
        zTarget = zTarget,
        weights = weights,
        minFraction = minFraction,
        smFactor = smFactor,
        rareCount = rareCount,
        rareSig = rareSig,
        collarProb = collarProb,
        codeRestriction = codeRestriction,
        customCoders = customCoders,
        scale = FALSE, 
        doCollar = FALSE,
        splitFunction = splitFunction,
        nSplits = ncross,
        catScaling = catScaling,
        parallelCluster = parallelCluster,
        use_parallel = use_parallel,
        verbose = FALSE)
      crossFrame <- crossData$crossFrame
      crossWeights <- crossData$crossWeights
      crossMethod <- crossData$method
      # score this frame
      if(is.null(zC)) {
        zoYS = crossFrame[[outcomename]]
        zCS = NULL
      } else {
        zCS = crossFrame[[outcomename]]==zTarget
        zoYS = ifelse(zCS,1,0)
      }
      swkr <- .mkScoreColWorker(zoYS,zCS,TRUE,crossWeights)
      newVarsSP <- lapply(newVarsS,
                          function(nv) {
                            list(nv=nv,dfc=crossFrame[[nv]])
                          })
      sframe <- plapply(newVarsSP,swkr,
                        parallelCluster = parallelCluster,
                        use_parallel = use_parallel) 
      sframe <- Filter(Negate(is.null),sframe)
      sframe <- .rbindListOfFrames(sframe)
      # overlay these results into treatments$scoreFrame
      nukeCols <- intersect(colnames(treatments$scoreFrame),
                            c('sig','rsq'))
      for(v in newVarsS) {
        for(n in nukeCols) {
          if(v %in% sframe$varName) {
            treatments$scoreFrame[[n]][treatments$scoreFrame$varName==v] <- 
              sframe[[n]][sframe==v]
          } else {
            treatments$scoreFrame[[n]][treatments$scoreFrame$varName==v] <- NA
          }
        }
      }
      # clean up sFrame a bit
      treatments$scoreFrame <- .neatenScoreFrame(treatments$scoreFrame)
      if(verbose) {
        print(paste("done rescoring complex variables",date()))
      }
    }
  }
  treatments$splitmethod <- crossMethod
  treatments$meanY <- .wmean(zoY,weights)
  treatments
}

.checkArgs1 <- function(dframe, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat checkargs")
  if(missing(dframe)||(!is.data.frame(dframe))||(nrow(dframe)<0)||(ncol(dframe)<=0)) {
    stop("dframe must be a non-empty data frame")
  }
}

.checkArgs <- function(dframe, varlist, outcomename, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat checkargs")
  if(missing(dframe)||(!is.data.frame(dframe))||
     (nrow(dframe)<0)||(ncol(dframe)<=0)) {
    stop("dframe must be a non-empty data frame")
  }
  if(missing(varlist)) {
    stop("required argument varlist missing")
  }
  if((!is.character(varlist))||(length(varlist)<1)) {
    stop("varlist must be a non-empty character vector")
  }
  if(length(varlist)!=length(unique(varlist))) {
    stop("duplicate variable name in varlist")
  }
  # designTreatmentsZ calls this, so outcomename may not be in dframe
  if(missing(outcomename)||
     (!is.character(outcomename))||(length(outcomename)!=1)) {
    stop("outcomename must be a length 1 character vector")
  }
  varlist <- setdiff(varlist, outcomename)
  varlist <- intersect(varlist, colnames(dframe))
  if(length(varlist)<1) {
    stop("varlist must include non-outcome column names")
  }
  if(sum(colnames(dframe) %in% varlist)!=length(varlist)) {
    stop("ambigous (duplicate) column name in data frame")
  }
}

