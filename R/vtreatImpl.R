



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

# colNames a subset of treated variable names
.vtreatList <- function(treatments,dframe,colNames,scale,doCollar,
                        parallelCluster) {
  resCounts <- vapply(treatments,function(tij) { 
    length(intersect(colNames,tij$newvars))
  },numeric(1))
  toProcess <- treatments[resCounts>0]
  procWorker <- function(ti) {
    xcolOrig <- dframe[[ti$origvar]]
    nRows <- length(xcolOrig)
    xcolClean <- .cleanColumn(xcolOrig,nRows)
    if(is.null(xcolClean)) {
      return(paste('column',ti$origvar,
                   'is not a type/class vtreat can work with (',class(xcolOrig),')'))
    }
    if(!is.null(ti$convertedColClass)) {
      curColClass <- paste(class(xcolClean),collapse='')
      if(curColClass!=ti$convertedColClass) {
        return(paste('column',ti$origvar,'expected to convert to ',
                     ti$convertedColClass,'saw',class(xcolOrig),curColClass))
      }
    }
    .vtreatA(ti,xcolClean,scale,doCollar)
  }
  gs <- plapply(toProcess,procWorker,parallelCluster)
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
.preProcCat <- function(col,levRestriction) {
  origna <- is.na(col)
  # don't use blank as a key and get into defendable level space
  col <- paste('x',as.character(col))
  col[origna] <- 'NA'
  if(!is.null(levRestriction)) {
    # map rare levels to a new special level
    rares <- !(col %in% levRestriction$safeLevs)
    col[rares] <- 'rare'
    zaps <- col %in% levRestriction$supressedLevs
    col[zaps] <- 'zap'
  }
  col
}


# determine non-rare and significant levels for numeric/regression target
# regression mode
.safeLevelsR <- function(vcolin,yNumeric,weights,rareCount,rareSig) {
  vcol <- .preProcCat(vcolin,c())
  # first: keep only levels with enough weighted counts
  counts <- tapply(weights,vcol,sum)
  safeLevs <- names(counts)[(counts>rareCount) & (counts<(sum(weights)-rareCount))]
  supressedLevs <- character(0)
  # re-code with rare symbol eligable
  vcol <- .preProcCat(vcolin,list(safeLevs=safeLevs,supressedLevs=supressedLevs))
  counts <- tapply(weights,vcol,sum)
  safeLevs <- names(counts)[(counts>rareCount) & (counts<(sum(weights)-rareCount))]
  if((length(safeLevs)>0)&&(!is.null(rareSig))&&(rareSig<1)) {
    # second: keep only levels that look significantly different than grand mean
    aovCalc <- function(level) {
      m <- stats::lm(yNumeric~vcol==level,weights = weights)
      stats::anova(m)[1,'Pr(>F)']
    }
    sigs <- vapply(safeLevs,aovCalc,numeric(1))
    supressedLevs <- safeLevs[sigs>rareSig]
  }
  list(safeLevs=safeLevs,supressedLevs=supressedLevs)
}


# determine non-rare and significant levels for numeric/regression target
# classification mode
.safeLevelsC <- function(vcolin,zC,zTarget,weights,rareCount,rareSig) {
  vcol <- .preProcCat(vcolin,c())
  # first: keep only levels with enough weighted counts
  counts <- tapply(weights,vcol,sum)
  safeLevs <- names(counts)[(counts>rareCount) & (counts<(sum(weights)-rareCount))]
  supressedLevs <- character(0)
  # re-code with rare symbol eligable
  vcol <- .preProcCat(vcolin,list(safeLevs=safeLevs,supressedLevs=supressedLevs))
  counts <- tapply(weights,vcol,sum)
  safeLevs <- names(counts)[(counts>rareCount) & (counts<(sum(weights)-rareCount))]
  if((length(safeLevs)>0)&&(!is.null(rareSig))&&(rareSig<1)) {
    # second: keep only levels that look significantly different than grand mean
    sigCalc <- function(level) {
      #tab <- table(vcol==level,zC==zTarget)  # not weighted
      tab <- .wTable(vcol==level,zC==zTarget,weights)
      if((nrow(tab)<=1)||(ncol(tab)<=1)) {
        return(1.0)
      }
      stats::fisher.test(tab)$p.value
    }
    sigs <- vapply(safeLevs,sigCalc,numeric(1))
    supressedLevs <- safeLevs[sigs>rareSig]
  }
  list(safeLevs=safeLevs,supressedLevs=supressedLevs)
}




# design a treatment for a single variables
# bind a bunch of variables, so we pass exactly what we need to sub-processes
# TODO: pivot warnings/print out of here
.varDesigner <- function(zoY,
                         zC,zTarget,
                         weights,
                         minFraction,smFactor,rareCount,rareSig,
                         collarProb,
                         trainRows,origRowCount,
                         impactOnly,
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
  force(trainRows)
  force(origRowCount)
  force(verbose)
  nRows = length(zoY)
  yMoves <- .has.range.cn(zoY)
  function(argpair) {
    v <- argpair$v
    vcolOrig <- argpair$vcolOrig
    if(verbose) {
      print(paste('design var',v,date()))
    }
    treatments <- list()
    vcol <- .cleanColumn(vcolOrig,origRowCount)[trainRows]
    if(length(vcol)!=nRows) {
      warning("wrong column length")
      vcol <- NULL
    }
    acceptTreatment <- function(ti) {
      if(!is.null(ti)) {
        ti$origType <- typeof(vcolOrig)
        ti$origClass <- paste(class(vcolOrig),collapse='')
        ti$convertedColClass <- paste(class(vcol),collapse='')
        treatments[[length(treatments)+1]] <<- ti # Deliberate side-effect
      }
    }
    if(is.null(vcol)) {
      warning(paste('column',v,'is not a type/class/value vtreat can work with (',class(vcolOrig),')'))
    } else {
      colclass <- class(vcol)
      if(.has.range(vcol)) {
        if((colclass=='numeric') || (colclass=='integer')) {
          if(!impactOnly) {
            ti <- .mkPassThrough(v,vcol,zoY,weights,collarProb)
            acceptTreatment(ti)
            ti <- .mkIsBAD(v,vcol,zoY,weights)
            acceptTreatment(ti)
          }
        } else if((colclass=='character') || (colclass=='factor')) {
          # expect character or factor here
          if(!is.null(zC)) {  # in categorical mode
            levRestriction <- .safeLevelsC(vcol,zC,zTarget,weights,rareCount,rareSig)
          } else {
            levRestriction <- .safeLevelsR(vcol,zoY,weights,rareCount,rareSig)
          }
          if(length(levRestriction$safeLevs)>0) {
            ti = NULL
            if(!impactOnly) {
              ti <- .mkCatInd(v,vcol,zoY,minFraction,levRestriction,weights)
              acceptTreatment(ti)
            }
            if(is.null(ti)||(length(unique(vcol))>2)) {  # make an impactmodel if catInd construction failed or there are more than 2 levels
              ti <- .mkCatP(v,vcol,zoY,zC,zTarget,levRestriction,weights)
              acceptTreatment(ti)
              if(yMoves) {
                if(!is.null(zC)) {  # in categorical mode
                  ti <- .mkCatBayes(v,vcol,zC,zTarget,smFactor,levRestriction,weights)
                  acceptTreatment(ti)      
                }
                if(is.null(zC)) { # is numeric mode
                  ti <- .mkCatNum(v,vcol,zoY,smFactor,levRestriction,weights)
                  acceptTreatment(ti)
                  ti <- .mkCatD(v,vcol,zoY,smFactor,levRestriction,weights)
                  acceptTreatment(ti)
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
}



.neatenScoreFrame <- function(sFrame) {
  # clean up sFrame a bit
  if(nrow(sFrame)>0) {
    for(cname in c('psig','csig','sig')) {
      if(cname %in% colnames(sFrame)) {
        sFrame[[cname]][.is.bad(sFrame[[cname]])] <- 1
      }
    }
  }
  sFrame
}


.scoreCol <- function(varName,nxcol,zoY,zC,zTarget,weights) {
  psig=1.0
  sig=1.0
  csig=1.0
  catTarget <- !is.null(zC)
  varMoves <- .has.range.cn(nxcol)
  if(varMoves) {
    yMoves <- .has.range.cn(zoY)
    if(varMoves && yMoves) {
      # TODO: pull this off scale fact
      lstat <- linScore(varName,
                        nxcol,
                        zoY,
                        weights)
      psig <- lstat$sig
      sig <- lstat$sig
      if(catTarget) {
        cstat <- catScore(varName,
                          nxcol,
                          zC,zTarget,
                          weights)
        csig <- cstat$sig
        sig <- cstat$sig
      }
    }
  }
  scoreFrameij <- data.frame(varName=varName,
                             varMoves=varMoves,
                             psig=psig,
                             sig=psig,
                             stringsAsFactors = FALSE)
  if(catTarget) {
    scoreFrameij$csig <- csig
    scoreFrameij$sig <- csig
  }
  .neatenScoreFrame(scoreFrameij)
}



# used in initial scoring of variables
.mkScoreVarWorker <- function(dframe,zoY,zC,zTarget,weights) {
  force(dframe)
  force(zoY)
  force(zC)
  force(zTarget)
  force(weights)
  nRows <- nrow(dframe)
  function(ti) {
    origName <- vorig(ti)
    xcolClean <- .cleanColumn(dframe[[origName]],nRows)
    fi <- .vtreatA(ti,xcolClean,FALSE,FALSE)
    scoreFrame <- lapply(seq_len(length(vnames(ti))),
                         function(nvi) {
                           nv <- vnames(ti)[[nvi]]
                           .scoreCol(nv,fi[[nv]],zoY,zC,zTarget,weights) 
                          })
    scoreFrame <- Filter(Negate(is.null),scoreFrame)
    if(length(scoreFrame)<=0) {
      return(NULL)
    }
    sFrame <- .rbindListOfFrames(scoreFrame)
    sFrame$needsSplit <- ti$needsSplit
    sFrame$origName <- origName
    sFrame$code <- ti$treatmentCode
    sFrame
  }
}


# used in re-scoring needsSplit variables on simulated out of sample
# (cross) frame
.mkScoreColWorker <- function(dframe,zoY,zC,zTarget,weights) {
  force(dframe)
  force(zoY)
  force(zC)
  force(zTarget)
  force(weights)
  function(nv) {
    scoreFrameij <- .scoreCol(nv,dframe[[nv]],zoY,zC,zTarget,weights)
    scoreFrameij
  }
}






# build all treatments for a data frame to predict a given outcome
.designTreatmentsXS <- function(dframe,varlist,outcomename,zoY,
                                zC,zTarget,
                                weights,
                                minFraction,smFactor,
                                rareCount,rareSig,
                                collarProb,
                                impactOnly,justWantTreatments,
                                verbose,
                                parallelCluster) {
  # In building the workList don't transform any variables (such as making
  # row selections), only select columns out of frame.  This prevents
  # data growth prior to doing the work.
  workList <- lapply(varlist,function(v) {list(v=v,vcolOrig=dframe[[v]])})
  # build the treatments we will return to the user
  worker <- .varDesigner(zoY,
                         zC,zTarget,
                         weights,
                         minFraction,smFactor,rareCount,rareSig,
                         collarProb,
                         seq_len(nrow(dframe)),nrow(dframe),
                         impactOnly,
                         verbose)
  treatments <- plapply(workList,worker,parallelCluster)
  treatments <- Filter(Negate(is.null),treatments)
  treatments <- unlist(treatments,recursive=FALSE)
  if(justWantTreatments) {
    return(treatments)
  }
  if(length(treatments)<=0) {
    stop('no usable vars')
  }
  # score variables
  if(verbose) {
    print(paste("scoring treatments",date()))
  }
  scrW <- .mkScoreVarWorker(dframe,zoY,zC,zTarget,weights)
  sFrames <- plapply(treatments,scrW,parallelCluster)
  sFrames <- Filter(Negate(is.null),sFrames)
  sFrame <- .rbindListOfFrames(sFrames)
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
.designTreatmentsX <- function(dframe,varlist,outcomename,zoY,
                               zC,zTarget,
                               weights,
                               minFraction,smFactor,
                               rareCount,rareSig,
                               collarProb,
                               ncross,
                               verbose,
                               parallelCluster) {
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
    print(paste("desigining treatments",date()))
  }
  varlist <- setdiff(varlist,outcomename)
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
  treatments <- .designTreatmentsXS(dframe,varlist,outcomename,zoY,
                                    zC,zTarget,
                                    weights,
                                    minFraction,smFactor,
                                    rareCount,rareSig,
                                    collarProb,
                                    FALSE,FALSE,
                                    verbose,
                                    parallelCluster)
  treatments$scoreFrame <- treatments$scoreFrame[treatments$scoreFrame$varMoves,]
  yMoves <- .has.range.cn(zoY)
  if(yMoves) {
    splitVars <- unique(treatments$scoreFrame$origName[treatments$scoreFrame$needsSplit])
    if(length(splitVars)>0) {
      newVarsS <- treatments$scoreFrame$varName[treatments$scoreFrame$needsSplit &
                                                  treatments$scoreFrame$varMoves]
      if(verbose) {
        print(paste("rescoring complex variables",date()))
      }
      crossData <- .mkCrossFrame(dframe,splitVars,newVarsS,outcomename,zoY,
                                zC,zTarget,
                                weights,
                                minFraction,smFactor,
                                rareCount,rareSig,
                                collarProb,
                                TRUE,
                                FALSE,FALSE,
                                ncross,
                                parallelCluster)
      crossFrame <- crossData$crossFrame
      crossWeights <- crossData$crossWeights
      # score this frame
      if(is.null(zC)) {
        zoYS = crossFrame[[outcomename]]
        zCS = NULL
      } else {
        zCS = crossFrame[[outcomename]]==zTarget
        zoYS = ifelse(zCS,1,0)
      }
      swkr <- .mkScoreColWorker(crossFrame,zoYS,zCS,TRUE,crossWeights)
      sframe <- plapply(newVarsS,swkr,parallelCluster) 
      sframe <- Filter(Negate(is.null),sframe)
      sframe <- .rbindListOfFrames(sframe)
      # overlay these results into treatments$scoreFrame
      nukeCols <- intersect(colnames(treatments$scoreFrame),
                            c('psig', 'sig', 'csig'))
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
  treatments
}

.checkArgs1 <- function(dframe,...) {
  args <- list(...)
  if(length(args)!=0) {
    nm <- setdiff(paste(names(args),collapse=", "),'')
    nv <- length(args)-length(nm)
    stop(paste("unexpected arguments",nm,"(and",nv,"unexpected values)"))
  }
  if(missing(dframe)||(!is.data.frame(dframe))||(nrow(dframe)<0)||(ncol(dframe)<=0)) {
    stop("dframe must be a non-empty data frame")
  }
}

.checkArgs <- function(dframe,varlist,outcomename,...) {
  args <- list(...)
  if(length(args)!=0) {
    nm <- setdiff(paste(names(args),collapse=", "),'')
    nv <- length(args)-length(nm)
    stop(paste("unexpected arguments",nm,"(and",nv,"unexpected values)"))
  }
  if(missing(dframe)||(!is.data.frame(dframe))||(nrow(dframe)<0)||(ncol(dframe)<=0)) {
    stop("dframe must be a non-empty data frame")
  }
  varlist <- setdiff(varlist,outcomename)
  varlist <- intersect(varlist,colnames(dframe))
  if(missing(varlist)||(!is.character(varlist))||(length(varlist)<1)) {
    stop("varlist must be a non-empty character vector")
  }
  if(missing(outcomename)||(!is.character(outcomename))||(length(outcomename)!=1)) {
    stop("outcomename must be a length 1 character vector")
  }
}

