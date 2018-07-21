

#' Function to build multi-outcome vtreat cross frame and treatment plan.
#' 
#' Please see \code{vignette("MultiClassVtreat", package = "vtreat")} \url{https://winvector.github.io/vtreat/articles/MultiClassVtreat.html}.
#' 
#' @param d data to learn from
#' @param vars character, vector of indpendent variable column names.
#' @param y_name character, name of outcome column.
#' @param ... not used, declared to forced named binding of later arguments
#' @param weights optional training weights for each row
#' @param minFraction optional minimum frequency a categorical level must have to be converted to an indicator column.
#' @param smFactor optional smoothing factor for impact coding models.
#' @param rareCount optional integer, allow levels with this count or below to be pooled into a shared rare-level.  Defaults to 0 or off.
#' @param rareSig optional numeric, suppress levels from pooling at this significance value greater.  Defaults to NULL or off.
#' @param collarProb what fraction of the data (pseudo-probability) to collar data at if doCollar is set during \code{\link{prepare.multinomial_plan}}.
#' @param codeRestriction what types of variables to produce (character array of level codes, NULL means no restriction).
#' @param customCoders map from code names to custom categorical variable encoding functions (please see \url{https://github.com/WinVector/vtreat/blob/master/extras/CustomLevelCoders.md}).
#' @param scale optional if TRUE replace numeric variables with regression ("move to outcome-scale").
#' @param doCollar optional if TRUE collar numeric variables by cutting off after a tail-probability specified by collarProb during treatment design.
#' @param splitFunction (optional) see vtreat::buildEvalSets .
#' @param ncross optional scalar>=2 number of cross-validation rounds to design.
#' @param forceSplit logical, if TRUE force cross-validated significance calculations on all variables.
#' @param catScaling optional, if TRUE use glm() linkspace, if FALSE use lm() for scaling.
#' @param y_dependent_treatments character what treatment types to build per-outcome level.
#' @param verbose if TRUE print progress.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow.
#' @param use_parallel logical, if TRUE use parallel methods.
#' @return list(cross_frame, treatments_0, treatments_m)
#' 
#' @seealso \code{\link{prepare.multinomial_plan}}
#' 
#' @export
#'
mkCrossFrameMExperiment <- function(d, vars, y_name, 
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
                                    y_dependent_treatments = c("catB"),
                                    verbose=FALSE,
                                    parallelCluster=NULL,
                                    use_parallel = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::mkCrossFrameMExperiment")
  y_levels <- sort(unique(as.character(d[[y_name]])))
  y_l_names <- vtreat_make_names(y_levels)
  if(length(y_levels)<2) {
    stop("vtreat::mkCrossFrameMExperiment outcome must have 2 or more levels")
  }
  # build y-independent variable treatments
  treatments_0 <- designTreatmentsZ(d, vars, 
                                    weights=weights,
                                    minFraction=minFraction,
                                    rareCount=rareCount,
                                    collarProb=collarProb,
                                    codeRestriction=codeRestriction,
                                    customCoders=customCoders,
                                    verbose= verbose,
                                    parallelCluster=parallelCluster,
                                    use_parallel = use_parallel)
  # score them
  tf_0 <- prepare(treatments_0, d,
                  extracols = y_name,
                  parallelCluster = parallelCluster,
                  use_parallel = use_parallel)
  sf_0 <- treatments_0$scoreFrame[order(treatments_0$scoreFrame$varName), , drop = FALSE]
  sframe_0 <- lapply(
    y_levels,
    function(y_target) {
      zCS <- tf_0[[y_name]]==y_target
      zoYS <- ifelse(zCS,1,0)
      z_vars <- setdiff(colnames(tf_0), y_name)
      swkr <- .mkScoreColWorker(zoYS, zCS, TRUE, weights)
      newVarsSP <- lapply(z_vars,
                          function(nv) {
                            list(nv=nv,dfc=tf_0[[nv]])
                          })
      sframe_0 <- plapply(newVarsSP,swkr,
                          parallelCluster = parallelCluster,
                          use_parallel = use_parallel) 
      sframe_0 <- do.call(rbind, sframe_0)
      sframe_0 <- sframe_0[order(sframe_0$varName), , drop = FALSE]
      sframe_0$outcome_level <- y_target
      sframe_0$needsSplit <- sf_0$needsSplit
      sframe_0$extraModelDegrees <- sf_0$extraModelDegrees
      sframe_0$origName <- sf_0$origName
      sframe_0$code <- sf_0$code
      rownames(sframe_0) <- NULL
      sframe_0
    })
  sframe_0 <- do.call(rbind, sframe_0)
  rownames(sframe_0) <- NULL
  rm(list = c("tf_0"))
  # build one set of y-dependent treatments per possible y outcome
  names(y_l_names) <- y_levels
  cfe_list <- lapply(
    y_levels,
    function(y_target) {
      cfe <- mkCrossFrameCExperiment(
        d, vars, y_name, y_target,
        weights=weights,
        minFraction=minFraction,
        smFactor=smFactor,
        rareCount=rareCount,
        rareSig=rareSig,
        collarProb=collarProb,
        codeRestriction=y_dependent_treatments,
        customCoders=customCoders,
        scale=scale,
        doCollar=doCollar,
        splitFunction=splitFunction,
        ncross=ncross,
        forceSplit = forceSplit,
        catScaling=catScaling,
        verbose= verbose,
        parallelCluster=parallelCluster,
        use_parallel = use_parallel)
      cross_frame_i = cfe$crossFrame
      cross_frame_i[[y_name]] <- NULL
      colnames(cross_frame_i) <- paste0(y_l_names[[y_target]], 
                                        "_", 
                                        colnames(cross_frame_i))
      score_frame_i <- cfe$treatments$scoreFrame
      score_frame_i$outcome_level <- y_target
      score_frame_i$varName <- paste0(y_l_names[[y_target]], 
                                      "_", 
                                      score_frame_i$varName)
      list(treatments_i = cfe$treatments,
           cross_frame_i = cross_frame_i,
           score_frame_i = score_frame_i)
    })
  names(cfe_list) <- NULL # make sure no names
  
  # build an overall cross-frame for training
  dy <- data.frame(y = as.character(d[[y_name]]),
                   stringsAsFactors = FALSE)
  cbind_args <- c(list(prepare(treatments_0, d)),
                  lapply(cfe_list, function(cfei) cfei$cross_frame_i),
                  list(dy),
                  stringsAsFactors = FALSE)
  cross_frame <- do.call(
    cbind, cbind_args) 
  score_frame <- do.call(
    rbind, 
    lapply(cfe_list, function(cfei) cfei$score_frame_i))
  rownames(score_frame) <- NULL
  # build a prepare function for new data
  treatments_m <- lapply(cfe_list, function(cfei) cfei$treatments_i)
  # return components
  treat_m <- list(
    y_l_names = y_l_names,
    treatments_0 = treatments_0,
    treatments_m = treatments_m)
  class(treat_m) <- "multinomial_plan"
  plan <- list(cross_frame = cross_frame,
               treat_m = treat_m,
               score_frame = rbind(sframe_0, score_frame))
  plan
}

#' Function to apply mkCrossFrameMExperiment treatemnts.
#' 
#' Please see \code{vignette("MultiClassVtreat", package = "vtreat")} \url{https://winvector.github.io/vtreat/articles/MultiClassVtreat.html}.
#' 
#' @param treatmentplan multinomial_plan from mkCrossFrameMExperiment.
#' @param dframe new data to process.
#' @param ... not used, declared to forced named binding of later arguments
#' @param pruneSig suppress variables with significance above this level
#' @param scale optional if TRUE replace numeric variables with single variable model regressions ("move to outcome-scale").  These have mean zero and (for variables with significant less than 1) slope 1 when regressed  (lm for regression problems/glm for classification problems) against outcome.
#' @param doCollar optional if TRUE collar numeric variables by cutting off after a tail-probability specified by collarProb during treatment design.
#' @param varRestriction optional list of treated variable names to restrict to
#' @param codeRestriction optional list of treated variable codes to restrict to
#' @param trackedValues optional named list mapping variables to know values, allows warnings upon novel level appearances (see \code{\link{track_values}})
#' @param extracols extra columns to copy.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow.
#' @param use_parallel logical, if TRUE use parallel methods.
#' @return prepared data frame.
#' 
#' @seealso \code{\link{mkCrossFrameMExperiment}}, \code{\link{prepare}}
#' @export
#'
prepare.multinomial_plan <- function(treatmentplan, dframe,
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
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::prepare_m")
  treatments_0 <- treatmentplan$treatments_0
  treatments_m <- treatmentplan$treatments_m
  y_l_names <- treatmentplan$y_l_names
  y_name <- treatments_m[[1]]$outcomename
  treated <- prepare(treatments_0, dframe,
                     pruneSig= pruneSig,
                     scale= scale,
                     doCollar= doCollar,
                     varRestriction= varRestriction,
                     codeRestriction= codeRestriction,
                     trackedValues= trackedValues,
                     extracols= extracols,
                     parallelCluster= parallelCluster,
                     use_parallel= use_parallel)
  for(ti in treatments_m) {
    treated_i <- prepare(ti, dframe,
                         pruneSig= pruneSig,
                         scale= scale,
                         doCollar= doCollar,
                         varRestriction= varRestriction,
                         codeRestriction= codeRestriction,
                         trackedValues= trackedValues,
                         parallelCluster= parallelCluster,
                         use_parallel= use_parallel)
    treated_i[[y_name]] <- NULL
    colnames(treated_i) <- paste0(y_l_names[[ti$outcomeTarget]], 
                                  "_", 
                                  colnames(treated_i))
    treated <- cbind(treated, treated_i,
                     stringsAsFactors = FALSE)
  }
  if(y_name %in% colnames(dframe)) {
    treated[[y_name]] <- dframe[[y_name]]
  }
  treated
}


#' @export
format.multinomial_plan <- function(x, ...) { 
  "Multi Class Plan"
}

#' @export
as.character.multinomial_plan <- function (x, ...) {
  format(x, ...)
}

#'
#' Print treatmentplan.
#' @param x treatmentplan
#' @param ... additional args (to match general signature).
#' @export
print.multinomial_plan <- function(x, ...) { 
  print(format(x), ...) 
}

