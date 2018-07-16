

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
#' @param y_dependent_treatments character what treatment types to build per-outcome level.
#' @return list(cross_frame, treatments_0, treatments_m)
#' 
#' @export
#'
mkCrossFrameMExperiment <- function(d, vars, y_name, 
                                    ...,
                                    y_dependent_treatments = c("catB"),
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
                                    verbose=FALSE,
                                    parallelCluster=NULL,
                                    use_parallel = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::mkCrossFrameMExperiment")
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
  
  # build one set of y-dependent treatments per possible y outcome
  y_levels <- sort(unique(as.character(d[[y_name]])))
  y_l_names <- vtreat_make_names(y_levels)
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
      list(treatments_i = cfe$treatments,
           cross_frame_i = cross_frame_i)
    })
  names(cfe_list) <- y_levels
  
  # build an overall cross-frame for training
  dy <- data.frame(y = as.character(d[[y_name]]),
                   stringsAsFactors = FALSE)
  cross_frame <- do.call(
    cbind,
    c(prepare(treatments_0, d),
      lapply(cfe_list, function(cfei) cfei$cross_frame_i),
      dy,
      stringsAsFactors = FALSE))
  
  # build a prepare function for new data
  treatments_m <- lapply(cfe_list, function(cfei) cfei$treatments_i)
  # return components
  list(cross_frame = cross_frame,
       treat_m = list(
         y_l_names = y_l_names,
         treatments_0 = treatments_0,
         treatments_m = treatments_m))
}

#' Function to apply mkCrossFrameMExperiment treatemnts.
#' 
#' Please see \code{vignette("MultiClassVtreat", package = "vtreat")} \url{https://winvector.github.io/vtreat/articles/MultiClassVtreat.html}.
#' 
#' @param new_d new data to process.
#' @param treat_m element from mkCrossFrameMExperiment return.
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
#' @export
#'
prepare_m <- function(new_d, treat_m,
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
  treatments_0 <- treat_m$treatments_0
  treatments_m <- treat_m$treatments_m
  y_l_names <- treat_m$y_l_names
  y_name <- treatments_m[[1]]$outcomename
  treated <- prepare(treatments_0, new_d,
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
    treated_i <- prepare(ti, new_d,
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
  if(y_name %in% colnames(new_d)) {
    treated[[y_name]] <- new_d[[y_name]]
  }
  treated
}


