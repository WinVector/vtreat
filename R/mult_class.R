

#' Function to build multi-outcome vtreat cross frame and treatment plan.
#' 
#' Please see \code{vignette("MultiClassVtreat", package = "vtreat")} \url{https://winvector.github.io/vtreat/articles/MultiClassVtreat.html}.
#' 
#' @param dframe data to learn from
#' @param varlist character, vector of indpendent variable column names.
#' @param outcomename character, name of outcome column.
#' @param ... not used, declared to forced named binding of later arguments
#' @param weights optional training weights for each row
#' @param minFraction optional minimum frequency a categorical level must have to be converted to an indicator column.
#' @param smFactor optional smoothing factor for impact coding models.
#' @param rareCount optional integer, allow levels with this count or below to be pooled into a shared rare-level.  Defaults to 0 or off.
#' @param rareSig optional numeric, suppress levels from pooling at this significance value greater.  Defaults to NULL or off.
#' @param collarProb what fraction of the data (pseudo-probability) to collar data at if doCollar is set during \code{\link{prepare.multinomial_plan}}.
#' @param codeRestriction what types of variables to produce (character array of level codes, NULL means no restriction).
#' @param customCoders map from code names to custom categorical variable encoding functions (please see \url{https://github.com/WinVector/vtreat/blob/main/extras/CustomLevelCoders.md}).
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
#' @param missingness_imputation function of signature f(values: numeric, weights: numeric), simple missing value imputer.
#' @param imputation_map map from column names to functions of signature f(values: numeric, weights: numeric), simple missing value imputers.
#' @return a names list containing cross_frame, treat_m, score_frame, and fit_obj_id
#' 
#' @seealso \code{\link{prepare.multinomial_plan}}
#' 
#' @examples
#' 
#' # numeric example
#' set.seed(23525)
#' 
#' # we set up our raw training and application data
#' dTrainM <- data.frame(
#'   x = c('a', 'a', 'a', 'a', 'b', 'b', NA, NA),
#'   z = c(1, 2, 3, 4, 5, NA, 7, NA), 
#'   y = c(0, 0, 0, 1, 0, 1, 2, 1))
#' 
#' dTestM <- data.frame(
#'   x = c('a', 'b', 'c', NA), 
#'   z = c(10, 20, 30, NA))
#' 
#' # we perform a vtreat cross frame experiment
#' # and unpack the results into treatmentsM,
#' # dTrainMTreated, and score_frame
#' unpack[
#'   treatmentsM = treat_m,
#'   dTrainMTreated = cross_frame,
#'   score_frame = score_frame
#'   ] <- mkCrossFrameMExperiment(
#'     dframe = dTrainM,
#'     varlist = setdiff(colnames(dTrainM), 'y'),
#'     outcomename = 'y',
#'     verbose = FALSE)
#' 
#' # the score_frame relates new
#' # derived variables to original columns
#' score_frame[, c('origName', 'varName', 'code', 'rsq', 'sig', 'outcome_level')] %.>%
#'   print(.)
#' 
#' # the treated frame is a "cross frame" which
#' # is a transform of the training data built 
#' # as if the treatment were learned on a different
#' # disjoint training set to avoid nested model
#' # bias and over-fit.
#' dTrainMTreated %.>%
#'   head(.) %.>%
#'   print(.)
#' 
#' # Any future application data is prepared with
#' # the prepare method.
#' dTestMTreated <- prepare(treatmentsM, dTestM, pruneSig=NULL)
#' 
#' dTestMTreated %.>%
#'   head(.) %.>%
#'   print(.)
#' 
#' @export
#'
mkCrossFrameMExperiment <- function(dframe, varlist, outcomename, 
                                    ...,
                                    weights=c(),
                                    minFraction=0.02,smFactor=0.0,
                                    rareCount=0,rareSig=1,
                                    collarProb=0.0,
                                    codeRestriction=NULL,
                                    customCoders=NULL,
                                    scale=FALSE,doCollar=FALSE,
                                    splitFunction=vtreat::kWayCrossValidation, ncross=3,
                                    forceSplit = FALSE,
                                    catScaling=FALSE,
                                    y_dependent_treatments = c("catB"),
                                    verbose=FALSE,
                                    parallelCluster=NULL,
                                    use_parallel = TRUE,
                                    missingness_imputation = NULL, imputation_map = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::mkCrossFrameMExperiment")
  y_levels <- sort(unique(as.character(dframe[[outcomename]])))
  y_l_names <- vtreat_make_names(y_levels)
  if(length(y_levels)<2) {
    stop("vtreat::mkCrossFrameMExperiment outcome must have 2 or more levels")
  }
  if(length(codeRestriction) > 0) {
    y_dependent_treatments = intersect(y_dependent_treatments, codeRestriction)
  }
  # build y-independent variable treatments
  treatments_0 <- designTreatmentsZ(dframe, varlist, 
                                    weights=weights,
                                    minFraction=minFraction,
                                    rareCount=rareCount,
                                    collarProb=collarProb,
                                    codeRestriction=codeRestriction,
                                    customCoders=customCoders,
                                    verbose= verbose,
                                    parallelCluster=parallelCluster,
                                    use_parallel = use_parallel,
                                    missingness_imputation = missingness_imputation, imputation_map = imputation_map)
  # score them
  tf_0 <- prepare(treatments_0, dframe,
                  extracols = outcomename,
                  parallelCluster = parallelCluster,
                  use_parallel = use_parallel)
  sf_0 <- treatments_0$scoreFrame[order(treatments_0$scoreFrame$varName), , drop = FALSE]
  sframe_0 <- lapply(
    y_levels,
    function(y_target) {
      zCS <- tf_0[[outcomename]]==y_target
      zoYS <- ifelse(zCS,1,0)
      z_vars <- setdiff(colnames(tf_0), outcomename)
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
  sframe_0 <- augment_score_frame(sframe_0)  # get columns to match
  rownames(sframe_0) <- NULL
  rm(list = c("tf_0"))
  # get a shared split plan to minimize data leakage
  if(is.null(splitFunction)) {
    splitFunction <- kWayCrossValidation
  }
  evalSets <- splitFunction(nRows = nrow(dframe), nSplits = ncross, dframe=dframe, y = dframe[[outcomename]])
  splitFunction <- pre_comp_xval(nRows=nrow(dframe), ncross, evalSets)
  # build one set of y-dependent treatments per possible y outcome
  names(y_l_names) <- y_levels
  cfe_list <- NULL
  if(length(y_dependent_treatments) > 0) {
    cfe_list <- lapply(
      y_levels,
      function(y_target) {
        cfe <- mkCrossFrameCExperiment(
          dframe, varlist, outcomename, y_target,
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
          use_parallel = use_parallel,
          missingness_imputation = missingness_imputation, imputation_map = imputation_map)
        cross_frame_i = cfe$crossFrame
        cross_frame_i[[outcomename]] <- NULL
        score_frame_i <- cfe$treatments$scoreFrame
        vars_found <- score_frame_i$varName
        new_vars <- paste0(y_l_names[[y_target]], 
                           "_", 
                           vars_found)
        vars_forward_map_i <- new_vars
        names(vars_forward_map_i) <- vars_found
        vars_reverse_map_i <- vars_found
        names(vars_reverse_map_i) <- new_vars
        colnames(cross_frame_i) <- vars_forward_map_i[colnames(cross_frame_i)]
        score_frame_i$outcome_level <- y_target
        score_frame_i$varName <- vars_forward_map_i[score_frame_i$varName]
        list(treatments_i = cfe$treatments,
             cross_frame_i = cross_frame_i,
             score_frame_i = score_frame_i,
             vars_forward_map_i = vars_forward_map_i,
             vars_reverse_map_i = vars_reverse_map_i)
      })
    names(cfe_list) <- NULL # make sure no names
  }
  
  # build an overall cross-frame for training
  dy <- data.frame(y = as.character(dframe[[outcomename]]),
                   stringsAsFactors = FALSE)
  colnames(dy) = outcomename
  cbind_args <- c(list(prepare(treatments_0, dframe)),
                  lapply(cfe_list, function(cfei) cfei$cross_frame_i),
                  list(dy),
                  stringsAsFactors = FALSE)
  cross_frame <- do.call(
    cbind, cbind_args) 
  treatments_m <- NULL
  if(length(cfe_list) > 0) {
    score_frame <- do.call(
      rbind, 
      lapply(cfe_list, function(cfei) cfei$score_frame_i))
    rownames(score_frame) <- NULL
    # build a prepare function for new data
    
    treatments_m <- lapply(cfe_list, 
                           function(cfei) {
                             list(treatment = cfei$treatments_i,
                                  score_frame_i = cfei$score_frame_i,
                                  vars_forward_map = cfei$vars_forward_map_i,
                                  vars_reverse_map = cfei$vars_reverse_map_i)
                           })
    
    score_frame <- rbind(sframe_0, score_frame)
    score_frame <- augment_score_frame(score_frame)  # recompute augment on joined frame
  } else {
    score_frame = sframe_0
  }
  # return components
  treat_m <- list(
    y_l_names = y_l_names,
    treatments_0 = treatments_0,
    treatments_m = treatments_m,
    fit_obj_id = id_f(dframe))
  class(treat_m) <- "multinomial_plan"
  plan <- list(cross_frame = cross_frame,
               treat_m = treat_m,
               score_frame = score_frame,
               fit_obj_id = treat_m$fit_obj_id)
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
#' @param check_for_duplicate_frames logical, if TRUE check if we called prepare on same data.frame as design step.
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
                      use_parallel= TRUE,
                      check_for_duplicate_frames= TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::prepare.multinomial_plan")
  treatments_0 <- treatmentplan$treatments_0
  treatments_m <- treatmentplan$treatments_m
  y_l_names <- treatmentplan$y_l_names
  outcomename <- treatments_m[[1]]$treatment$outcomename
  old_fit_obj_id <- treatmentplan$fit_obj_id
  if(check_for_duplicate_frames && (!is.null(old_fit_obj_id))) {
    fit_obj_id <- id_f(dframe)
    if(!is.null(fit_obj_id)) {
      if(fit_obj_id == old_fit_obj_id) {
        warning("possibly called prepare() on same data frame as designTreatments*()/mkCrossFrame*Experiment(), this can lead to over-fit.  To avoid this, please use mkCrossFrameMExperiment$crossFrame.")
      }
    }
  }
  treated <- prepare(treatments_0, dframe,
                     pruneSig= pruneSig,
                     scale= scale,
                     doCollar= doCollar,
                     varRestriction= varRestriction,
                     codeRestriction= codeRestriction,
                     trackedValues= trackedValues,
                     extracols= extracols,
                     parallelCluster= parallelCluster,
                     use_parallel= use_parallel,
                     check_for_duplicate_frames= FALSE)
  for(tli in treatments_m) {
    ti <- tli$treatment
    vars_forward_map_i <- tli$vars_forward_map
    vars_reverse_map_i <- tli$vars_reverse_map
    vri <- NULL
    if(length(varRestriction)>0) {
      common_keys <- intersect(varRestriction, names(vars_reverse_map_i))
      if(length(common_keys)<=0) {
        next
      }
      vri <- vars_reverse_map_i[common_keys]
    }
    treated_i <- prepare(ti, dframe,
                         pruneSig= pruneSig,
                         scale= scale,
                         doCollar= doCollar,
                         varRestriction= vri,
                         codeRestriction= codeRestriction,
                         trackedValues= trackedValues,
                         parallelCluster= parallelCluster,
                         use_parallel= use_parallel,
                         check_for_duplicate_frames = FALSE)
    treated_i[[outcomename]] <- NULL
    colnames(treated_i) <- vars_forward_map_i[colnames(treated_i)]
    treated <- cbind(treated, treated_i,
                     stringsAsFactors = FALSE)
  }
  if(outcomename %in% colnames(dframe)) {
    treated[[outcomename]] <- dframe[[outcomename]]
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

