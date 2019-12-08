
# pyvtreat style interfaces for the vtreat code


merge_params <- function(..., params = NULL, user_params = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat:::merge_args")
  if(length(user_params) > 0) {
    for(k in names(user_params)) {
      if(!(k %in% names(params))) {
        stop(paste("parameter key", k, "not recognized"))
      }
      params[[k]] <- user_params[[k]]
    }
  }
  return(params)
}


#' vtreat parameters.
#' 
#' A list of settings and values for vtreat supervised fitting. 
#' See
#' \code{\link{mkCrossFrameCExperiment}}, 
#' \code{\link{designTreatmentsC}}, and
#' \code{\link{prepare.treatmentplan}} for details.
#' 
#' @param user_params list of user overrides.
#' @return filled out parameter list
#' 
#' @export
vtreat_parameters <- function(user_params = NULL) {
  params = list(
    minFraction = 0.02, 
    smFactor = 0.0,
    rareCount = 0, 
    rareSig = NULL,
    collarProb = 0.00,
    codeRestriction = NULL,
    customCoders = NULL, 
    splitFunction = NULL, 
    ncross = 3,
    forceSplit = FALSE,
    catScaling = TRUE,
    verbose = FALSE,
    parallelCluster = NULL,
    use_parallel = TRUE,
    missingness_imputation = NULL,
    pruneSig = NULL,
    scale = FALSE,
    doCollar= FALSE,
    varRestriction = NULL,
    trackedValues = NULL)
  merged_params <- merge_params(params = params, 
                                user_params = user_params)
  return(merged_params)
}


#' Stateful object for designing and applying binomial outcome treatments.
#' 
#' Hold settings are results for binomial classification data preparation
#' 
#' See
#' \code{\link{mkCrossFrameCExperiment}}, 
#' \code{\link{designTreatmentsC}}, and
#' \code{\link{prepare.treatmentplan}} for details.
#' 
#' @param ... not used, force arguments to be specified by name.
#' @param var_list Names of columns to treat (effective variables).
#' @param outcome_name Name of column holding outcome variable. \code{dframe[[outcomename]]} must be only finite non-missing values.
#' @param outcome_target Value/level of outcome to be considered "success",  and there must be a cut such that \code{dframe[[outcomename]]==outcometarget} at least twice and dframe[[outcomename]]!=outcometarget at least twice.
#' @param cols_to_copy list of extra columns to copy.
#' @param params parameters list from \code{vtreat_parameters}
#' @param imputation_map map from column names to functions of signature f(values: numeric, weights: numeric), simple missing value imputers.
#' 
#' 
#' @export
#' 
BinomialOutcomeTreatment <- function(...,
                                     var_list,
                                     outcome_name, 
                                     outcome_target,
                                     cols_to_copy = NULL,
                                     params = NULL,
                                     imputation_map = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::BinomialOutcomeTreatment")
  params <- vtreat_parameters(params)
  settings <- list(
    var_list = var_list,
    outcome_name = outcome_name, 
    outcome_target = outcome_target,
    cols_to_copy = cols_to_copy,
    params = params,
    imputation_map = imputation_map,
    state = new.env(parent = emptyenv())
  )
  assign("tp", NULL, envir = settings$state)
  assign("ce", NULL, envir = settings$state)
  fit <- function(dframe, ..., weights = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::BinomialOutcomeTreatment$fit")
    assign("tp", NULL, envir = settings$state)
    assign("ce", NULL, envir = settings$state)
    tp <- designTreatmentsC(
      dframe = dframe,
      varlist = settings$var_list,
      outcomename = settings$outcome_name, 
      outcometarget = settings$outcome_target,
      weights = weights,
      minFraction = settings$params$minFraction,
      smFactor = settings$params$smFactor,
      rareCount = settings$params$rareCount,
      rareSig = settings$params$rareSig,
      collarProb = settings$params$collarProb,
      codeRestriction = settings$params$codeRestriction,
      customCoders = settings$params$customCoders, 
      splitFunction = settings$params$splitFunction,
      ncross = settings$params$ncross,
      forceSplit = settings$params$forceSplit,
      catScaling = settings$params$catScaling,
      verbose = settings$params$verbose,
      parallelCluster = settings$params$parallelCluster,
      use_parallel = settings$params$use_parallel,
      missingness_imputation = settings$params$missingness_imputation, 
      imputation_map = settings$params$imputation_map)
    assign("tp", tp, envir = settings$state)
    invisible(obj) # allow method chaining
  }
  obj <- list(
    fit = fit,
    settings = settings)
  class(obj) <- "vtreat_BinomialOutcomeTreatment"
  transform <- function(dframe) {
    tp <- get('tp', envir = settings$state, inherits = FALSE)
    res <- prepare(
      treatmentplan = tp,
      dframe = dframe,
      pruneSig= settings$params$pruneSig,
      scale= settings$params$scale,
      doCollar= settings$params$doCollar,
      varRestriction= settings$params$varRestriction,
      codeRestriction= settings$params$codeRestriction,
      trackedValues= settings$params$trackedValues,
      extracols = settings$cols_to_copy,
      parallelCluster = settings$params$parallelCluster,
      use_parallel = settings$params$use_parallel)
    return(res)
  }
  fit_transform <- function(dframe, ..., weights = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::BinomialOutcomeTreatment$fit_transform")
    assign("tp", NULL, envir = settings$state)
    assign("ce", NULL, envir = settings$state)
    ce <- mkCrossFrameCExperiment(
      dframe = dframe,
      varlist = settings$var_list,
      outcomename = settings$outcome_name, 
      outcometarget = settings$outcome_target,
      weights = weights,
      minFraction = settings$params$minFraction,
      smFactor = settings$params$smFactor,
      rareCount = settings$params$rareCount,
      rareSig = settings$params$rareSig,
      collarProb = settings$params$collarProb,
      codeRestriction = settings$params$codeRestriction,
      customCoders = settings$params$customCoders, 
      splitFunction = settings$params$splitFunction,
      ncross = settings$params$ncross,
      forceSplit = settings$params$forceSplit,
      catScaling = settings$params$catScaling,
      verbose = settings$params$verbose,
      parallelCluster = settings$params$parallelCluster,
      use_parallel = settings$params$use_parallel,
      missingness_imputation = settings$params$missingness_imputation, 
      imputation_map = settings$params$imputation_map)
    tp <- ce$treatments
    assign("tp", tp, envir = settings$state)
    assign("ce", ce, envir = settings$state)
    return(ce$crossFrame)
  }
  score_frame <- function() {
    tp <- get('tp', envir = settings$state, inherits = FALSE)
    return(tp$scoreFrame)
  }
  obj$transform = transform
  obj$fit_transform = fit_transform
  obj$score_frame = score_frame
  return(obj)
}
