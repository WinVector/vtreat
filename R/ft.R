
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
#' Hold settings are results for binomial classification data preparation.
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
  obj <- list(settings = settings)
  class(obj) <- "vtreat_BinomialOutcomeTreatment"
  fit <- function(dframe, ..., weights = NULL, parallelCluster = NULL) {
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
      parallelCluster = parallelCluster,
      use_parallel = settings$params$use_parallel,
      missingness_imputation = settings$params$missingness_imputation, 
      imputation_map = settings$params$imputation_map)
    assign("tp", tp, envir = settings$state)
    invisible(obj) # allow method chaining
  }
  transform <- function(dframe, ..., parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::BinomialOutcomeTreatment$transform")
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
      parallelCluster = parallelCluster,
      use_parallel = settings$params$use_parallel)
    return(res)
  }
  fit_transform <- function(dframe, ..., weights = NULL, parallelCluster = NULL) {
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
      parallelCluster = parallelCluster,
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
  get_transform <- function() {
    tp <- get('tp', envir = settings$state, inherits = FALSE)
    return(tp)
  }
  get_cross_frame_experiment <- function() {
    ce <- get('tp', envir = settings$state, inherits = FALSE)
    return(ce)
  }
  obj$fit = fit
  obj$transform = transform
  obj$fit_transform = fit_transform
  obj$score_frame = score_frame
  obj$get_transform = get_transform
  obj$get_cross_frame_experiment = get_cross_frame_experiment
  return(obj)
}


#' Stateful object for designing and applying numeric outcome treatments.
#' 
#' Hold settings are results for regression data preparation.
#' 
#' See
#' \code{\link{mkCrossFrameNExperiment}}, 
#' \code{\link{designTreatmentsN}}, and
#' \code{\link{prepare.treatmentplan}} for details.
#' 
#' @param ... not used, force arguments to be specified by name.
#' @param var_list Names of columns to treat (effective variables).
#' @param outcome_name Name of column holding outcome variable. \code{dframe[[outcomename]]} must be only finite non-missing values.
#' @param cols_to_copy list of extra columns to copy.
#' @param params parameters list from \code{vtreat_parameters}
#' @param imputation_map map from column names to functions of signature f(values: numeric, weights: numeric), simple missing value imputers.
#' 
#' 
#' @export
#' 
NumericOutcomeTreatment <- function(...,
                                    var_list,
                                    outcome_name, 
                                    cols_to_copy = NULL,
                                    params = NULL,
                                    imputation_map = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::NumericOutcomeTreatment")
  params <- vtreat_parameters(params)
  settings <- list(
    var_list = var_list,
    outcome_name = outcome_name, 
    cols_to_copy = cols_to_copy,
    params = params,
    imputation_map = imputation_map,
    state = new.env(parent = emptyenv())
  )
  assign("tp", NULL, envir = settings$state)
  assign("ce", NULL, envir = settings$state)
  obj <- list(settings = settings)
  class(obj) <- "vtreat_NumericOutcomeTreatment"
  fit <- function(dframe, ..., weights = NULL, parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::NumericOutcomeTreatment$fit")
    assign("tp", NULL, envir = settings$state)
    assign("ce", NULL, envir = settings$state)
    tp <- designTreatmentsN(
      dframe = dframe,
      varlist = settings$var_list,
      outcomename = settings$outcome_name, 
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
      parallelCluster = parallelCluster,
      use_parallel = settings$params$use_parallel,
      missingness_imputation = settings$params$missingness_imputation, 
      imputation_map = settings$params$imputation_map)
    assign("tp", tp, envir = settings$state)
    invisible(obj) # allow method chaining
  }
  transform <- function(dframe, ..., parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::NumericOutcomeTreatment$transform")
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
      parallelCluster = parallelCluster,
      use_parallel = settings$params$use_parallel)
    return(res)
  }
  fit_transform <- function(dframe, ..., weights = NULL, parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::NumericOutcomeTreatment$fit_transform")
    assign("tp", NULL, envir = settings$state)
    assign("ce", NULL, envir = settings$state)
    ce <- mkCrossFrameNExperiment(
      dframe = dframe,
      varlist = settings$var_list,
      outcomename = settings$outcome_name, 
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
      parallelCluster = parallelCluster,
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
  get_transform <- function() {
    tp <- get('tp', envir = settings$state, inherits = FALSE)
    return(tp)
  }
  get_cross_frame_experiment <- function() {
    ce <- get('tp', envir = settings$state, inherits = FALSE)
    return(ce)
  }
  obj$fit = fit
  obj$transform = transform
  obj$fit_transform = fit_transform
  obj$score_frame = score_frame
  obj$get_transform = get_transform
  obj$get_cross_frame_experiment = get_cross_frame_experiment
  return(obj)
}


#' Stateful object for designing and applying multinomial outcome treatments.
#' 
#' Hold settings are results for multinomial classification data preparation.
#' 
#' See
#' \code{\link{mkCrossFrameMExperiment}} and
#' \code{\link{prepare.multinomial_plan}} for details.
#' 
#' Note: there currently is no \code{designTreatmentsM}, 
#' so \code{MultinomialOutcomeTreatment$fit()} is implemented in terms
#' of \code{MultinomialOutcomeTreatment$fit_transform()} 
#' 
#' @param ... not used, force arguments to be specified by name.
#' @param var_list Names of columns to treat (effective variables).
#' @param outcome_name Name of column holding outcome variable. \code{dframe[[outcomename]]} must be only finite non-missing values.
#' @param cols_to_copy list of extra columns to copy.
#' @param params parameters list from \code{vtreat_parameters}
#' @param imputation_map map from column names to functions of signature f(values: numeric, weights: numeric), simple missing value imputers.
#' 
#' 
#' @export
#' 
MultinomialOutcomeTreatment <- function(...,
                                        var_list,
                                        outcome_name, 
                                        cols_to_copy = NULL,
                                        params = NULL,
                                        imputation_map = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::MultinomialOutcomeTreatment")
  params <- vtreat_parameters(params)
  settings <- list(
    var_list = var_list,
    outcome_name = outcome_name, 
    cols_to_copy = cols_to_copy,
    params = params,
    imputation_map = imputation_map,
    state = new.env(parent = emptyenv())
  )
  assign("tp", NULL, envir = settings$state)
  assign("ce", NULL, envir = settings$state)
  obj <- list(settings = settings)
  class(obj) <- "vtreat_MultinomialOutcomeTreatment"
  transform <- function(dframe, ..., parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::MultinomialOutcomeTreatment$transform")
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
      parallelCluster = parallelCluster,
      use_parallel = settings$params$use_parallel)
    return(res)
  }
  fit_transform <- function(dframe, ..., weights = NULL, parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::MultinomialOutcomeTreatment$fit_transform")
    assign("tp", NULL, envir = settings$state)
    assign("ce", NULL, envir = settings$state)
    ce <- mkCrossFrameNExperiment(
      dframe = dframe,
      varlist = settings$var_list,
      outcomename = settings$outcome_name, 
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
      parallelCluster = parallelCluster,
      use_parallel = settings$params$use_parallel,
      missingness_imputation = settings$params$missingness_imputation, 
      imputation_map = settings$params$imputation_map)
    tp <- ce$treatments
    assign("tp", tp, envir = settings$state)
    assign("ce", ce, envir = settings$state)
    return(ce$crossFrame)
  }
  fit <- function(dframe, ..., weights = NULL, parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::MultinomialOutcomeTreatment$fit")
   fit_transform(dframe = dframe, weights = weights, parallelCluster = parallelCluster)
    invisible(obj) # allow method chaining
  }
  score_frame <- function() {
    tp <- get('tp', envir = settings$state, inherits = FALSE)
    return(tp$scoreFrame)
  }
  get_transform <- function() {
    tp <- get('tp', envir = settings$state, inherits = FALSE)
    return(tp)
  }
  get_cross_frame_experiment <- function() {
    ce <- get('tp', envir = settings$state, inherits = FALSE)
    return(ce)
  }
  obj$fit = fit
  obj$transform = transform
  obj$fit_transform = fit_transform
  obj$score_frame = score_frame
  obj$get_transform = get_transform
  obj$get_cross_frame_experiment = get_cross_frame_experiment
  return(obj)
}


#' vtreat unsupervisedparameters.
#' 
#' A list of settings and values for vtreat unsupervised fitting. 
#' See
#' \code{\link{mkCrossFrameCExperiment}}, 
#' \code{\link{designTreatmentsC}}, and
#' \code{\link{prepare.treatmentplan}} for details.
#' 
#' @param user_params list of user overrides.
#' @return filled out parameter list
#' 
#' @export
unsupervised_parameters <- function(user_params = NULL) {
  params = list(
    minFraction = 0.02, 
    smFactor = 0.0,
    rareCount = 0, 
    rareSig = NULL,
    collarProb = 0.00,
    codeRestriction = NULL,
    customCoders = NULL, 
    verbose = FALSE,
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


#' Stateful object for designing and applying unsupervised treatments.
#' 
#' Hold settings are results for unsupervised data preparation.
#' 
#' See
#' \code{\link{designTreatmentsZ}} and
#' \code{\link{prepare.treatmentplan}} for details.
#' 
#' Note: for \code{UnsupervisedTreatment} \code{fit_transform(d)} is implemented
#' as \code{fit(d)$transform(d)}.
#' 
#' @param ... not used, force arguments to be specified by name.
#' @param var_list Names of columns to treat (effective variables).
#' @param cols_to_copy list of extra columns to copy.
#' @param params parameters list from \code{unsupervised_parameters}
#' @param imputation_map map from column names to functions of signature f(values: numeric, weights: numeric), simple missing value imputers.
#' 
#' 
#' @export
#' 
UnsupervisedTreatment <- function(...,
                                  var_list,
                                  cols_to_copy = NULL,
                                  params = NULL,
                                  imputation_map = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::MultinomialOutcomeTreatment")
  params <- unsupervised_parameters(params)
  settings <- list(
    var_list = var_list,
    cols_to_copy = cols_to_copy,
    params = params,
    imputation_map = imputation_map,
    state = new.env(parent = emptyenv())
  )
  assign("tp", NULL, envir = settings$state)
  obj <- list(settings = settings)
  class(obj) <- "vtreat_UnsupervisedTreatment"
  fit <- function(dframe, ..., weights = NULL, parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::UnsupervisedTreatment$fit")
    assign("tp", NULL, envir = settings$state)
    tp <- designTreatmentsZ(
      dframe = dframe,
      varlist = settings$var_list,
      weights = weights,
      minFraction = settings$params$minFraction,
      smFactor = settings$params$smFactor,
      rareCount = settings$params$rareCount,
      rareSig = settings$params$rareSig,
      collarProb = settings$params$collarProb,
      codeRestriction = settings$params$codeRestriction,
      customCoders = settings$params$customCoders, 
      parallelCluster = parallelCluster,
      use_parallel = settings$params$use_parallel,
      missingness_imputation = settings$params$missingness_imputation, 
      imputation_map = settings$params$imputation_map)
    assign("tp", tp, envir = settings$state)
    invisible(obj) # allow method chaining
  }
  transform <- function(dframe, ..., parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::UnsupervisedTreatment$transform")
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
      parallelCluster = parallelCluster,
      use_parallel = settings$params$use_parallel)
    return(res)
  }
  fit_transform <- function(dframe, ..., weights = NULL, parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::UnsupervisedTreatment$fit_transform")
    fit(dframe = dframe, weights = weights, parallelCluster = parallelCluster)
    res <- transform(dframe = dframe, parallelCluster = parallelCluster)
    return(res)
  }
  score_frame <- function() {
    tp <- get('tp', envir = settings$state, inherits = FALSE)
    return(tp$scoreFrame)
  }
  get_transform <- function() {
    tp <- get('tp', envir = settings$state, inherits = FALSE)
    return(tp)
  }
  obj$fit = fit
  obj$transform = transform
  obj$fit_transform = fit_transform
  obj$score_frame = score_frame
  obj$get_transform = get_transform
  return(obj)
}


