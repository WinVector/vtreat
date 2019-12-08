
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


#' vtreat classification parameters.
#' 
#' A list of settings and values for vtreat binomial classification fitting. 
#' See
#' \code{\link{mkCrossFrameCExperiment}}, 
#' \code{\link{designTreatmentsC}}, and
#' \code{\link{prepare.treatmentplan}} for details.
#' 
#' @param user_params list of user overrides.
#' @return filled out parameter list
#' 
#' @export
classification_parameters <- function(user_params = NULL) {
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
  class(merged_params) <- 'classification_parameters'
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
#' @param params parameters list from \code{classification_parameters}
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
  if((!is.null(params)) && (!('classification_parameters' %in% class(params)))) {
    stop("vtreat::BinomialOutcomeTreatment extpected class classification_parameters")
  }
  params <- classification_parameters(params)
  settings <- list(
    var_list = var_list,
    outcome_name = outcome_name, 
    outcome_target = outcome_target,
    cols_to_copy = cols_to_copy,
    params = params,
    imputation_map = imputation_map,
    state = new.env(parent = emptyenv())
  )
  assign("transform", NULL, envir = settings$state)
  assign("score_frame", NULL, envir = settings$state)
  obj <- list(settings = settings)
  class(obj) <- "vtreat_BinomialOutcomeTreatment"
  fit <- function(dframe, ..., weights = NULL, parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::BinomialOutcomeTreatment$fit")
    assign("transform", NULL, envir = settings$state)
    assign("score_frame", NULL, envir = settings$state)
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
    assign("transform", tp, envir = settings$state)
    assign("score_frame", tp$scoreFrame, envir = settings$state)
    invisible(obj) # allow method chaining
  }
  transform <- function(dframe, ..., parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::BinomialOutcomeTreatment$transform")
    tp <- get('transform', envir = settings$state, inherits = FALSE)
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
    assign("transform", NULL, envir = settings$state)
    assign("score_frame", NULL, envir = settings$state)
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
    assign("transform", tp, envir = settings$state)
    assign("score_frame", tp$scoreFrame, envir = settings$state)
    res <- ce$crossFrame
    for(c in settings$cols_to_copy) {
      res[[c]] <- dframe[[c]]
    }
    return(res)
  }
  score_frame <- function() {
    res <- get('score_frame', envir = settings$state, inherits = FALSE)
    return(res)
  }
  get_transform <- function() {
    td <- get('transform', envir = settings$state, inherits = FALSE)
    return(res)
  }
  obj$fit = fit
  obj$transform = transform
  obj$fit_transform = fit_transform
  obj$score_frame = score_frame
  obj$get_transform = get_transform
  return(obj)
}


#' vtreat regression parameters.
#' 
#' A list of settings and values for vtreat regression fitting. 
#' See
#' \code{\link{mkCrossFrameCExperiment}}, 
#' \code{\link{designTreatmentsC}}, and
#' \code{\link{mkCrossFrameNExperiment}}, 
#' \code{\link{designTreatmentsN}},
#' \code{\link{prepare.treatmentplan}} for details.
#' 
#' @param user_params list of user overrides.
#' @return filled out parameter list
#' 
#' @export
regression_parameters <- function(user_params = NULL) {
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
  class(merged_params) <- 'regression_parameters'
  return(merged_params)
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
#' @param params parameters list from \code{regression_parameters}
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
  if((!is.null(params)) && (!('regression_parameters' %in% class(params)))) {
    stop("vtreat::NumericOutcomeTreatment extpected class regression_parameters")
  }
  params <- regression_parameters(params)
  settings <- list(
    var_list = var_list,
    outcome_name = outcome_name, 
    cols_to_copy = cols_to_copy,
    params = params,
    imputation_map = imputation_map,
    state = new.env(parent = emptyenv())
  )
  assign("transform", NULL, envir = settings$state)
  assign("score_frame", NULL, envir = settings$state)
  obj <- list(settings = settings)
  class(obj) <- "vtreat_NumericOutcomeTreatment"
  fit <- function(dframe, ..., weights = NULL, parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::NumericOutcomeTreatment$fit")
    assign("transform", NULL, envir = settings$state)
    assign("score_frame", NULL, envir = settings$state)
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
      verbose = settings$params$verbose,
      parallelCluster = parallelCluster,
      use_parallel = settings$params$use_parallel,
      missingness_imputation = settings$params$missingness_imputation, 
      imputation_map = settings$params$imputation_map)
    assign("transform", tp, envir = settings$state)
    assign("score_frame", tp$scoreFrame, envir = settings$state)
    invisible(obj) # allow method chaining
  }
  transform <- function(dframe, ..., parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::NumericOutcomeTreatment$transform")
    tp <- get('transform', envir = settings$state, inherits = FALSE)
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
    assign("transform", NULL, envir = settings$state)
    assign("score_frame", NULL, envir = settings$state)
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
      verbose = settings$params$verbose,
      parallelCluster = parallelCluster,
      use_parallel = settings$params$use_parallel,
      missingness_imputation = settings$params$missingness_imputation, 
      imputation_map = settings$params$imputation_map)
    tp <- ce$treatments
    assign("transform", tp, envir = settings$state)
    assign("score_frame", tp$scoreFrame, envir = settings$state)
    res <- ce$crossFrame
    for(c in settings$cols_to_copy) {
      res[[c]] <- dframe[[c]]
    }
    return(res)
  }
  score_frame <- function() {
    res <- get('score_frame', envir = settings$state, inherits = FALSE)
    return(res)
  }
  get_transform <- function() {
    td <- get('transform', envir = settings$state, inherits = FALSE)
    return(res)
  }
  obj$fit = fit
  obj$transform = transform
  obj$fit_transform = fit_transform
  obj$score_frame = score_frame
  obj$get_transform = get_transform
  return(obj)
}


#' vtreat multinomial parameters.
#' 
#' A list of settings and values for vtreat multinomial classification fitting. 
#' See
#' \code{\link{mkCrossFrameMExperiment}} and
#' \code{\link{prepare.multinomial_plan}} for details.
#' 
#' @param user_params list of user overrides.
#' @return filled out parameter list
#' 
#' @export
multinomial_parameters <- function(user_params = NULL) {
  params = list(
    minFraction=0.02,
    smFactor=0.0,
    rareCount=0,
    rareSig=1,
    collarProb=0.0,
    codeRestriction=NULL,
    customCoders=NULL,
    scale=FALSE,doCollar=FALSE,
    splitFunction=NULL,ncross=3,
    forceSplit = FALSE,
    catScaling=FALSE,
    y_dependent_treatments = c("catB"),
    verbose=FALSE,
    use_parallel = TRUE,
    missingness_imputation = NULL, 
    imputation_map = NULL)
  merged_params <- merge_params(params = params, 
                                user_params = user_params)
  class(merged_params) <- 'multinomial_parameters'
  return(merged_params)
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
#' @param params parameters list from \code{multinomial_parameters}
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
  if((!is.null(params)) && (!('multinomial_parameters' %in% class(params)))) {
    stop("vtreat::MultinomialOutcomeTreatment extpected class multinomial_parameters")
  }
  params <- multinomial_parameters(params)
  settings <- list(
    var_list = var_list,
    outcome_name = outcome_name, 
    cols_to_copy = cols_to_copy,
    params = params,
    imputation_map = imputation_map,
    state = new.env(parent = emptyenv())
  )
  assign("transform", NULL, envir = settings$state)
  assign("score_frame", NULL, envir = settings$state)
  obj <- list(settings = settings)
  class(obj) <- "vtreat_MultinomialOutcomeTreatment"
  transform <- function(dframe, ..., parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::MultinomialOutcomeTreatment$transform")
    tp <- get('transform', envir = settings$state, inherits = FALSE)
    res <- prepare(
      treatmentplan = tp,
      dframe = dframe,
      parallelCluster = parallelCluster,
      pruneSig= settings$params$pruneSig,
      scale= settings$params$scale,
      doCollar= settings$params$doCollar,
      varRestriction= settings$params$varRestriction,
      codeRestriction= settings$params$codeRestriction,
      trackedValues= settings$params$trackedValues,
      extracols= settings$cols_to_copy,
      use_parallel= settings$params$use_parallel)
    return(res)
  }
  fit_transform <- function(dframe, ..., weights = NULL, parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::MultinomialOutcomeTreatment$fit_transform")
    assign("transform", NULL, envir = settings$state)
    assign("score_frame", NULL, envir = settings$state)
    td <- mkCrossFrameMExperiment(
      d = dframe,
      vars = settings$var_list,
      y_name = settings$outcome_name, 
      weights = weights,
      parallelCluster = parallelCluster,
      minFraction=settings$params$minFraction,
      smFactor=settings$params$smFactor,
      rareCount=settings$params$rareCount,
      rareSig=settings$params$rareSig,
      collarProb=settings$params$collarProb,
      codeRestriction=settings$params$codeRestriction,
      customCoders=settings$params$customCoders,
      scale=settings$params$scale,
      doCollar=settings$params$doCollar,
      splitFunction=settings$params$splitFunction,
      ncross=settings$params$ncross,
      forceSplit = settings$params$forceSplit,
      catScaling=settings$params$catScaling,
      y_dependent_treatments = settings$params$y_dependent_treatments,
      verbose=settings$params$verbose,
      use_parallel = settings$params$use_parallel,
      missingness_imputation = settings$params$missingness_imputation, 
      imputation_map = settings$params$imputation_map)
    assign("transform", td$treat_m, envir = settings$state)
    assign("score_frame", td$score_frame, envir = settings$state)
    res <- td$cross_frame
    return(res)
  }
  fit <- function(dframe, ..., weights = NULL, parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::MultinomialOutcomeTreatment$fit")
    fit_transform(dframe = dframe, weights = weights, parallelCluster = parallelCluster)
    invisible(obj) # allow method chaining
  }
  score_frame <- function() {
    res <- get('score_frame', envir = settings$state, inherits = FALSE)
    return(res)
  }
  get_transform <- function() {
    td <- get('transform', envir = settings$state, inherits = FALSE)
    return(res)
  }
  obj$fit = fit
  obj$transform = transform
  obj$fit_transform = fit_transform
  obj$score_frame = score_frame
  obj$get_transform = get_transform
  return(obj)
}


#' vtreat unsupervisedparameters.
#' 
#' A list of settings and values for vtreat unsupervised fitting. 
#' See
#' \code{\link{designTreatmentsZ}}, and
#' \code{\link{prepare.treatmentplan}} for details.
#' 
#' @param user_params list of user overrides.
#' @return filled out parameter list
#' 
#' @export
unsupervised_parameters <- function(user_params = NULL) {
  params = list(
    minFraction = 0.02, 
    rareCount = 0, 
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
  class(merged_params) <- 'unsupervised_parameters'
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
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::UnsupervisedTreatment")
  if((!is.null(params)) && (!('unsupervised_parameters' %in% class(params)))) {
    stop("vtreat::UnsupervisedTreatment extpected class unsupervised_parameters")
  }
  params <- unsupervised_parameters(params)
  settings <- list(
    var_list = var_list,
    cols_to_copy = cols_to_copy,
    params = params,
    imputation_map = imputation_map,
    state = new.env(parent = emptyenv())
  )
  assign("transform", NULL, envir = settings$state)
  assign("score_frame", NULL, envir = settings$state)
  obj <- list(settings = settings)
  class(obj) <- "vtreat_UnsupervisedTreatment"
  fit <- function(dframe, ..., weights = NULL, parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::UnsupervisedTreatment$fit")
    assign("transform", NULL, envir = settings$state)
    assign("score_frame", NULL, envir = settings$state)
    tp <- designTreatmentsZ(
      dframe = dframe,
      varlist = settings$var_list,
      weights = weights,
      minFraction = settings$params$minFraction,
      collarProb = settings$params$collarProb,
      codeRestriction = settings$params$codeRestriction,
      customCoders = settings$params$customCoders, 
      parallelCluster = parallelCluster,
      verbose = settings$params$verbose,
      use_parallel = settings$params$use_parallel,
      missingness_imputation = settings$params$missingness_imputation, 
      imputation_map = settings$params$imputation_map)
    assign("transform", tp, envir = settings$state)
    assign("score_frame", tp$scoreFrame, envir = settings$state)
    invisible(obj) # allow method chaining
  }
  transform <- function(dframe, ..., parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::UnsupervisedTreatment$transform")
    tp <- get('transform', envir = settings$state, inherits = FALSE)
    res <- prepare(
      treatmentplan = tp,
      dframe = dframe,
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
    res <- get('score_frame', envir = settings$state, inherits = FALSE)
    return(res)
  }
  get_transform <- function() {
    td <- get('transform', envir = settings$state, inherits = FALSE)
    return(res)
  }
  obj$fit = fit
  obj$transform = transform
  obj$fit_transform = fit_transform
  obj$score_frame = score_frame
  obj$get_transform = get_transform
  return(obj)
}


