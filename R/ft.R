
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
#' Please see
#' \url{https://github.com/WinVector/vtreat/blob/master/Examples/fit_transform/fit_transform_api.md},
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
    trackedValues = NULL,
    check_for_duplicate_frames = TRUE)
  merged_params <- merge_params(params = params, 
                                user_params = user_params)
  class(merged_params) <- 'classification_parameters'
  return(merged_params)
}


#' Stateful object for designing and applying binomial outcome treatments.
#' 
#' Hold settings and results for binomial classification data preparation.
#' 
#' Please see
#' \url{https://github.com/WinVector/vtreat/blob/master/Examples/fit_transform/fit_transform_api.md},
#' \code{\link{mkCrossFrameCExperiment}}, 
#' \code{\link{designTreatmentsC}}, and
#' \code{\link{prepare.treatmentplan}} for details.
#' 
#' @param ... not used, force arguments to be specified by name.
#' @param var_list Names of columns to treat (effective variables).
#' @param outcome_name Name of column holding outcome variable. \code{dframe[[outcomename]]} must be only finite and non-missing values.
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
                                     outcome_target = TRUE,
                                     cols_to_copy = NULL,
                                     params = NULL,
                                     imputation_map = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::BinomialOutcomeTreatment")
  if((!is.null(params)) && (!('classification_parameters' %in% class(params)))) {
    stop("vtreat::BinomialOutcomeTreatment expected class classification_parameters")
  }
  if(missing(outcome_name)) {
    stop("vtreat::BinomialOutcomeTreatment outcome_name is required")
  }
  if(!is.character(outcome_name)) {
    stop("vtreat::BinomialOutcomeTreatment outcome_name must be character class")
  }
  if(length(outcome_name) != 1) {
    stop("vtreat::BinomialOutcomeTreatment outcome_name must be length 1")
  }
  params <- classification_parameters(params)
  var_list <- setdiff(var_list, c(outcome_name, cols_to_copy))
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
  class(obj) <- "vtreat_pipe_step"
  obj$treatment_type <- "BinomialOutcomeTreatment"
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
    tp <- mget('transform', envir = settings$state, inherits = FALSE,
               ifnotfound = list('transform' = NULL))[[1]]
    if(is.null(tp)) {
      stop("tried to use transform() on a not-fit treatment")
    }
    if(isTRUE(settings$params$check_for_duplicate_frames)) {
      old_obj_id <- tp$fit_obj_id
      fit_obj_id <- NULL
      if(!is.null(old_obj_id)) {
        fit_obj_id <- id_f(dframe)
      }
      if(!is.null(fit_obj_id)) {
        if(fit_obj_id == old_obj_id) {
          warning("possibly called transform() on same data frame as fit(), this can lead to over-fit.  To avoid this, please use fit_transform().")
        }
      }
    }
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
      use_parallel = settings$params$use_parallel,
      check_for_duplicate_frames = FALSE)
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
  get_score_frame <- function() {
    res <- get('score_frame', envir = settings$state, inherits = FALSE)
    return(res)
  }
  get_transform <- function() {
    res <- get('transform', envir = settings$state, inherits = FALSE)
    return(res)
  }
  get_feature_names <- function(input_features=NULL) {
    sf <- get('score_frame', envir = settings$state, inherits = FALSE)
    want <- sf$varMoves
    if(!is.null(input_features)) {
      want <- want & (sf$origName %in% input_features)
    }
    return(sf$varName[want])
  }
  fresh_copy <- function() {
    BinomialOutcomeTreatment(
      var_list = settings$var_list,
      outcome_name = settings$outcome_name,
      outcome_target = settings$outcome_target,
      cols_to_copy = settings$cols_to_copy,
      params = settings$params,
      imputation_map = settings$imputation_map)
  }
  # get globalenv early on environment chain for seralization
  # See pseudo-SEXPTYPEs in https://cran.r-project.org/doc/manuals/r-release/R-ints.html
  f_env <- new.env(parent = globalenv())
  assign("settings", settings, envir = f_env)
  for(nm in c("fit", "transform", "fit_transform",
              "get_score_frame", "get_transform", "get_feature_names",
              "fresh_copy")) {
    fi <- get(nm)
    environment(fi) <- f_env
    assign(nm, fi, envir = f_env)
  }
  # build up result object
  obj$fit = fit
  obj$transform = transform
  obj$fit_transform = fit_transform
  obj$score_frame = get_score_frame
  obj$get_score_frame = get_score_frame
  obj$get_transform = get_transform
  obj$get_feature_names = get_feature_names
  obj$fresh_copy = fresh_copy
  assign("obj", obj, envir = f_env)
  return(obj)
}


#' vtreat regression parameters.
#' 
#' A list of settings and values for vtreat regression fitting. 
#' Please see
#' \url{https://github.com/WinVector/vtreat/blob/master/Examples/fit_transform/fit_transform_api.md},
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
    trackedValues = NULL,
    check_for_duplicate_frames = TRUE)
  merged_params <- merge_params(params = params, 
                                user_params = user_params)
  class(merged_params) <- 'regression_parameters'
  return(merged_params)
}


#' Stateful object for designing and applying numeric outcome treatments.
#' 
#' Hold settings and results for regression data preparation.
#' 
#' Please see
#' \url{https://github.com/WinVector/vtreat/blob/master/Examples/fit_transform/fit_transform_api.md},
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
    stop("vtreat::NumericOutcomeTreatment expected class regression_parameters")
  }
  if(missing(outcome_name)) {
    stop("vtreat::NumericOutcomeTreatment outcome_name is required")
  }
  if(!is.character(outcome_name)) {
    stop("vtreat::NumericOutcomeTreatment outcome_name must be character class")
  }
  if(length(outcome_name) != 1) {
    stop("vtreat::NumericOutcomeTreatment outcome_name must be length 1")
  }
  params <- regression_parameters(params)
  var_list <- setdiff(var_list, c(outcome_name, cols_to_copy))
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
  class(obj) <- "vtreat_pipe_step"
  obj$treatment_type <- "NumericOutcomeTreatment"
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
    tp <- mget('transform', envir = settings$state, inherits = FALSE,
               ifnotfound = list('transform' = NULL))[[1]]
    if(is.null(tp)) {
      stop("tried to use transform() on a not-fit treatment")
    }
    if(isTRUE(settings$params$check_for_duplicate_frames)) {
      old_obj_id <- tp$fit_obj_id
      fit_obj_id <- NULL
      if(!is.null(old_obj_id)) {
        fit_obj_id <- id_f(dframe)
      }
      if(!is.null(fit_obj_id)) {
        if(fit_obj_id == old_obj_id) {
          warning("possibly called transform() on same data frame as fit(), this can lead to over-fit.  To avoid this, please use fit_transform().")
        }
      }
    }
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
      use_parallel = settings$params$use_parallel,
      check_for_duplicate_frames = FALSE)
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
  get_score_frame <- function() {
    res <- get('score_frame', envir = settings$state, inherits = FALSE)
    return(res)
  }
  get_transform <- function() {
    res <- get('transform', envir = settings$state, inherits = FALSE)
    return(res)
  }
  get_feature_names <- function(input_features=NULL) {
    sf <- get('score_frame', envir = settings$state, inherits = FALSE)
    want <- sf$varMoves
    if(!is.null(input_features)) {
      want <- want & (sf$origName %in% input_features)
    }
    return(sf$varName[want])
  }
  fresh_copy <- function() {
    NumericOutcomeTreatment(
      var_list = settings$var_list,
      outcome_name = settings$outcome_name,
      cols_to_copy = settings$cols_to_copy,
      params = settings$params,
      imputation_map = settings$imputation_map)
  }
  # get globalenv early on environment chain for seralization
  # See pseudo-SEXPTYPEs in https://cran.r-project.org/doc/manuals/r-release/R-ints.html
  f_env <- new.env(parent = globalenv())
  assign("settings", settings, envir = f_env)
  for(nm in c("fit", "transform", "fit_transform",
              "get_score_frame", "get_transform", "get_feature_names",
              "fresh_copy")) {
    fi <- get(nm)
    environment(fi) <- f_env
    assign(nm, fi, envir = f_env)
  }
  # build up result object
  obj$fit = fit
  obj$transform = transform
  obj$fit_transform = fit_transform
  obj$score_frame = get_score_frame
  obj$get_score_frame = get_score_frame
  obj$get_transform = get_transform
  obj$get_feature_names = get_feature_names
  obj$fresh_copy = fresh_copy
  assign("obj", obj, envir = f_env)
  return(obj)
}


#' vtreat multinomial parameters.
#' 
#' A list of settings and values for vtreat multinomial classification fitting. 
#' Please see
#' \url{https://github.com/WinVector/vtreat/blob/master/Examples/fit_transform/fit_transform_api.md},
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
    imputation_map = NULL,
    check_for_duplicate_frames = TRUE)
  merged_params <- merge_params(params = params, 
                                user_params = user_params)
  class(merged_params) <- 'multinomial_parameters'
  return(merged_params)
}


#' Stateful object for designing and applying multinomial outcome treatments.
#' 
#' Hold settings and results for multinomial classification data preparation.
#' 
#' Please see
#' \url{https://github.com/WinVector/vtreat/blob/master/Examples/fit_transform/fit_transform_api.md},
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
    stop("vtreat::MultinomialOutcomeTreatment expected class multinomial_parameters")
  }
  if(missing(outcome_name)) {
    stop("vtreat::MultinomialOutcomeTreatment outcome_name is required")
  }
  if(!is.character(outcome_name)) {
    stop("vtreat::MultinomialOutcomeTreatment outcome_name must be character class")
  }
  if(length(outcome_name) != 1) {
    stop("vtreat::MultinomialOutcomeTreatment outcome_name must be length 1")
  }
  params <- multinomial_parameters(params)
  var_list <- setdiff(var_list, c(outcome_name, cols_to_copy))
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
  class(obj) <- "vtreat_pipe_step"
  obj$treatment_type <- "MultinomialOutcomeTreatment"
  transform <- function(dframe, ..., parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::MultinomialOutcomeTreatment$transform")
    tp <- mget('transform', envir = settings$state, inherits = FALSE,
               ifnotfound = list('transform' = NULL))[[1]]
    if(is.null(tp)) {
      stop("tried to use transform() on a not-fit treatment")
    }
    if(isTRUE(settings$params$check_for_duplicate_frames)) {
      old_obj_id <- tp$fit_obj_id
      fit_obj_id <- NULL
      if(!is.null(old_obj_id)) {
        fit_obj_id <- id_f(dframe)
      }
      if(!is.null(fit_obj_id)) {
        if(fit_obj_id == old_obj_id) {
          warning("possibly called transform() on same data frame as fit(), this can lead to over-fit.  To avoid this, please use fit_transform().")
        }
      }
    }
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
      use_parallel= settings$params$use_parallel,
      check_for_duplicate_frames = FALSE)
    return(res)
  }
  fit_transform <- function(dframe, ..., weights = NULL, parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::MultinomialOutcomeTreatment$fit_transform")
    assign("transform", NULL, envir = settings$state)
    assign("score_frame", NULL, envir = settings$state)
    td <- mkCrossFrameMExperiment(
      dframe = dframe,
      varlist = settings$var_list,
      outcomename = settings$outcome_name, 
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
    for(c in settings$cols_to_copy) {
      res[[c]] <- dframe[[c]]
    }
    return(res)
  }
  fit <- function(dframe, ..., weights = NULL, parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::MultinomialOutcomeTreatment$fit")
    fit_transform(dframe = dframe, weights = weights, parallelCluster = parallelCluster)
    invisible(obj) # allow method chaining
  }
  get_score_frame <- function() {
    res <- get('score_frame', envir = settings$state, inherits = FALSE)
    return(res)
  }
  get_transform <- function() {
    res <- get('transform', envir = settings$state, inherits = FALSE)
    return(res)
  }
  get_feature_names <- function(input_features=NULL) {
    sf <- get('score_frame', envir = settings$state, inherits = FALSE)
    want <- sf$varMoves
    if(!is.null(input_features)) {
      want <- want & (sf$origName %in% input_features)
    }
    return(unique(sf$varName[want]))
  }
  fresh_copy <- function() {
    MultinomialOutcomeTreatment(
      var_list = settings$var_list,
      outcome_name = settings$outcome_name,
      cols_to_copy = settings$cols_to_copy,
      params = settings$params,
      imputation_map = settings$imputation_map)
  }
  # get globalenv early on environment chain for seralization
  # See pseudo-SEXPTYPEs in https://cran.r-project.org/doc/manuals/r-release/R-ints.html
  f_env <- new.env(parent = globalenv())
  assign("settings", settings, envir = f_env)
  for(nm in c("fit", "transform", "fit_transform",
              "get_score_frame", "get_transform", "get_feature_names",
              "fresh_copy")) {
    fi <- get(nm)
    environment(fi) <- f_env
    assign(nm, fi, envir = f_env)
  }
  # build up result object
  obj$fit = fit
  obj$transform = transform
  obj$fit_transform = fit_transform
  obj$score_frame = get_score_frame
  obj$get_score_frame = get_score_frame
  obj$get_transform = get_transform
  obj$get_feature_names = get_feature_names
  obj$fresh_copy = fresh_copy
  assign("obj", obj, envir = f_env)
  return(obj)
}


#' vtreat unsupervised parameters.
#' 
#' A list of settings and values for vtreat unsupervised fitting. 
#' Please see
#' \url{https://github.com/WinVector/vtreat/blob/master/Examples/fit_transform/fit_transform_api.md},
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
#' Hold settings and results for unsupervised data preparation.
#' 
#' Please see
#' \url{https://github.com/WinVector/vtreat/blob/master/Examples/fit_transform/fit_transform_api.md},
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
    stop("vtreat::UnsupervisedTreatment expected class unsupervised_parameters")
  }
  params <- unsupervised_parameters(params)
  var_list <- setdiff(var_list, cols_to_copy)
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
  class(obj) <- "vtreat_pipe_step"
  obj$treatment_type <- "UnsupervisedTreatment"
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
    tp <- mget('transform', envir = settings$state, inherits = FALSE,
               ifnotfound = list('transform' = NULL))[[1]]
    if(is.null(tp)) {
      stop("tried to use transform() on a not-fit treatment")
    }
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
      use_parallel = settings$params$use_parallel,
      check_for_duplicate_frames = FALSE)
    return(res)
  }
  fit_transform <- function(dframe, ..., weights = NULL, parallelCluster = NULL) {
    wrapr::stop_if_dot_args(substitute(list(...)), 
                            "vtreat::UnsupervisedTreatment$fit_transform")
    fit(dframe = dframe, weights = weights, parallelCluster = parallelCluster)
    res <- transform(dframe = dframe, parallelCluster = parallelCluster)
    return(res)
  }
  get_score_frame <- function() {
    res <- get('score_frame', envir = settings$state, inherits = FALSE)
    return(res)
  }
  get_transform <- function() {
    res <- get('transform', envir = settings$state, inherits = FALSE)
    return(res)
  }
  get_feature_names <- function(input_features=NULL) {
    sf <- get('score_frame', envir = settings$state, inherits = FALSE)
    want <- sf$varMoves
    if(!is.null(input_features)) {
      want <- want & (sf$origName %in% input_features)
    }
    return(sf$varName[want])
  }
  fresh_copy <- function() {
    UnsupervisedTreatment(
      var_list = settings$var_list,
      cols_to_copy = settings$cols_to_copy,
      params = settings$params,
      imputation_map = settings$imputation_map)
  }
  # get globalenv early on environment chain for seralization
  # See pseudo-SEXPTYPEs in https://cran.r-project.org/doc/manuals/r-release/R-ints.html
  f_env <- new.env(parent = globalenv())
  assign("settings", settings, envir = f_env)
  for(nm in c("fit", "transform", "fit_transform",
              "get_score_frame", "get_transform", "get_feature_names",
              "fresh_copy")) {
    fi <- get(nm)
    environment(fi) <- f_env
    assign(nm, fi, envir = f_env)
  }
  # build up result object
  obj$fit = fit
  obj$transform = transform
  obj$fit_transform = fit_transform
  obj$score_frame = get_score_frame
  obj$get_score_frame = get_score_frame
  obj$get_transform = get_transform
  obj$get_feature_names = get_feature_names
  obj$fresh_copy = fresh_copy
  assign("obj", obj, envir = f_env)
  return(obj)
}


#' @export
format.vtreat_pipe_step <- function(x, ...) {
  return(x$treatment_type)
}


#' @export
as.character.vtreat_pipe_step <- function(x, ...) {
  return(format(x, ...))
}


#' @export
print.vtreat_pipe_step <- function(x, ...) {
  print(format(x, ...))
  sf <- x$score_frame()
  if(!is.null(sf)) {
    cols <- c('origName', 'varName', 'code', 'rsq', 'sig', 'extraModelDegrees', 'recommended')
    cols <- intersect(cols, colnames(sf))
    sf <- sf[, cols, drop = FALSE]
    if(!is.null(sf)) {
      sf <- sf[order(sf$origName, sf$varName), , drop = FALSE]
      rownames(sf) <- NULL
      print(sf)
    }
  }
  invisible(x)
}


#' @export
apply_right.vtreat_pipe_step <- function(pipe_left_arg,
                                         pipe_right_arg,
                                         pipe_environment,
                                         left_arg_name,
                                         pipe_string,
                                         right_arg_name) {
  pipe_right_arg$transform(pipe_left_arg)
}



# S3 interface, immutable to be more R-like


#' Fit first arguemnt to data in second argument.
#' 
#' Update the state of first argument to have learned or fit from second argument.
#' 
#' Note: input vps is not altered, fit is in returned value.
#' 
#' @param vps vtreat pipe step, object specifying fit
#' @param dframe data.frame, data to fit from.
#' @param ... not used, forces later arguments to bind by name.
#' @param weights optional, per-dframe data weights.
#' @param parallelCluster optional, parallel cluster to run on.
#' @return new fit object
#' 
#' @export
fit <- function(vps, dframe, ..., weights = NULL, parallelCluster = NULL) {
  UseMethod("fit")
}

#' @export
fit.vtreat_pipe_step <- function(vps, dframe, ..., weights = NULL, parallelCluster = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::fit.vtreat_pipe_step")
  vps <- vps$fresh_copy()
  vps$fit(dframe = dframe, weights = weights, parallelCluster = parallelCluster)
}


#' Transform second argument by first.
#' 
#' Apply first argument to second as a transform.
#' 
#' @param vps vtreat pipe step, object defining transform.
#' @param dframe data.frame, data to transform
#' @param ... not used, forces later arguments to bind by name.
#' @param parallelCluster optional, parallel cluster to run on.
#' @return transformed dframe
#' 
#' @export
apply_transform <- function(vps, dframe, ..., parallelCluster = NULL) {
  # don't use transform name to stay out of base::transform's way.
  UseMethod("apply_transform")
}

#' @export
apply_transform.vtreat_pipe_step <- function(vps, dframe, ..., parallelCluster = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::apply_transform.vtreat_pipe_step")
  vps$transform(dframe = dframe, parallelCluster = parallelCluster)
}

#' @export
prepare.vtreat_pipe_step <- function(treatmentplan, dframe, ...) {
  # vtreat prepare interface
  treatmentplan$transform(dframe = dframe, ...)
}


#' Fit and transform in a cross-validated manner.
#' 
#' Update the state of first argument to have learned or fit from second argument, and compute a cross
#' validated example of such a transform.
#' 
#' Note: input vps is not altered, fit is in returned list.
#'
#' @param vps vtreat pipe step, object specifying fit.
#' @param dframe data.frame, data to fit from.
#' @param ... not used, forces later arguments to bind by name.
#' @param weights optional, per-dframe data weights.
#' @param parallelCluster optional, parallel cluster to run on.
#' @return @return named list containing: treatments and cross_frame
#' 
#' @export
fit_transform <- function(vps, dframe, ..., weights = NULL, parallelCluster = NULL) {
  UseMethod("fit_transform")
}

#' @export
fit_transform.vtreat_pipe_step <- function(vps, dframe, ..., weights = NULL, parallelCluster = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::fit_transform.vtreat_pipe_step") 
  vps <- vps$fresh_copy()
  cross_frame <- vps$fit_transform(dframe = dframe, weights = weights, parallelCluster = parallelCluster)
  list(treatments = vps, cross_frame = cross_frame)
}


#' Fit and prepare in a cross-validated manner.
#' 
#' Update the state of first argument to have learned or fit from second argument, and compute a cross
#' validated example of such a transform.
#' 
#' Note: input vps is not altered, fit is in returned list.
#'
#' @param vps vtreat pipe step, object specifying fit.
#' @param dframe data.frame, data to fit from.
#' @param ... not used, forces later arguments to bind by name.
#' @param weights optional, per-dframe data weights.
#' @param parallelCluster optional, parallel cluster to run on.
#' @return @return named list containing: treatments and cross_frame
#' 
#' @export
fit_prepare <- function(vps, dframe, ..., weights = NULL, parallelCluster = NULL) {
  UseMethod("fit_transform")
}

#' @export
fit_prepare.vtreat_pipe_step <- function(vps, dframe, ..., weights = NULL, parallelCluster = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::fit_prepare.vtreat_pipe_step") 
  vps <- vps$fresh_copy()
  cross_frame <- vps$fit_transform(dframe = dframe, weights = weights, parallelCluster = parallelCluster)
  list(treatments = vps, cross_frame = cross_frame)
}


#' Return score frame from vps.
#' 
#' Return previously fit score frame.
#' 
#' @param vps vtreat pipe step, mutable object to read from.
#' @return score frame
#' 
#' @export
get_score_frame <- function(vps) {
  UseMethod("get_score_frame")
}

#' @export
get_score_frame.vtreat_pipe_step <- function(vps) {
  vps$get_score_frame()
}


#' Return underlying transform from vps.
#' 
#' Return previously fit transform.
#' 
#' @param vps vtreat pipe step, mutable object to read from.
#' @return transform
#' 
#' @export
get_transform <- function(vps) {
  UseMethod("get_transform")
}

#' @export
get_transform.vtreat_pipe_step <- function(vps) {
  vps$get_transform()
}


#' Return feasible feature names.
#' 
#' Return previously fit feature names.
#' 
#' @param vps vtreat pipe step, mutable object to read from.
#' @return feature names
#' 
#' @export
get_feature_names <- function(vps) {
  UseMethod("get_feature_names")
}

#' @export
get_feature_names.vtreat_pipe_step <- function(vps) {
  vps$get_feature_names()
}


