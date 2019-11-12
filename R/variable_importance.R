

#' Return variable evaluations.
#'
#'
#' @param sf scoreFrame from from vtreat treatments
#' @return per-original varaible evaluations
#'
#'
#' @export
#' 
variable_values <- function(sf) {
  res <- data.frame(rsq = tapply(sf$rsq, sf$origName, max))
  res <- cbind(res, data.frame(count = tapply(numeric(nrow(sf))+1, sf$origName, sum)))
  res <- cbind(res, data.frame(sig = tapply(sf$sig, sf$origName, min)))
  res$sig <- pmin(1, res$sig*res$count) # Bonforroni correction
  res$var <- rownames(res)
  rownames(res) <- NULL
  res
}


#' Value variables for prediction a numeric outcome.
#' 
#'  
#' @param dframe Data frame to learn treatments from (training data), must have at least 1 row.
#' @param varlist Names of columns to treat (effective variables).
#' @param outcomename Name of column holding outcome variable. dframe[[outcomename]] must be only finite non-missing values and there must be a cut such that dframe[[outcomename]] is both above the cut at least twice and below the cut at least twice.
#' @param ... no additional arguments, declared to forced named binding of later arguments
#' @param weights optional training weights for each row
#' @param minFraction optional minimum frequency a categorical level must have to be converted to an indicator column.
#' @param smFactor optional smoothing factor for impact coding models.
#' @param rareCount optional integer, allow levels with this count or below to be pooled into a shared rare-level.  Defaults to 0 or off.
#' @param rareSig optional numeric, suppress levels from pooling at this significance value greater.  Defaults to NULL or off.
#' @param collarProb what fraction of the data (pseudo-probability) to collar data at if doCollar is set during \code{\link{prepare.treatmentplan}}.
#' @param scale optional if TRUE replace numeric variables with regression ("move to outcome-scale").
#' @param doCollar optional if TRUE collar numeric variables by cutting off after a tail-probability specified by collarProb during treatment design.
#' @param splitFunction (optional) see vtreat::buildEvalSets .
#' @param ncross optional scalar>=2 number of cross-validation rounds to design.
#' @param forceSplit logical, if TRUE force cross-validated significance calculations on all variables.
#' @param verbose if TRUE print progress.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow.
#' @param use_parallel logical, if TRUE use parallel methods.
#' @param customCoders additional coders to use for variable importance estimate.
#' @param codeRestriction codes to restrict to for variable importance estimate.
#' @param missingness_imputation function of signature f(values: numeric, weights: numeric), simple missing value imputer.
#' @param imputation_map map from column names to functions of signature f(values: numeric, weights: numeric), simple missing value imputers.
#' @return table of variable valuations
#' 
#' @export
#' 
value_variables_N <- function(dframe,varlist,
                              outcomename,
                              ...,
                              weights=c(),
                              minFraction=0.02,smFactor=0.0,
                              rareCount=0,rareSig=1,
                              collarProb=0.00,
                              scale=FALSE,doCollar=FALSE,
                              splitFunction=NULL,ncross=3,
                              forceSplit=FALSE,
                              verbose= FALSE,
                              parallelCluster=NULL,
                              use_parallel = TRUE,
                              customCoders = list('c.PiecewiseV.num' = vtreat::solve_piecewisec,
                                                  'n.PiecewiseV.num' = vtreat::solve_piecewise,
                                                  'c.knearest.num' = vtreat::square_windowc,
                                                  'n.knearest.num' = vtreat::square_window),
                              codeRestriction = c("PiecewiseV", 
                                                  "knearest",
                                                  "clean", "isBAD", "catB", "catP"),
                              missingness_imputation = NULL,
                              imputation_map = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::value_variables_N")
  cfn <- mkCrossFrameNExperiment(
    dframe= dframe,
    varlist = varlist,
    outcomename = outcomename,
    weights=weights,
    minFraction=minFraction,smFactor=smFactor,
    rareCount=rareCount,rareSig=rareSig,
    collarProb=collarProb,
    codeRestriction=codeRestriction,
    customCoders=customCoders,
    scale=scale,doCollar=doCollar,
    splitFunction=splitFunction,ncross=ncross,
    forceSplit=forceSplit,
    verbose= verbose,
    parallelCluster=parallelCluster,
    use_parallel = use_parallel,
    missingness_imputation = missingness_imputation, imputation_map=imputation_map)
  variable_values(cfn$treatments$scoreFrame)
}


#' Value variables for prediction a categorical outcome.
#' 
#' 
#'
#' @param dframe Data frame to learn treatments from (training data), must have at least 1 row.
#' @param varlist Names of columns to treat (effective variables).
#' @param outcomename Name of column holding outcome variable. dframe[[outcomename]] must be only finite non-missing values.
#' @param outcometarget Value/level of outcome to be considered "success",  and there must be a cut such that dframe[[outcomename]]==outcometarget at least twice and dframe[[outcomename]]!=outcometarget at least twice.
#' @param ... no additional arguments, declared to forced named binding of later arguments
#' @param weights optional training weights for each row
#' @param minFraction optional minimum frequency a categorical level must have to be converted to an indicator column.
#' @param smFactor optional smoothing factor for impact coding models.
#' @param rareCount optional integer, allow levels with this count or below to be pooled into a shared rare-level.  Defaults to 0 or off.
#' @param rareSig optional numeric, suppress levels from pooling at this significance value greater.  Defaults to NULL or off.
#' @param collarProb what fraction of the data (pseudo-probability) to collar data at if doCollar is set during \code{\link{prepare.treatmentplan}}.
#' @param scale optional if TRUE replace numeric variables with regression ("move to outcome-scale").
#' @param doCollar optional if TRUE collar numeric variables by cutting off after a tail-probability specified by collarProb during treatment design.
#' @param splitFunction (optional) see vtreat::buildEvalSets .
#' @param ncross optional scalar>=2 number of cross-validation rounds to design.
#' @param forceSplit logical, if TRUE force cross-validated significance calculations on all variables.
#' @param catScaling optional, if TRUE use glm() linkspace, if FALSE use lm() for scaling.
#' @param verbose if TRUE print progress.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow.
#' @param use_parallel logical, if TRUE use parallel methods.
#' @param customCoders additional coders to use for variable importance estimate.
#' @param codeRestriction codes to restrict to for variable importance estimate.
#' @param missingness_imputation function of signature f(values: numeric, weights: numeric), simple missing value imputer.
#' @param imputation_map map from column names to functions of signature f(values: numeric, weights: numeric), simple missing value imputers.
#' @return table of variable valuations
#' 
#' 
#' @export
#' 
value_variables_C <- function(dframe,varlist,
                              outcomename,outcometarget,
                              ...,
                              weights=c(),
                              minFraction=0.02,smFactor=0.0,
                              rareCount=0,rareSig=1,
                              collarProb=0.00,
                              scale=FALSE,doCollar=FALSE,
                              splitFunction=NULL,ncross=3,
                              forceSplit = FALSE,
                              catScaling = TRUE,
                              verbose= FALSE,
                              parallelCluster=NULL,
                              use_parallel = TRUE,
                              customCoders = list('c.PiecewiseV.num' = vtreat::solve_piecewisec,
                                                  'n.PiecewiseV.num' = vtreat::solve_piecewise,
                                                  'c.knearest.num' = vtreat::square_windowc,
                                                  'n.knearest.num' = vtreat::square_window),
                              codeRestriction = c("PiecewiseV", 
                                                  "knearest",
                                                  "clean", "isBAD", "catB", "catP"),
                              missingness_imputation = NULL,
                              imputation_map = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::value_variables_C")
  cfc <- mkCrossFrameCExperiment(
    dframe= dframe,
    varlist = varlist,
    outcomename= outcomename,outcometarget = outcometarget,
    ...,
    weights=weights,
    minFraction=minFraction,smFactor=smFactor,
    rareCount=rareCount,rareSig=rareSig,
    collarProb=collarProb,
    codeRestriction=codeRestriction,
    customCoders=customCoders,
    scale=scale,doCollar=doCollar,
    splitFunction=splitFunction,ncross=ncross,
    forceSplit = forceSplit,
    catScaling=catScaling,
    verbose= verbose,
    parallelCluster=parallelCluster,
    use_parallel = use_parallel,
    missingness_imputation = missingness_imputation, imputation_map=imputation_map)
  variable_values(cfc$treatments$scoreFrame)
}
  