
test_imputation_control <- function() {
  
  d = data.frame(
    "x" = c(0, 1, 1000, NA),
    "w" = c(3, 6, NA, 100),
    "y" = c(0, 0, 1, 1)
  )
  
  
  # NULL for global_val or map means "use default"
  
  check_unsupervised = function(d, global_val, map) {
    newparams = unsupervised_parameters(list(missingness_imputation=global_val))
    
    transform = UnsupervisedTreatment(
      var_list = c('x','w'),
      cols_to_copy = 'y',
      params = newparams,
      imputation_map = map
    )
    
    # use the fit().prepare() path
    d_treated = fit(transform, d) %.>%
      prepare(., d)
    
    d_treated
    
  }
  
  check_classification = function(d, global_val, map, useFT=TRUE) {
    newparams = classification_parameters(
      list(
        missingness_imputation=global_val,
        check_for_duplicate_frames = FALSE # shut the warning up
      )
    )
    
    transform = BinomialOutcomeTreatment(
      var_list = c('x','w'),
      outcome_name = 'y',
      outcome_target = 1,
      params = newparams,
      imputation_map = map
    )
    
    if(useFT) {
      unpack[treatments = treatments] <-
        fit_prepare(transform, d)
      d_treated = prepare(treatments,d)
    } else {
      # use the fit().prepare() path
      d_treated = fit(transform, d) %.>%
        prepare(., d)
    }
    
    d_treated
    
  }
  
  check_regression = function(d, global_val, map, useFT=TRUE) {
    newparams = regression_parameters(
      list(
        missingness_imputation=global_val,
        check_for_duplicate_frames = FALSE # shut the warning up
      )
    )
    
    transform = NumericOutcomeTreatment(
      var_list = c('x','w'),
      outcome_name = 'y',
      params = newparams,
      imputation_map = map
    )
    
    if(useFT) {
      unpack[treatments = treatments] <-
        fit_prepare(transform, d)
      d_treated = prepare(treatments,d)
    } else {
      # use the fit().prepare() path
      d_treated = fit(transform, d) %.>%
        prepare(., d)
    }
    d_treated
  }
  
  equal_df = function(a, b, tolerance = 0.1) {
    isTRUE(all.equal(a, b, tolerance = tolerance))
  }
  
  check_all = function(d, global_val, map, gold_standard=NULL) {
    # unsupervised is the gold standard
    c0 = check_unsupervised(d, global_val, map)
    if(!is.null(gold_standard)) {
      RUnit::checkTrue(equal_df(c0, gold_standard))
    } else {
      gold_standard = c0
    }
    
    # classification
    c1 = check_classification(d, global_val, map)
    RUnit::checkTrue(equal_df(c1, gold_standard))
    
    c2 = check_classification(d, global_val, map, useFT=FALSE)
    RUnit::checkTrue(equal_df(c2, gold_standard))
    
    # regression
    r1 = check_regression(d, global_val, map)
    RUnit::checkTrue(equal_df(r1, gold_standard))
    
    r2 = check_regression(d, global_val, map, useFT=FALSE)
    RUnit::checkTrue(equal_df(r2, gold_standard))
    invisible(gold_standard)
  }
  
  
  global_imp = NULL
  imp_map = NULL
  
  gs <- wrapr::build_frame(
    "x"    , "x_isBAD", "w"  , "w_isBAD", "y" |
      0    , 0        , 3    , 0        , 0   |
      1    , 0        , 6    , 0        , 0   |
      1000 , 0        , 36.33, 1        , 1   |
      333.7, 1        , 100  , 0        , 1   )
  check_all(d, global_imp, imp_map, gold_standard = gs)
  
  median2 <- function(x, wts) {
    median(x)
  }
  
  global_imp = median2
  imp_map = NULL
  check_all(d, global_imp, imp_map)
  
  
  global_imp = -1
  imp_map = NULL
  check_all(d, global_imp, imp_map)
  
  
  max2 <- function(x, wts) {
    max(x)
  }
  
  global_imp = NULL
  imp_map = list(
    x = max2,
    w = 0
  )
  
  check_all(d, global_imp, imp_map)
  
  
  global_imp = -1
  imp_map = list(
    x = max2
  )
  
  check_all(d, global_imp, imp_map)
  
  
  global_imp = NULL
  
  imp_map = list(
    x = max2
  )
  
  check_all(d, global_imp, imp_map)
  
  invisible(NULL)
}
  
  