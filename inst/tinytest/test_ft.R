

test_ft_classification <- function() {
  # From: https://github.com/WinVector/vtreat/blob/master/Examples/Classification/Classification_FT.md
  make_data <- function(nrows) {
    d <- data.frame(x = 5*rnorm(nrows))
    d['y'] = sin(d['x']) + 0.1*rnorm(n = nrows)
    d[4:10, 'x'] = NA                  # introduce NAs
    d['xc'] = paste0('level_', 5*round(d$y/5, 1))
    d['x2'] = rnorm(n = nrows)
    d[d['xc']=='level_-1', 'xc'] = NA  # introduce a NA level
    d['yc'] = d[['y']]>0.5
    d['qq'] = d[['y']]
    return(d)
  }
  
  d = make_data(50)
  
  transform_design = vtreat::BinomialOutcomeTreatment(
    var_list = setdiff(colnames(d), c('y', 'yc', 'qq')),  # columns to transform
    outcome_name = 'yc',                            # outcome variable
    cols_to_copy = c('y', 'yc'),                    # make sure this gets copied
    outcome_target = TRUE                           # outcome of interest
  )
  
  # learn transform from data
  d_prepared <-  transform_design$fit_transform(d)
  
  expect_true('yc' %in% colnames(d_prepared))
  expect_true('y' %in% colnames(d_prepared))
  expect_true(!('qq' %in% colnames(d_prepared)))
  
  # get statistics on the variables
  score_frame <- transform_design$score_frame()
  
  expect_true(!('yc' %in% score_frame$origName))
  expect_true(!('y' %in% score_frame$origName))
  expect_true(!('qq' %in% score_frame$origName))
  
  # check simple xform
  saw_warning <- FALSE
  tryCatch(
    d2 <- transform_design$transform(d),
    warning = function(...) { saw_warning <<- TRUE })
  expect_true(saw_warning)
  dZ <- d
  dZ['zz'] <- 0
  saw_warning <- FALSE
  tryCatch(
    d2 <- transform_design$transform(dZ),
    warning = function(...) { saw_warning <<- TRUE })
  expect_true(!saw_warning)
  
  
  expect_true('yc' %in% colnames(d2))
  expect_true('y' %in% colnames(d2))
  
  d2b <- dZ %.>% transform_design
  expect_true(isTRUE(all.equal(d2, d2b)))

  invisible(NULL)
}

test_ft_classification()


test_ft_regression <- function() {
  # From: https://github.com/WinVector/vtreat/blob/master/Examples/Classification/Classification_FT.md
  make_data <- function(nrows) {
    d <- data.frame(x = 5*rnorm(nrows))
    d['y'] = sin(d['x']) + 0.1*rnorm(n = nrows)
    d[4:10, 'x'] = NA                  # introduce NAs
    d['xc'] = paste0('level_', 5*round(d$y/5, 1))
    d['x2'] = rnorm(n = nrows)
    d[d['xc']=='level_-1', 'xc'] = NA  # introduce a NA level
    d['yc'] = d[['y']]>0.5
    d['qq'] = d[['y']]
    return(d)
  }
  
  d = make_data(50)
  
  transform_design = vtreat::NumericOutcomeTreatment(
    var_list = setdiff(colnames(d), c('y', 'yc', 'qq')),  # columns to transform
    outcome_name = 'y',                             # outcome variable
    cols_to_copy = c('y', 'yc')                     # make sure this gets copied
  )
  
  # learn transform from data
  d_prepared <-  transform_design$fit_transform(d)
  
  expect_true('yc' %in% colnames(d_prepared))
  expect_true('y' %in% colnames(d_prepared))
  expect_true(!('qq' %in% colnames(d_prepared)))
  
  # get statistics on the variables
  score_frame <- transform_design$score_frame()
  
  expect_true(!('yc' %in% score_frame$origName))
  expect_true(!('y' %in% score_frame$origName))
  expect_true(!('qq' %in% score_frame$origName))
  
  # check simple xform
  saw_warning <- FALSE
  tryCatch(
    d2 <- transform_design$transform(d),
    warning = function(...) { saw_warning <<- TRUE })
  expect_true(saw_warning)
  dZ <- d
  dZ['zz'] <- 0
  saw_warning <- FALSE
  tryCatch(
    d2 <- transform_design$transform(dZ),
    warning = function(...) { saw_warning <<- TRUE })
  expect_true(!saw_warning)
  
  expect_true('yc' %in% colnames(d2))
  expect_true('y' %in% colnames(d2))
  
  d2b <- dZ %.>% transform_design
  expect_true(isTRUE(all.equal(d2, d2b)))
  
  invisible(NULL)
}

test_ft_regression()


test_ft_unsupervised <- function() {
  # From: https://github.com/WinVector/vtreat/blob/master/Examples/Classification/Classification_FT.md
  make_data <- function(nrows) {
    d <- data.frame(x = 5*rnorm(nrows))
    d['y'] = sin(d['x']) + 0.1*rnorm(n = nrows)
    d[4:10, 'x'] = NA                  # introduce NAs
    d['xc'] = paste0('level_', 5*round(d$y/5, 1))
    d['x2'] = rnorm(n = nrows)
    d[d['xc']=='level_-1', 'xc'] = NA  # introduce a NA level
    d['yc'] = d[['y']]>0.5
    d['qq'] = d[['y']]
    return(d)
  }
  
  d = make_data(50)
  
  transform_design = vtreat::UnsupervisedTreatment(
    var_list = setdiff(colnames(d), c('y', 'yc', 'qq')),  # columns to transform
    cols_to_copy = c('y', 'yc')                     # make sure this gets copied
  )
  
  # learn transform from data
  d_prepared <-  transform_design$fit_transform(d)
  
  expect_true('yc' %in% colnames(d_prepared))
  expect_true('y' %in% colnames(d_prepared))
  expect_true(!('qq' %in% colnames(d_prepared)))
  
  # get statistics on the variables
  score_frame <- transform_design$score_frame()
  
  expect_true(!('yc' %in% score_frame$origName))
  expect_true(!('y' %in% score_frame$origName))
  expect_true(!('qq' %in% score_frame$origName))
  
  # check simple xform
  saw_warning <- FALSE
  tryCatch(
    d2 <- transform_design$transform(d),
    warning = function(...) { saw_warning <<- TRUE })
  expect_true(!saw_warning)
  
  expect_true('yc' %in% colnames(d2))
  expect_true('y' %in% colnames(d2))
  
  d2b <- d %.>% transform_design
  expect_true(isTRUE(all.equal(d2, d2b)))
  
  invisible(NULL)
}

test_ft_unsupervised()



test_ft_multinomial <- function() {
  # From: https://github.com/WinVector/vtreat/blob/master/Examples/Classification/Classification_FT.md
  make_data <- function(nrows) {
    d <- data.frame(x = 5*rnorm(nrows))
    d['y'] = sin(d['x']) + 0.1*rnorm(n = nrows)
    d[4:10, 'x'] = NA                  # introduce NAs
    d['xc'] = paste0('level_', 5*round(d$y/5, 1))
    d['x2'] = rnorm(n = nrows)
    d[d['xc']=='level_-1', 'xc'] = NA  # introduce a NA level
    d['yc'] = d[['y']]>0.5
    d['qq'] = d[['y']]
    return(d)
  }
  
  d = make_data(50)
  
  transform_design = vtreat::MultinomialOutcomeTreatment(
    var_list = setdiff(colnames(d), c('y', 'yc', 'qq')),  # columns to transform
    outcome_name = 'yc',                             # outcome variable
    cols_to_copy = c('y', 'yc')                     # make sure this gets copied
  )
  
  # learn transform from data
  d_prepared <-  transform_design$fit_transform(d)
  
  expect_true('yc' %in% colnames(d_prepared))
  expect_true('y' %in% colnames(d_prepared))
  expect_true(!('qq' %in% colnames(d_prepared)))
  
  # get statistics on the variables
  score_frame <- transform_design$score_frame()
  
  expect_true(!('yc' %in% score_frame$origName))
  expect_true(!('y' %in% score_frame$origName))
  expect_true(!('qq' %in% score_frame$origName))
  
  # check simple xform
  saw_warning <- FALSE
  tryCatch(
    d2 <- transform_design$transform(d),
    warning = function(...) { saw_warning <<- TRUE })
  expect_true(saw_warning)
  dZ <- d
  dZ['zz'] <- 0
  saw_warning <- FALSE
  tryCatch(
    d2 <- transform_design$transform(dZ),
    warning = function(...) { saw_warning <<- TRUE })
  expect_true(!saw_warning)
  
  
  expect_true('yc' %in% colnames(d2))
  expect_true('y' %in% colnames(d2))
  
  d2b <- dZ %.>% transform_design
  expect_true(isTRUE(all.equal(d2, d2b)))
  
  invisible(NULL)
}

test_ft_multinomial()





test_Rapi_classification <- function() {
  # From: https://github.com/WinVector/vtreat/blob/master/Examples/Classification/Classification_FT.md
  make_data <- function(nrows) {
    d <- data.frame(x = 5*rnorm(nrows))
    d['y'] = sin(d['x']) + 0.1*rnorm(n = nrows)
    d[4:10, 'x'] = NA                  # introduce NAs
    d['xc'] = paste0('level_', 5*round(d$y/5, 1))
    d['x2'] = rnorm(n = nrows)
    d[d['xc']=='level_-1', 'xc'] = NA  # introduce a NA level
    d['yc'] = d[['y']]>0.5
    d['qq'] = d[['y']]
    return(d)
  }
  
  d = make_data(50)
  
  transform_design = vtreat::mkCrossFrameCExperiment(
    dframe = d,                                    # data to learn transform from
    varlist = setdiff(colnames(d), c('y', 'yc', 'qq')),  # columns to transform
    outcomename = 'yc',                            # outcome variable
    outcometarget = TRUE,                           # outcome of interest
    verbose = FALSE
  )
  
  # learn transform from data
  d_prepared <-  transform_design$crossFrame
  
  expect_true('yc' %in% colnames(d_prepared))
  expect_true(!('y' %in% colnames(d_prepared)))
  expect_true(!('qq' %in% colnames(d_prepared)))
  
  # get statistics on the variables
  score_frame <- transform_design$treatments$scoreFrame
  
  expect_true(!('yc' %in% score_frame$origName))
  expect_true(!('y' %in% score_frame$origName))
  expect_true(!('qq' %in% score_frame$origName))
  
  # check simple xform
  saw_warning <- FALSE
  tryCatch(
    d2 <- prepare(transform_design$treatments, d),
    warning = function(...) { saw_warning <<- TRUE })
  expect_true(saw_warning)
  dZ <- d
  dZ['zz'] <- 0
  saw_warning <- FALSE
  tryCatch(
    d2 <- prepare(transform_design$treatments, dZ),
    warning = function(...) { saw_warning <<- TRUE })
  expect_true(!saw_warning)
  
  
  expect_true('yc' %in% colnames(d2))
  expect_true(!('y' %in% colnames(d2)))
  
  d2b <- dZ %.>% transform_design$treatments
  expect_true(isTRUE(all.equal(d2, d2b)))
  
  invisible(NULL)
}

test_Rapi_classification()


