

test_PC <- function() {
  set.seed(2352)
  d <- data.frame(x=c(1,1,1,1,2,2,2,2),
                  y=c(0,0,1,0,0,1,1,1),
                  stringsAsFactors = FALSE)
  splitter <- makekWayCrossValidationGroupedByColumn('x')
  cfe <- mkCrossFrameNExperiment(d,'x','y',
                                 splitFunction = splitter,
                                 ncross = 2, 
                                 verbose = FALSE)
  expect_true(length(cfe$evalSets)==2)
  xValCount <- vapply(cfe$evalSets,function(ci) { 
    length(unique(d$x[ci$train]))},
    numeric(1))
  expect_true(all(xValCount==1))
  expect_true('clean' %in% cfe$treatments$scoreFrame$code)
  
  invisible(NULL)
}

test_PC()
