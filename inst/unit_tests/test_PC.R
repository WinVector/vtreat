

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
  RUnit::checkTrue(length(cfe$evalSets)==2)
  xValCount <- vapply(cfe$evalSets,function(ci) { 
    length(unique(d$x[ci$train]))},
    numeric(1))
  RUnit::checkTrue(all(xValCount==1))
  RUnit::checkTrue('clean' %in% cfe$treatments$scoreFrame$code)
  
  invisible(NULL)
}