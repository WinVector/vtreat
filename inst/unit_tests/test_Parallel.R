
test_Parallel <- function() {
  # seems to kill testthat on stop, possibly https://github.com/hadley/testthat/issues/129
  Sys.setenv("R_TESTS" = "")

  dir <- system.file("unit_tests", package = "vtreat", mustWork = TRUE)
  load(paste(dir, 'uci.car.data.Rdata', sep = "/"))
  cl <- NULL
  if(requireNamespace("parallel",quietly=TRUE)) {
    cl <- parallel::makeCluster(2)
  }
  
  dYName <- "rating"
  dYTarget <- 'vgood'
  pvars <- setdiff(colnames(uci.car.data),dYName)
  seedVal=946463L
  set.seed(seedVal)
  treatmentsCP <- designTreatmentsC(uci.car.data,
                                   pvars,dYName,dYTarget,verbose=FALSE,
                                   parallelCluster=cl)
  dTrainCTreatedP <- prepare(treatmentsCP,uci.car.data,pruneSig=c(),
                             parallelCluster=cl, check_for_duplicate_frames=FALSE)
  if(!is.null(cl)) {
    parallel::stopCluster(cl)
    cl <- NULL
  }
  set.seed(seedVal)
  treatmentsC <- designTreatmentsC(uci.car.data,
                                    pvars,dYName,dYTarget,verbose=FALSE)
  dTrainCTreated <- prepare(treatmentsC,uci.car.data,pruneSig=c(), check_for_duplicate_frames=FALSE)
  
  RUnit::checkTrue(nrow(dTrainCTreated)==nrow(dTrainCTreatedP))
  RUnit::checkTrue(length(colnames(dTrainCTreated))==length(colnames(dTrainCTreatedP)))
  RUnit::checkTrue(all(colnames(dTrainCTreated)==colnames(dTrainCTreatedP)))
  for(v in setdiff(colnames(dTrainCTreated),dYName)) {
    ev <- max(abs(dTrainCTreated[[v]]-dTrainCTreatedP[[v]]))
    RUnit::checkTrue(ev<1.0e-3)
  }
  
  invisible(NULL)
}
