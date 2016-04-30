library('vtreat')

context("Parallel Example")

test_that("testParallel: Parallel works", {
  # load('tests/testthat/uci.car.data.Rdata')
  load('uci.car.data.Rdata')
  cl <- NULL
  # This kills build check as I think you are not allowed to spawn.
  #if(requireNamespace("parallel",quietly=TRUE)) {
  #   cl <- parallel::makeCluster(4)
  #}
  
  dYName <- "rating"
  dYTarget <- 'vgood'
  pvars <- setdiff(colnames(uci.car.data),dYName)
  seedVal=946463L
  set.seed(seedVal)
  treatmentsCP <- designTreatmentsC(uci.car.data,
                                   pvars,dYName,dYTarget,verbose=FALSE,
                                   parallelCluster=cl)
  dTrainCTreatedP <- prepare(treatmentsCP,uci.car.data,pruneSig=c(),
                             parallelCluster=cl)
  if(!is.null(cl)) {
    parallel::stopCluster(cl)
    cl <- NULL
  }
  set.seed(seedVal)
  treatmentsC <- designTreatmentsC(uci.car.data,
                                    pvars,dYName,dYTarget,verbose=FALSE)
  dTrainCTreated <- prepare(treatmentsC,uci.car.data,pruneSig=c())
  
  expect_true(nrow(dTrainCTreated)==nrow(dTrainCTreatedP))
  expect_true(length(colnames(dTrainCTreated))==length(colnames(dTrainCTreatedP)))
  expect_true(all(colnames(dTrainCTreated)==colnames(dTrainCTreatedP)))
  for(v in setdiff(colnames(dTrainCTreated),dYName)) {
    ev <- max(abs(dTrainCTreated[[v]]-dTrainCTreatedP[[v]]))
    expect_true(ev<1.0e-3)
  }
})