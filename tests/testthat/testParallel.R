library('vtreat')

context("Parallel Example")

test_that("Parallel works", {
  cl <- NULL
  if(requireNamespace("parallel",quietly=TRUE)) {
     cl <- parallel::makeCluster(4)
  }
  
  load('uci.car.data.Rdata')
  
  dYName <- "rating"
  dYTarget <- 'vgood'
  pvars <- setdiff(colnames(uci.car.data),dYName)
  treatmentsCP <- designTreatmentsC(uci.car.data,
                                   pvars,dYName,dYTarget,verbose=FALSE,
                                   parallelCluster=cl)
  dTrainCTreatedP <- prepare(treatmentsCP,uci.car.data)
  if(!is.null(cl)) {
    parallel::stopCluster(cl)
    cl <- NULL
  }
  treatmentsC <- designTreatmentsC(uci.car.data,
                                    pvars,dYName,dYTarget,verbose=FALSE)
  dTrainCTreated <- prepare(treatmentsC,uci.car.data)
  expect_true(nrow(dTrainCTreated)==nrow(dTrainCTreatedP))
  expect_true(length(colnames(dTrainCTreated))==length(colnames(dTrainCTreatedP)))
  expect_true(all(colnames(dTrainCTreated)==colnames(dTrainCTreatedP)))
  for(v in setdiff(colnames(dTrainCTreated),dYName)) {
    ev <- max(abs(dTrainCTreated[[v]]-dTrainCTreatedP[[v]]))
    expect_true(ev<1.0e-3)
  }
})