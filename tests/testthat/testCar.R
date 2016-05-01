library('vtreat')

context("Simple Example")

test_that("testCar: Numeric Var Scores as expected car", {
  load('uci.car.data.Rdata')
  set.seed(2352)
  dYName <- "rating"
  dYTarget <- 'vgood'
  pvars <- setdiff(colnames(uci.car.data),dYName)
  treatmentsC <- designTreatmentsC(uci.car.data,
                                   pvars,dYName,dYTarget,verbose=FALSE)
  dTrainCTreated <- prepare(treatmentsC,uci.car.data,pruneSig=0.99)
  cvars <- setdiff(colnames(dTrainCTreated),dYName)
  expect_true(max(treatmentsC$scoreFrame$sig)<0.9)
  codes <- sort(unique(treatmentsC$scoreFrame$code))
  expect_true('catB' %in% codes)
  expect_true('lev' %in% codes)
})