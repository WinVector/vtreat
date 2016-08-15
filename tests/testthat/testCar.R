library('vtreat')

context("Simple Example")

test_that("testCar: Numeric Var Scores as expected car", {
  load('uci.car.data.Rdata')
  set.seed(2352)
  uci.car.data$noise <- sample(paste0('v',1:100),
                               nrow(uci.car.data),
                               replace=TRUE)
  dYName <- "rating"
  dYTarget <- 'vgood'
  pvars <- setdiff(colnames(uci.car.data),dYName)
  treatmentsC <- designTreatmentsC(uci.car.data,
                                   pvars,dYName,dYTarget,verbose=FALSE)
  dTrainCTreated <- prepare(treatmentsC,uci.car.data,pruneSig=0.5)
  cvars <- setdiff(colnames(dTrainCTreated),dYName)
  expect_true(min(treatmentsC$scoreFrame$sig[treatmentsC$scoreFrame$origName=='noise'])>0.3)
})