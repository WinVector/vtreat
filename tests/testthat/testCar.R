library('vtreat')

context("Simple Example")

test_that("Numeric Var Scores as expected", {
  load('uci.car.data.Rdata')

  dYName <- "rating"
  dYTarget <- 'vgood'
  pvars <- setdiff(colnames(uci.car.data),dYName)
  treatmentsC <- designTreatmentsC(uci.car.data,
                                   pvars,dYName,dYTarget,verbose=FALSE)
  dTrainCTreated <- prepare(treatmentsC,uci.car.data)
  cvars <- setdiff(colnames(dTrainCTreated),dYName)
  
  
  uci.car.data$y <- ifelse(uci.car.data[,dYName]==dYTarget,1,0)
  uci.car.data$w <- 1
})