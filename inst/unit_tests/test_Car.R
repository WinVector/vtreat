
test_Car <- function() {
  dir <- system.file("unit_tests", package = "vtreat", mustWork = TRUE)
  load(paste(dir, 'uci.car.data.Rdata', sep = "/"))

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

  RUnit::checkTrue(min(treatmentsC$scoreFrame$sig[treatmentsC$scoreFrame$origName=='noise'])>0.3)
  
  invisible(NULL)
}
