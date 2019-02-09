
test_Car <- function() {
  dir <- system.file("unit_tests", package = "vtreat", mustWork = TRUE)
  load(paste(dir, 'uci.car.data.Rdata', sep = "/"))

  set.seed(2352)
  # check for non-significance is tricky so repeat a few times.
  p_values <- numeric(0)
  for(rep in seq_len(5)) {
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
    p_values <- c(p_values, min(treatmentsC$scoreFrame$sig[treatmentsC$scoreFrame$origName=='noise']))
    
  }
  RUnit::checkTrue(max(p_values)>0.05)
  
  invisible(NULL)
}
