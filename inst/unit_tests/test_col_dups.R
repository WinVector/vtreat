
test_col_dups <- function() {
  d <- data.frame(x = c(1:8, NA, NA), y = c(1,1,1,1,1,0,0,0,0,0))
  cross_frame_experiment <- vtreat::mkCrossFrameCExperiment(
    d, 
    varlist = "x", 
    outcomename = "y", 
    outcometarget = 1,
    verbose = FALSE,
    scale = TRUE)
  
  dTrainAll_treated <- cross_frame_experiment$crossFrame
  RUnit::checkTrue(length(colnames(dTrainAll_treated))==length(unique(colnames(dTrainAll_treated))))
  RUnit::checkTrue(!isTRUE(any(is.na(dTrainAll_treated$x))))
  
  treatment_plan <- cross_frame_experiment$treatments
  
  dTest_treated <- prepare(treatment_plan, 
                           d,
                           scale = TRUE)
  
  dTest_treated
  RUnit::checkTrue(length(colnames(dTest_treated))==length(unique(colnames(dTest_treated))))
  RUnit::checkTrue(!isTRUE(any(is.na(dTest_treated$x))))
  
  invisible(NULL)
}
