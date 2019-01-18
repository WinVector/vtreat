library('vtreat')

context("coldups")

test_that("test_col_dups.R: issue", {
  d <- data.frame(x = c(1:8, NA, NA), y = c(1,1,1,1,1,0,0,0,0,0))
  cross_frame_experiment <- vtreat::mkCrossFrameCExperiment(
    d, 
    varlist = "x", 
    outcomename = "y", 
    outcometarget = 1,
    verbose = FALSE,
    scale = TRUE)
  
  dTrainAll_treated <- cbind(
    d,
    cross_frame_experiment$crossFrame)
  
  
  treatment_plan <- cross_frame_experiment$treatments
  
  dTest_treated <- prepare(treatment_plan, 
                           d,
                           scale = TRUE)
  
  dTest_treated
  

})
