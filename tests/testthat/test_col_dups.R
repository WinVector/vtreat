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
  
  dTrainAll_treated <- cross_frame_experiment$crossFrame
  testthat::expect_true(length(colnames(dTrainAll_treated))==length(unique(colnames(dTrainAll_treated))))
  testthat::expect_true(!isTRUE(any(is.na(dTrainAll_treated$x))))
  
  treatment_plan <- cross_frame_experiment$treatments
  
  dTest_treated <- prepare(treatment_plan, 
                           d,
                           scale = TRUE)
  
  dTest_treated
  testthat::expect_true(length(colnames(dTest_treated))==length(unique(colnames(dTest_treated))))
  testthat::expect_true(!isTRUE(any(is.na(dTest_treated$x))))
  

})
