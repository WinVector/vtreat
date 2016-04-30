library('vtreat')

context("dplyr Example")

test_that("testdplyr: works with dplyr", {
  # load('tests/testthat/uci.car.data.Rdata')
  if(requireNamespace("dplyr",quietly=TRUE)) {
    library("dplyr")
    load('uci.car.data.Rdata')
    
    dYName <- "rating"
    dYTarget <- 'vgood'
    pvars <- setdiff(colnames(uci.car.data),dYName)
    seedVal=946463L
    
    # run on dplyr tbl
    set.seed(seedVal)
    dT <- dplyr::as.tbl(uci.car.data)
    dT %>%  designTreatmentsC(pvars,dYName,dYTarget,verbose=FALSE) -> 
      treatmentsCP
    treatmentsCP %>% prepare(dT,pruneSig=c()) -> dTrainCTreatedP
    
    # re-run on data.frame
    set.seed(seedVal)
    dT <- uci.car.data
    treatmentsC <- designTreatmentsC(dT,
                                     pvars,dYName,dYTarget,verbose=FALSE)
    dTrainCTreated <- prepare(treatmentsC,dT,pruneSig=c())
    
    expect_true(nrow(dTrainCTreated)==nrow(dTrainCTreatedP))
    expect_true(length(colnames(dTrainCTreated))==length(colnames(dTrainCTreatedP)))
    expect_true(all(colnames(dTrainCTreated)==colnames(dTrainCTreatedP)))
    for(v in setdiff(colnames(dTrainCTreated),dYName)) {
      ev <- max(abs(dTrainCTreated[[v]]-dTrainCTreatedP[[v]]))
      expect_true(ev<1.0e-3)
    }
  }
})