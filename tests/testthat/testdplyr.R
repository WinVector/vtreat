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
    
    set.seed(seedVal)
    dT <- dplyr::as.tbl(uci.car.data)
    dT %>%  designTreatmentsC(pvars,dYName,dYTarget,verbose=FALSE) -> 
      treatmentsCP
    treatmentsCP %>% prepare(dT,pruneSig=c()) -> dTrainCTreatedP
    
    set.seed(seedVal)
    dT %>% designTreatmentsC(pvars,dYName,dYTarget,verbose=FALSE) ->
      treatmentsC
    treatmentsC %>% prepare(dT,pruneSig=c()) -> dTrainCTreated
    expect_true(nrow(dTrainCTreated)==nrow(dTrainCTreatedP))
    expect_true(length(colnames(dTrainCTreated))==length(colnames(dTrainCTreatedP)))
    expect_true(all(colnames(dTrainCTreated)==colnames(dTrainCTreatedP)))
    for(v in setdiff(colnames(dTrainCTreated),dYName)) {
      ev <- max(abs(dTrainCTreated[[v]]-dTrainCTreatedP[[v]]))
      expect_true(ev<1.0e-3)
    }
    expect_true(max(abs(pmax(0,treatmentsC$PRESSRsquared)-pmax(0,treatmentsCP$PRESSRsquared)))<=1.0e-2)
    expect_true(max(abs(pmax(0,treatmentsC$varScore)-pmax(0,treatmentsCP$varScore)))<=1.0e-2)
    # this score depends strongly on pseudo random samples
    expect_true(max(abs(pmax(0,treatmentsC$catPseudoRSquared)-pmax(0,treatmentsCP$catPseudoRSquared)))<=0.3)
  }
})