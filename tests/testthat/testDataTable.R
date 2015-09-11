library('vtreat')

context("DataTable Example")

test_that("DataTable works", {
  # load('tests/testthat/uci.car.data.Rdata')
  if(requireNamespace("data.table",quietly=TRUE)) {
    load('uci.car.data.Rdata')
    
    dYName <- "rating"
    dYTarget <- 'vgood'
    pvars <- setdiff(colnames(uci.car.data),dYName)
    seedVal=946463L
    
    set.seed(seedVal)
    dT <- data.table::data.table(uci.car.data)
    treatmentsCP <- designTreatmentsC(dT,
                                      pvars,dYName,dYTarget,verbose=FALSE)
    dTrainCTreatedP <- prepare(treatmentsCP,dT,pruneSig=c())
    
    set.seed(seedVal)
    treatmentsC <- designTreatmentsC(uci.car.data,
                                     pvars,dYName,dYTarget,verbose=FALSE)
    dTrainCTreated <- prepare(treatmentsC,uci.car.data,pruneSig=c())
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