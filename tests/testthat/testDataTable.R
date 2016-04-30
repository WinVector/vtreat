library('vtreat')

context("DataTable Example")

test_that("testDataTable: DataTable works", {
  # load('tests/testthat/uci.car.data.Rdata')
  if(requireNamespace("data.table",quietly=TRUE)) {
    library("data.table")
    load('uci.car.data.Rdata')
    
    dYName <- "rating"
    dYTarget <- 'vgood'
    pvars <- setdiff(colnames(uci.car.data),dYName)
    seedVal=946463L
    
    # run on data.table (P "prime")
    set.seed(seedVal)
    dT <- data.table::data.table(uci.car.data)
    treatmentsCP <- designTreatmentsC(dT,
                                      pvars,dYName,dYTarget,verbose=FALSE)
    dTrainCTreatedP <- prepare(treatmentsCP,dT,pruneSig=c())
    
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