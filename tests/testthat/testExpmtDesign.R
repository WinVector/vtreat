library('vtreat')

context("Experiment Design")

test_that("testExpmtDesign: cross frame desisgn", {
  for(nrowd in c(1,3,5,10,100,200,1000)) {
    eSets <- buildEvalSets(nrowd)
    fullSeq <- seq_len(nrowd)
    expect_true(length(eSets)>0)
    for(ei in eSets) {
      expect_true(length(ei$train)>0)
      expect_true(length(ei$app)>0)
      expect_true(length(ei$train)>=length(ei$app))
      all(ei$train %in% fullSeq)
      all(ei$app %in% fullSeq)
    }
    if(nrowd>1) {
      for(ei in eSets) {
        expect_true(length(intersect(ei$train,ei$app))==0)
      }
    }
    apps <- Reduce(c,lapply(eSets,function(ei) ei$app))
    expect_true(length(apps)==nrowd)
    expect_true(length(unique(apps))==nrowd)
  }
})

