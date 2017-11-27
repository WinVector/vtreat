library('vtreat')

context("ExperimentDesign1")

test_that("testExpmtDesign: cross frame design", {
  set.seed(2325235)
  for(nrowd in c(1,3,5,10,100,200,1000)) {
    y = rnorm(nrowd)
    eSets <- buildEvalSets(nrowd,y=y)
    if(nrowd>=100) {
      expect_true(attr(eSets,'splitmethod')=='kwaycrossystratified')
    }
    fullSeq <- seq_len(nrowd)
    expect_true(length(eSets)>0)
    for(ei in eSets) {
      expect_true(length(ei$train)>0)
      expect_true(length(ei$app)>0)
      expect_true(all(ei$train %in% fullSeq))
      expect_true(all(ei$app %in% fullSeq))
    }
    if(nrowd>1) {
      for(ei in eSets) {
        expect_true(length(intersect(ei$train,ei$app))==0)
      }
    }
    apps <- Reduce(c,lapply(eSets,function(ei) ei$app))
    expect_true(length(apps)==nrowd)
    expect_true(length(unique(apps))==nrowd)
    problem <- problemAppPlan(nrowd,3,eSets,nrowd>=100)
    expect_true(is.null(problem))
  }
})
