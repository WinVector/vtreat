library('vtreat')

context("Experiment Design")

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


test_that("testExpmtDesign: makekWayCrossValidationGroupedByColumn", {
  set.seed(2325235)
  nrowd = 200
  y <- rnorm(nrowd)
  d <- data.frame(y=y)
  d$group= floor(seq_len(nrow(d))/5)
  splitFn <- makekWayCrossValidationGroupedByColumn('group')
  eSets <- buildEvalSets(nrowd,y=y,dframe=d,
                         splitFunction=splitFn)
  expect_true(attr(eSets,'splitmethod')=='kwaycrossystratifiedgrouped')
  fullSeq <- seq_len(nrowd)
  expect_true(length(eSets)>0)
  for(ei in eSets) {
    expect_true(length(ei$train)>0)
    expect_true(length(ei$app)>0)
    expect_true(all(ei$train %in% fullSeq))
    expect_true(all(ei$app %in% fullSeq))
  }
  apps <- Reduce(c,lapply(eSets,function(ei) ei$app))
  expect_true(length(apps)==nrowd)
  expect_true(length(unique(apps))==nrowd)
  problem <- problemAppPlan(nrowd,3,eSets,TRUE)
  expect_true(is.null(problem))
  # check grouping property
  d$splitLabel <- vtreat::getSplitPlanAppLabels(nrow(d),eSets)
  rs <- rowSums(table(d$group,d$splitLabel)>0)
  expect_true(max(rs)==1)
  expect_true(min(rs)==1)
})

