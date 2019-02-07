
test_ExpmtDesign2 <- function() {
  set.seed(2325235)
  nrowd = 200
  y <- rnorm(nrowd)
  d <- data.frame(y=y)
  d$group= floor(seq_len(nrow(d))/5)
  splitFn <- makekWayCrossValidationGroupedByColumn('group')
  eSets <- buildEvalSets(nrowd,y=y,dframe=d,
                         splitFunction=splitFn)
  RUnit::checkTrue(attr(eSets,'splitmethod')=='kwaycrossystratifiedgrouped')
  fullSeq <- seq_len(nrowd)
  RUnit::checkTrue(length(eSets)>0)
  for(ei in eSets) {
    RUnit::checkTrue(length(ei$train)>0)
    RUnit::checkTrue(length(ei$app)>0)
    RUnit::checkTrue(all(ei$train %in% fullSeq))
    RUnit::checkTrue(all(ei$app %in% fullSeq))
  }
  apps <- Reduce(c,lapply(eSets,function(ei) ei$app))
  RUnit::checkTrue(length(apps)==nrowd)
  RUnit::checkTrue(length(unique(apps))==nrowd)
  problem <- problemAppPlan(nrowd,3,eSets,TRUE)
  RUnit::checkTrue(is.null(problem))
  # check grouping property
  d$splitLabel <- vtreat::getSplitPlanAppLabels(nrow(d),eSets)
  rs <- rowSums(table(d$group,d$splitLabel)>0)
  RUnit::checkTrue(max(rs)==1)
  RUnit::checkTrue(min(rs)==1)
  
  invisible(NULL)
}

