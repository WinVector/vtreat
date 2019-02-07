

test_ExpmtDesign <- function() {
  set.seed(2325235)
  for(nrowd in c(1,3,5,10,100,200,1000)) {
    y = rnorm(nrowd)
    eSets <- buildEvalSets(nrowd,y=y)
    if(nrowd>=100) {
      RUnit::checkTrue(attr(eSets,'splitmethod')=='kwaycrossystratified')
    }
    fullSeq <- seq_len(nrowd)
    RUnit::checkTrue(length(eSets)>0)
    for(ei in eSets) {
      RUnit::checkTrue(length(ei$train)>0)
      RUnit::checkTrue(length(ei$app)>0)
      RUnit::checkTrue(all(ei$train %in% fullSeq))
      RUnit::checkTrue(all(ei$app %in% fullSeq))
    }
    if(nrowd>1) {
      for(ei in eSets) {
        RUnit::checkTrue(length(intersect(ei$train,ei$app))==0)
      }
    }
    apps <- Reduce(c,lapply(eSets,function(ei) ei$app))
    RUnit::checkTrue(length(apps)==nrowd)
    RUnit::checkTrue(length(unique(apps))==nrowd)
    problem <- problemAppPlan(nrowd,3,eSets,nrowd>=100)
    RUnit::checkTrue(is.null(problem))
  }
  
  invisible(NULL)
}
