library('vtreat')

context("Variable Scoring")

test_that("Numeric Var Scores as expected", {
  set.seed(23525)
  zip <- paste('z',1:400)
  N = 1000
  d <- data.frame(zip=sample(zip,N,replace=TRUE),
                  zip2=sample(zip,N,replace=TRUE),
                  y=runif(N))
  del <- runif(length(zip))
  names(del) <- zip
  d$y <- d$y + del[d$zip2]
  d$yc <- d$y>=mean(d$y)
  library(vtreat)
  tN <- designTreatmentsN(d,c('zip','zip2'),'y',verbose=FALSE)
  dTN <- prepare(tN,d)
  #print(tN$varScores)
  tC <- designTreatmentsC(d,c('zip','zip2'),'yc',TRUE,verbose=FALSE)
  dTC <- prepare(tC,d)
  #print(tC$varScores)
  
  
  expect_true(tN$varScores[['zip_catN']]>1)
  expect_true(tN$varScores[['zip2_catN']]<1)
  expect_true(tC$varScores[['zip_catN']]>1)
  expect_true(tC$varScores[['zip2_catN']]<1)
})

