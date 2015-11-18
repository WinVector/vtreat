library('vtreat')

context("Variable Scoring")

test_that("testW1: Numeric Var Scores as expected w1", {
  set.seed(23525)
  zip <- paste('z',1:100)
  N = 1000
  d <- data.frame(zip=sample(zip,N,replace=TRUE),
                  zip2=sample(zip,N,replace=TRUE),
                  y=runif(N))
  del <- runif(length(zip))
  names(del) <- zip
  d$y <- d$y + del[d$zip2]
  d$yc <- d$y>=mean(d$y)
  library(vtreat)
  tN <- designTreatmentsN(d,c('zip','zip2'),'y',verbose=FALSE,
                          rareCount=2,rareSig=0.9)
  dTN <- prepare(tN,d,pruneSig=0.1)
  tC <- designTreatmentsC(d,c('zip','zip2'),'yc',TRUE,verbose=FALSE,
                          rareCount=2,rareSig=0.9)
  dTC <- prepare(tC,d,pruneSig=0.1)

  expect_true('zip2_catN' %in% colnames(dTN))
  expect_true('zip2_catB' %in% colnames(dTC))
  expect_false('zip_catN' %in% colnames(dTN))
  expect_false('zip_catB' %in% colnames(dTC))
})

