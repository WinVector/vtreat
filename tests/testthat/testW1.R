library('vtreat')

context("Variable Scoring")

test_that("Numeric Var Scores as expected w1", {
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
  dTN <- prepare(tN,d,pruneSig=0.99)
  tC <- designTreatmentsC(d,c('zip','zip2'),'yc',TRUE,verbose=FALSE)
  dTC <- prepare(tC,d,pruneSig=0.99)

  expect_true(tN$sig[['zip2_catN']]<0.05)
  expect_true(tC$sig[['zip2_catB']]<0.05)
  expect_true(tN$sig[['zip_catN']]>0.1)
  expect_true(tC$sig[['zip_catB']]>0.1)
})

