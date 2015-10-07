library('vtreat')

context("Rare Y")

test_that("Rare Y cases", {
  d0 <- data.frame(x1=1:5,x2=c('a','a','b','b','b'),y=c(0,0,0,0,0))
  yName <- 'y'
  yTarget <- 1
  vars <- setdiff(colnames(d0),yName)
  expect_error(
    designTreatmentsC(d0,
                      vars,yName,yTarget,verbose=FALSE),
    "outcome variable doesn't vary")
  
  d1 <- data.frame(x1=1:5,x2=c('a','a','b','b','b'),y=c(1,0,0,0,0))
  expect_error(
    treatmentsC1 <- designTreatmentsC(d1,
                                      vars,yName,yTarget,verbose=FALSE),
    "there must be a cut")
  d2 <- data.frame(x1=1:5,x2=c('a','a','b','b','b'),y=c(1,1,0,0,0))
  treatmentsC2 <- designTreatmentsC(d2,
                                    vars,yName,yTarget,verbose=FALSE)
})