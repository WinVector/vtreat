library('vtreat')

context("UserScoring")

test_that("User Score fns work", {
  set.seed(23525)
  d <- data.frame(x=runif(100))
  d$yN <- d$x+runif(nrow(d))
  d$yC <- d$yN>1
  csf <- catScoreFrame(d,'x',d$yC,TRUE)
  cs <- catScore(d$x,d$yC,TRUE)
  psf <- pressStatOfBestLinearFitFrame(d,'x',d$yN)
  ps <- pressStatOfBestLinearFit(d$x,d$yN)
  expect_true(ps>0)
  expect_true(cs>0)
  expect_that(psf[['x']],equals(ps))
  expect_that(csf[['x']],equals(cs))
})

