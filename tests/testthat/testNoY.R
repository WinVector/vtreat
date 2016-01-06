library('vtreat')

context("No Y Examples")

test_that("testNoY: Can transform without Y", {
  dTrainZ <- data.frame(x=c('a','a','a','a','b','b','b'),
                        z=c(1,2,3,4,5,NA,7))
  dTestZ <- data.frame(x=c('a','b','c',NA),
                       z=c(10,20,30,NA))
  treatmentsZ = designTreatmentsZ(dTrainZ,colnames(dTrainZ),
                                  rareCount=0,
                                  verbose=FALSE)
  dTrainZTreated <- prepare(treatmentsZ,dTrainZ,pruneSig=NULL)
  dTestZTreated <- prepare(treatmentsZ,dTestZ,pruneSig=NULL)
})