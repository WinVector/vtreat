library('vtreat')

context("Unique Value Examples")

test_that("testUniqValue: Can work with unique values", {
  dTrainN <- data.frame(x=c('a','a','a','a','a','a','b'),
                        z=c(0,0,0,0,0,0,1),
                        y=c(1,0,0,0,0,0,0))
  dTestN <- data.frame(x=c('a','b','c',NA),
                       z=c(10,20,30,NA))
  treatmentsN = designTreatmentsN(dTrainN,colnames(dTrainN),'y',
                                  rareCount=0,rareSig=1,
                                  verbose=FALSE)
  dTrainNTreated <- prepare(treatmentsN,dTrainN,pruneSig=1)
  dTestNTreated <- prepare(treatmentsN,dTestN,pruneSig=1)
  
  
  dTrainC <- data.frame(x=c('a','a','a','a','a','a','b'),
                        z=c(0,0,0,0,0,0,1),
                        y=c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE))
  dTestC <- data.frame(x=c('a','b','c',NA),
                       z=c(10,20,30,NA))
  treatmentsC <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE,
                                   rareCount=0,rareSig=1,
                                   verbose=FALSE)
  dTrainCTreated <- prepare(treatmentsC,dTrainC,
                            pruneSig=1,doCollar=FALSE)
  dTestCTreated <- prepare(treatmentsC,dTestC,
                           pruneSig=1,doCollar=FALSE)
  
  dTrainZ <- data.frame(x=c('a','a','a','a','a','a','b'),
                        z=c(0,0,0,0,0,0,1))
  dTestZ <- data.frame(x=c('a','b','c',NA),
                       z=c(10,20,30,NA))
  treatmentsZ = designTreatmentsZ(dTrainN,colnames(dTrainN),
                                  rareCount=0,
                                  verbose=FALSE)
  dTrainZTreated <- prepare(treatmentsN,dTrainN,pruneSig=1)
  dTestZTreated <- prepare(treatmentsN,dTestN,pruneSig=1)
})