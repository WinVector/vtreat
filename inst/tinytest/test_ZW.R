
test_ZW <- function() {
  # categorical example
  set.seed(235256)
  dTrainC <- data.frame(x=c('a','a','a','b','b',NA),
                        z=c(1,2,3,4,NA,6),y=c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))
  dTrainC <- rbind(dTrainC,dTrainC)
  trainCWeights <- numeric(nrow(dTrainC))
  trainCWeights[1:(length(trainCWeights)/2)] <- 1
  dTestC <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))
  treatmentsC <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE,
                                   catScaling = TRUE,
                                   weights=trainCWeights,verbose=FALSE)
  dTrainCTreated <- prepare(treatmentsC,dTrainC,pruneSig=c(),scale=TRUE, check_for_duplicate_frames=FALSE)
  varsC <- setdiff(colnames(dTrainCTreated),'y')
  # all input variables should be mean 0
  sapply(dTrainCTreated[,varsC,drop=FALSE],mean)
  # all slopes should be 1
  sapply(varsC,function(c) { glm(paste('y',c,sep='~'),family='binomial',
                                data=dTrainCTreated)$coefficients[[2]]})
  dTestCTreated <- prepare(treatmentsC,dTestC,pruneSig=c(),scale=TRUE, check_for_duplicate_frames=FALSE)
  
  # categorical example indicator mode
  set.seed(235256)
  treatmentsC <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE,
                                   catScaling = FALSE,
                                   weights=trainCWeights,verbose=FALSE)
  dTrainCTreated <- prepare(treatmentsC,dTrainC,pruneSig=c(),scale=TRUE, check_for_duplicate_frames=FALSE)
  varsC <- setdiff(colnames(dTrainCTreated),'y')
  # all input variables should be mean 0
  sapply(dTrainCTreated[,varsC,drop=FALSE],mean)
  # all slopes should be 1
  sapply(varsC,function(c) { lm(paste('y',c,sep='~'),
                                 data=dTrainCTreated)$coefficients[[2]]})
  dTestCTreated <- prepare(treatmentsC,dTestC,pruneSig=c(),scale=TRUE, check_for_duplicate_frames=FALSE)
  
  # numeric example
  set.seed(235256)
  dTrainN <- data.frame(x=c('a','a','a','a','b','b',NA),
                        z=c(1,2,3,4,5,NA,7),y=c(0,0,0,1,0,1,1))
  dTrainN <- rbind(dTrainN,dTrainN)
  trainNWeights <- numeric(nrow(dTrainN))
  trainNWeights[1:(length(trainNWeights)/2)] <- 1
  dTestN <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))
  treatmentsN = designTreatmentsN(dTrainN,colnames(dTrainN),'y',
                                  weights=trainNWeights,
                                  verbose=FALSE)
  dTrainNTreated <- prepare(treatmentsN,dTrainN,pruneSig=c(),scale=TRUE, check_for_duplicate_frames=FALSE)
  varsN <- setdiff(colnames(dTrainNTreated),'y')
  # all input variables should be mean 0
  sapply(dTrainNTreated[,varsN,drop=FALSE],mean) 
  # all slopes should be 1
  sapply(varsN,function(c) { lm(paste('y',c,sep='~'),
                                data=dTrainNTreated)$coefficients[[2]]}) 
  dTestNTreated <- prepare(treatmentsN,dTestN,pruneSig=c(),scale=TRUE, check_for_duplicate_frames=FALSE)
  expect_true(!is.null(dTestNTreated))
  
  invisible(NULL)
}

test_ZW()
