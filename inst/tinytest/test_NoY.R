
test_NoY <- function() {
  dTrainZ <- data.frame(x=c('a','a','a','a','b','b','b'),
                        z=c(1,2,3,4,5,NA,7))
  dTestZ <- data.frame(x=c('a','b','c',NA),
                       z=c(10,20,30,NA))
  treatmentsZ = designTreatmentsZ(dTrainZ,colnames(dTrainZ),
                                  rareCount=0,
                                  verbose=FALSE)
  dTrainZTreated <- prepare(treatmentsZ,dTrainZ,pruneSig=NULL, check_for_duplicate_frames=FALSE)
  dTestZTreated <- prepare(treatmentsZ,dTestZ,pruneSig=NULL, check_for_duplicate_frames=FALSE)
  expect_true(!is.null(dTestZTreated))
  
  invisible(NULL)
}

test_NoY()
