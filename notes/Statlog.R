
# load example from https://github.com/WinVector/zmPDSwR/tree/master/Statlog

load('GCDData.RData')
yName <- 'Good.Loan'
yTarget <- 'GoodLoan'
vars <- setdiff(colnames(d),yName)
treatmentsC <- designTreatmentsC(d,vars,yName,yTarget)
dTrainCTreated <- prepare(treatmentsC,d)
cvars <- setdiff(colnames(dTrainCTreated),yName)
