
# data load example from https://github.com/WinVector/zmPDSwR/tree/master/UCICar
d <- read.table('car.data.csv',header=TRUE,sep=',',stringsAsFactors=FALSE)

dYName <- "rating"
dYTarget <- 'vgood'
pvars <- setdiff(colnames(d),dYName)
treatmentsC <- designTreatmentsC(d,pvars,dYName,dYTarget)
dTrainCTreated <- prepare(treatmentsC,d)
cvars <- setdiff(colnames(dTrainCTreated),dYName)
