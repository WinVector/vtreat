
# load CDC example from https://github.com/WinVector/zmPDSwR/tree/master/CDC

d <- read.table('natal2010Sample.tsv.gz',sep='\t',header=T,stringsAsFactors=F)
yName <- 'atRisk'
d[,yName] <- d$APGAR5<7
vars <- setdiff(colnames(d),c(yName,
  "ORIGFILEROWNUMBER","ORIGFILENAME","ORIGINSERTTIME",
  "ORIGRANDGROUP","REVISION","APGAR5","F_APGAR5","APGAR5R"))
treatmentsC <- designTreatmentsC(d[sample(1:dim(d)[[1]],1000),],vars,yName,TRUE)
dTrainCTreated <- prepare(treatmentsC,d)
cvars <- setdiff(colnames(dTrainCTreated),yName)
