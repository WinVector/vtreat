## ----categoricalexample, tidy=FALSE--------------------------------------
library(vtreat)
dTrainC <- data.frame(x=c('a','a','a','b','b',NA),
   z=c(1,2,3,4,NA,6),y=c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE),
   stringsAsFactors = FALSE)
treatmentsC <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE)
scoreColsToPrint <- c('origName','varName','code','rsq','sig','extraModelDegrees')
print(treatmentsC$scoreFrame[,scoreColsToPrint])

## ----map-----------------------------------------------------------------
# Build a map from vtreat names back to reasonable display names
vmap <- as.list(treatmentsC$scoreFrame$origName)
names(vmap) <- treatmentsC$scoreFrame$varName
print(vmap['x_catB'])

# Map significances back to original variables
aggregate(sig~origName,data=treatmentsC$scoreFrame,FUN=min)

## ----numericexample, tidy=FALSE------------------------------------------
library(vtreat)
dTrainN <- data.frame(x=c('a','a','a','b','b',NA),
   z=c(1,2,3,4,NA,6),y=as.numeric(c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE)),
   stringsAsFactors = FALSE)
treatmentsN <- designTreatmentsN(dTrainN,colnames(dTrainN),'y')
print(treatmentsN$scoreFrame[,scoreColsToPrint])

## ----notargetexample, tidy=FALSE-----------------------------------------
library(vtreat)
dTrainZ <- data.frame(x=c('a','a','a','b','b',NA),
   z=c(1,2,3,4,NA,6),
   stringsAsFactors = FALSE)
treatmentsZ <- designTreatmentsZ(dTrainZ,colnames(dTrainZ))
print(treatmentsZ$scoreFrame[, c('origName','varName','code','extraModelDegrees')])

## ----restrict1-----------------------------------------------------------
dTrainN <- data.frame(x=c('a','a','a','b','b',NA),
   z=c(1,2,3,4,NA,6),y=as.numeric(c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE)),
   stringsAsFactors = FALSE)

treatmentsN <- designTreatmentsN(dTrainN,colnames(dTrainN),'y',
                                 codeRestriction = c('lev', 
                                                      'catN',
                                                      'clean',
                                                      'isBAD'),
                                 verbose=FALSE)

# no catP or catD variables
print(treatmentsN$scoreFrame[,scoreColsToPrint])


## ----restrict2-----------------------------------------------------------
dTreated = prepare(treatmentsN, dTrainN, 
                   codeRestriction = c('lev','clean', 'isBAD'))

# no catN variables
head(dTreated)

## ----selectvars----------------------------------------------------------
dTrainN <- data.frame(x=c('a','a','a','b','b',NA),
   z=c(1,2,3,4,NA,6),y=as.numeric(c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE)),
   stringsAsFactors = FALSE)
treatmentsN <- designTreatmentsN(dTrainN,colnames(dTrainN),'y',
                                  codeRestriction = c('lev', 
                                                      'catN',
                                                      'clean',
                                                      'isBAD'),
                                 verbose=FALSE)
print(treatmentsN$scoreFrame[,scoreColsToPrint])

pruneSig <- 1.0 # don't filter on significance for this tiny example
vScoreFrame <- treatmentsN$scoreFrame
varsToUse <- vScoreFrame$varName[(vScoreFrame$sig<=pruneSig)]
print(varsToUse)
origVarNames <- sort(unique(vScoreFrame$origName[vScoreFrame$varName %in% varsToUse]))
print(origVarNames)

# prepare a treated data frame using only the "significant" variables
dTreated = prepare(treatmentsN, dTrainN, 
                   varRestriction = varsToUse)

head(dTreated)


## ----displayvars---------------------------------------------------------
origVarNames <- sort(unique(vScoreFrame$origName[vScoreFrame$varName %in% varsToUse]))
print(origVarNames)

origVarSigs <- vScoreFrame[vScoreFrame$varName %in% varsToUse,]
aggregate(sig~origName,data=origVarSigs,FUN=min)

