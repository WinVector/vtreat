
# example of working with KDD2009 data (just to show library works, results are not great as the
# big issue with this data set is over-fitting).

#get some libraries
library('ggplot2')
#library('vtreat')  # This library isn't public yet


# load the data as in https://github.com/WinVector/zmPDSwR/tree/master/KDD2009 README.md
# download the raw form of: https://github.com/WinVector/zmPDSwR/raw/master/KDD2009/orange_small_train.data.gz
d <- read.table('orange_small_train.data.gz',
                header=T,sep='\t',na.strings=c('NA',''))

churn <- read.table('orange_small_train_churn.labels.txt',
                    header=F,sep='\t')
d$churn <- churn$V1
appetency <- read.table('orange_small_train_appetency.labels.txt',
                        header=F,sep='\t')
d$appetency <- appetency$V1
upselling <- read.table('orange_small_train_upselling.labels.txt',
                        header=F,sep='\t')
d$upselling <- upselling$V1
set.seed(729375)
d$rgroup <- runif(dim(d)[[1]])
dTrainAll <- subset(d,rgroup<=0.9)
dTest <- subset(d,rgroup>0.9)
rm(list=c('d','churn','appetency','upselling'))


outcomes <- c('churn','appetency','upselling')
vars <- setdiff(colnames(dTrainAll),
                c(outcomes,'rgroup'))
yName <- 'churn'
yTarget <- 1
yCond <- paste(yName,yTarget,sep='==')

# try the automatic variable treatment
treatments <- designTreatmentsC(dTrainAll[sample(1:dim(dTrainAll)[[1]],1000),],vars,yName,1)
#pruneLevel <- sort(treatments$varScores)[(length(treatments$varScores)-50)]
pruneLevel <- 0.99
treatedTrain <- prepare(treatments,dTrainAll,
                        pruneLevel=pruneLevel,scale=TRUE)
treatedTest <- prepare(treatments,dTest,
                       pruneLevel=pruneLevel,scale=TRUE)
mvars <- intersect(treatments$vars,colnames(treatedTrain))

# # model directly on variables
# formula <- paste(yCond,paste(mvars,collapse=' + '),sep=' ~ ')
# model <- glm(formula,data=treatedTrain,family=binomial(link='logit'))
# treatedTest$pred <- predict(model,newdata=treatedTest,type='link')
# ggplot(data=treatedTest) + 
#    geom_density(aes_string(x='pred',color=paste('as.factor(',yName,')')))
# aggregate(as.formula(paste('pred',yName,sep='~')),data=treatedTest,FUN=mean)

# try a principal components version of the analysis
pcomp <- prcomp(treatedTrain[,mvars])
goodP <- pcomp$sdev>1.0e-2
projection <- pcomp$rotation[,goodP]
treatedTrainP <- as.data.frame(as.matrix(treatedTrain[,mvars]) %*% projection)
pvars <- colnames(treatedTrainP)
treatedTrainP[,yName] = treatedTrain[,yName]
treatedTestP <- as.data.frame(as.matrix(treatedTest[,mvars]) %*% projection)
treatedTestP[,yName] = treatedTest[,yName]
formulaP <- paste(yCond,paste(pvars,collapse=' + '),sep=' ~ ')
modelP <- glm(formulaP,data=treatedTrainP,family=binomial(link='logit'))
treatedTestP$pred <- predict(modelP,newdata=treatedTestP,type='link')
ggplot(data=treatedTestP) + 
   geom_density(aes_string(x='pred',color=paste('as.factor(',yName,')')))
aggregate(as.formula(paste('pred',yName,sep='~')),data=treatedTestP,FUN=mean)




