## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(fig.width = 7)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
library(vtreat)
set.seed(23255)

have_ggplot = requireNamespace("ggplot2", quietly=TRUE)
have_dplyr = requireNamespace("dplyr", quietly=TRUE)
if(have_ggplot) {
  library(ggplot2)
}
if(have_dplyr) {
  library(dplyr)
}

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
#
# takes the frame (d) and the outcome column (d$conc)
# from the global environment
#
showGroupingBehavior = function(groupcol, title) {
  print(title)
  
  # display means of each group
  print("Group means:")
  means = tapply(d$conc, d[[groupcol]], mean)
  print(means)
  print(paste("Standard deviation of group means:", sd(means)))
  
  if(have_ggplot && have_dplyr) {
    # unify the time indices
    d %>% group_by(Subject) %>% 
      arrange(Time) %>% mutate(timeIndex=1:n()) -> dtmp
    
    # I want the time indices to have the same spacing as the original time points
    # dtmp$timeFrac = with(dtmp, round(100*Time/25)/100) # round to nearest 1/100th
    dtmp %>% group_by(timeIndex) %>% summarize(approxTime=mean(Time)) -> atimef
    dtmp$approxTime = atimef$approxTime[dtmp$timeIndex]
    
    dtmp[[groupcol]] = as.factor(dtmp[[groupcol]])
    
    plt = ggplot(data=dtmp, aes_string(x="approxTime", y="conc", color=groupcol)) + 
      stat_summary(fun.y="mean", geom="line") +  stat_summary(fun.y="mean", geom="point") + 
      ggtitle(paste("Mean concentration over time:\n", title)) + 
      theme(legend.position="none")
    print(plt)
  }
}

## ----data----------------------------------------------------------------
# panel data for concentration in multiple subjects 
d <- datasets::Theoph
head(d)
summary(d)

## ------------------------------------------------------------------------

if(have_ggplot) {
  ggplot(d, aes(x=Time, y=conc, color=Subject)) + 
  geom_point() + geom_line() + 
    theme(legend.position="none") + 
    ggtitle("Theophylline concentrations over time")
}

## ------------------------------------------------------------------------
# a somewhat arbitrary split of patients
subnum = as.numeric(as.character(d$Subject))
d$modSplit = as.factor(subnum %% 3)


## ------------------------------------------------------------------------
print(table(Subject=d$Subject, groupid=d$modSplit))

## ------------------------------------------------------------------------
# stratify by outcome only
# forces concentration to be equivalent
pStrat <- kWayStratifiedY(nrow(d),3,d,d$conc)
attr(pStrat, "splitmethod")
d$stratSplit <- vtreat::getSplitPlanAppLabels(nrow(d),pStrat)

print(table(Subject=d$Subject, groupid=d$stratSplit))

## ------------------------------------------------------------------------
# stratify by patient and outcome
# allows concentration to vary amoung individual patients
splitter <- makekWayCrossValidationGroupedByColumn('Subject')
split <- splitter(nrow(d),3,d,d$conc)
attr(split, "splitmethod")
d$subjectSplit <- vtreat::getSplitPlanAppLabels(nrow(d),split)

print(table(Subject=d$Subject, groupid=d$subjectSplit))

## ----echo=FALSE----------------------------------------------------------
showGroupingBehavior("modSplit", "Arbitrary grouping")

## ----echo=FALSE----------------------------------------------------------
showGroupingBehavior("subjectSplit", "Group by patient, stratify on y")

