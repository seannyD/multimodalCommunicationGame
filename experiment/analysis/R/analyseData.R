# This file loads the data in ../data/csv/*.csv and compiles it into a single file
#  plus some handy variables

library(gplots)

# 16 trials per game
# 64 per block
# 128 per experiment

source("multiHist.R")

setwd("~/Documents/MPI/ViniciusMultimodal/multimodalCommunicationGame/experiment/analysis/R/")


overlaps = function(sigs1,sigs2, margin=50){
  # Are there any overlaps between the list of two communicative acts
  # turns must have at least 'margin' overlap to count
     inside = outer(sigs1$signalEnd,sigs2$signalStart, "-")
     outside = outer(sigs1$signalStart,sigs2$signalEnd, "-")
     overlap = (inside>margin) & (outside<0)
     numberOfOverlaps=sum(overlap)
     propOverlapsActs = numberOfOverlaps / max(dim(inside))
     proportionOfOverlapsTime = apply(
       cbind(
         -(outside/inside)[overlap],
         -(inside/outside)[overlap])
       ,1,min)
     return(list(
       numberOfOverlaps=numberOfOverlaps,
       propOverlapsTime=proportionOfOverlapsTime,
       propOverlapsActs = propOverlapsActs))
}

plotTurn = function(sigs1,sigs2){
  plot(range(c(sigs1$signalStart,sigs1$signalStart,sigs1$signalEnd,sigs1$signalEnd)),c(1,nrow(sigs1)+0.5), type='n',yaxt='n')
  arrows(sigs1$signalStart,
         1:nrow(sigs1),
         sigs1$signalEnd,
         1:nrow(sigs1),
         code=3, angle=90)
  arrows(sigs2$signalStart,
         1:nrow(sigs2)+0.5,
         sigs2$signalEnd,
         1:nrow(sigs2)+0.5,
         code=3, angle=90
         , col=2)
}

basedir = "../../data/csv/"

d = data.frame()
files = list.files(basedir , pattern = "*.csv")
for(f in files){
  dx = read.csv(paste(basedir,f,sep=''), stringsAsFactors = F)
  d = rbind(d,dx)
}

d$trialString = paste(d$condition,d$dyadNumber, d$game,d$trial)

experDetails = read.delim("../../data/ExperimentDetails.tab",sep='\t', stringsAsFactors = F)
experDetails$dyad = paste("D",experDetails$dyad,sep='')
rownames(experDetails) = experDetails$dyad

d$modalityCondition = experDetails[d$dyadNumber,]$condition

# we don't have info on when the very last trial of each block ends, so assume it's the end of the trial
d[d$game==3 & d$trial==15,]$startOfNextTrial = d[d$game==3 & d$trial==15,]$trialEnd

# fix stuff

d$turnType  = gsub("\\.","",d$turnType)

# Exclude cases

# Remove first turn by matcher
d = d[d$turnType!='T0',]




# work out trail length
#  From start of first turn to when the listener selects a meaning.
#  The trials track the matchers turn, so that the end of the trial is when
#  the matcher makes a choice.
trialLengths = tapply(d$trialEnd, d$trialString, max)- tapply(d$turnStart, d$trialString, head,n=1)

d$trialLength = trialLengths[d$trialString]

d = d[d$trialLength> 2000,]

d$correct = d$correct=="Correct"


# Write the data to a main file
write.csv(d,"../../data/FinalSignalData.csv")