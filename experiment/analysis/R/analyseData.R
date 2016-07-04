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
trialLengths = tapply(d$startOfNextTrial, d$trialString, max)- tapply(d$turnStart, d$trialString, head,n=1)

d$trialLength = trialLengths[d$trialString]

d = d[d$trialLength> 2000,]

d$correct = d$correct=="Correct"

plotmeans(trialLength~paste(modalityCondition,condition), 
          data=d[!duplicated(d$trialString),], 
          connect=list(1:2,3:4,5:6))


pdf("../../results/graphs/Efficiency.pdf", width=10, height=6)
par(mfrow=c(1,2))
for(stimType in unique(d$condition)){
  
plotmeans(trialLength/1000~game,
          data = d[d$modalityCondition=='vocal' & !duplicated(d$trialString)
                   & d$condition==stimType,],
          col=1,barcol = 1,n.label = F,
          ylim=c(0,30),
          xlab="Game",
          ylab="Trial length (s)", las=1)
plotmeans(trialLength/1000~game,
          data = d[d$modalityCondition=='multi' & !duplicated(d$trialString)
                   & d$condition==stimType,],
          add=T,col=2,barcol = 2,
          xaxt='n',
          n.label = F)
plotmeans(trialLength/1000~game,
          data = d[d$modalityCondition=='visual' & !duplicated(d$trialString)
                   & d$condition==stimType,],
          add=T,col=3,barcol = 3,n.label = F,
          xaxt='n')
  if(stimType=="Auditory"){
    legend(2.5,25,legend=c('vocal','multimodal','visual'), col=1:3,lty=1,pch=1)
  }
  title(main=stimType)
}
dev.off()




pdf("../../results/graphs/Accuracy.pdf", width=10, height=6)
par(mfrow=c(1,2))
for(stimType in unique(d$condition)){
  
  plotmeans(correct~game,
            data = d[d$modalityCondition=='vocal' & !duplicated(d$trialString)
                     & d$condition==stimType,],
            col=1,barcol = 1,n.label = F,
            ylim=c(0,1),
            xlab="Game",
            ylab="Accuracy", las=1)
  plotmeans(correct~game,
            data = d[d$modalityCondition=='multi' & !duplicated(d$trialString)
                     & d$condition==stimType,],
            add=T,col=2,barcol = 2,
            xaxt='n',
            n.label = F)
  plotmeans(correct~game,
            data = d[d$modalityCondition=='visual' & !duplicated(d$trialString)
                     & d$condition==stimType,],
            add=T,col=3,barcol = 3,n.label = F,
            xaxt='n')
  if(stimType=="Auditory"){
    legend(2.5,0.5,legend=c('vocal','multimodal','visual'), col=1:3,lty=1,pch=1)
  }
  title(main=stimType)
}
dev.off()

getPropTimeAcoustic = function(acousticSel,visualSel){
    
    # Add the signal time for each modality for each trial
    acousticTime = tapply(d[acousticSel,]$signalLength,
                          d[acousticSel,]$trialString,
                          sum)
    
    visualTime = tapply(d[visualSel,]$signalLength,
                        d[visualSel,]$trialString,
                          sum)
    
    # Make sure there's a time for each trial
    # if the trial time is NA, set it to zero
    allTrials = unique(d[d$modalityCondition=="multi",]$trialString)
    visualTime= visualTime[allTrials]
    visualTime[is.na(visualTime)] = 0
    acousticTime= acousticTime[allTrials]
    acousticTime[is.na(acousticTime)] = 0
    
    propAcousticSignals = acousticTime / (acousticTime+visualTime)
    names(propAcousticSignals) = allTrials
    
    propAcousticSignals = propAcousticSignals[!is.na(propAcousticSignals)]
    
    #x = hist(propAcousticSignals, plot = F)
    #x$counts = x$counts/length(unique(d[d$modalityCondition=="multi",]$trialString))
    #plot(x, ylab="Proportion of trials")
    
    propAcousticSignals_AuditoryStim = 
      propAcousticSignals[grepl("Auditory",names(propAcousticSignals))]
    propAcousticSignals_VisualStim = 
      propAcousticSignals[grepl("Visual",names(propAcousticSignals))]
    
      return(list(propAcousticSignals_AuditoryStim=propAcousticSignals_AuditoryStim,
                  propAcousticSignals_VisualStim=propAcousticSignals_VisualStim))
}

# which cases are T1s with acoustic/visual signals?
acousticT1s = d$modalityCondition=="multi" & d$turnType=='T1' & d$modality=="Acoustic"
visualT1s = d$modalityCondition=="multi" & d$turnType=='T1' & d$modality=="Visual"

x = getPropTimeAcoustic(acousticT1s,visualT1s)
propAcousticSignals_AuditoryStim = x[[1]]
propAcousticSignals_VisualStim = x[[2]]

# Plot histograms
cols= c(rgb(0,1,0,0.5),rgb(1,0,0,0.5))
breaks = seq(0,1,0.05)
hist(propAcousticSignals_AuditoryStim, col=cols[1], border=cols[1], breaks=breaks)
hist(propAcousticSignals_VisualStim, add=T, col=cols[2], border = cols[2], breaks=breaks)



# Mirrored histogram
multhist(propAcousticSignals_AuditoryStim,propAcousticSignals_VisualStim,
         bin.width=0.05,
         col=c(2,3),
         dir=c(1,-1), 
         main="T1",
         legends = c("Acoustic","Visual"),
         xlab="Proportion of Acoustic signals",
         ylab="Number of trials")


acousticDirector = d$modalityCondition=="multi" & d$role=="Director" & d$modality=="Acoustic"
visualDirector = d$modalityCondition=="multi" & d$role=="Director" & d$modality=="Visual"

x = getPropTimeAcoustic(acousticDirector ,visualDirector)
propAcousticSignals_AuditoryStim_director = x[[1]]
propAcousticSignals_VisualStim_director = x[[2]]

multhist(propAcousticSignals_AuditoryStim_director,
         propAcousticSignals_VisualStim_director,
         bin.width=0.05,
         col=c(2,3),
         dir=c(1,-1), 
         main="Director Turns",
         legends = c("Acoustic","Visual"),
         xlab="Proportion of Acoustic signals",
         ylab="Number of trials")

############
# T2
acousticT2s = d$modalityCondition=="multi" & d$turnType=='T2' & d$modality=="Acoustic"
visualT2s = d$modalityCondition=="multi" & d$turnType=='T2' & d$modality=="Visual"

x = getPropTimeAcoustic(acousticT2s,visualT2s)
propAcousticSignals_AuditoryStimT2 = x[[1]]
propAcousticSignals_VisualStimT2 = x[[2]]

# Mirrored histogram
multhist(propAcousticSignals_AuditoryStimT2,propAcousticSignals_VisualStimT2,
         bin.width=0.05,
         col=c(1,2),
         dir=c(1,-1),
         main='T2',
         legends = c("Acoustic","Visual"),
         xlab="Proportion of Acoustic signals",
         ylab="Number of trials")
############
# T3
acousticT3s = d$modalityCondition=="multi" & d$turnType=='T3' & d$modality=="Acoustic"
visualT3s = d$modalityCondition=="multi" & d$turnType=='T3' & d$modality=="Visual"

x = getPropTimeAcoustic(acousticT3s,visualT3s)
propAcousticSignals_AuditoryStimT3 = x[[1]]
propAcousticSignals_VisualStimT3 = x[[2]]

# Mirrored histogram
multhist(propAcousticSignals_AuditoryStimT3,propAcousticSignals_VisualStimT3,
         bin.width=0.05,
         col=c(1,2),
         dir=c(1,-1),
         main='T3',
         legends = c("Acoustic","Visual"),
         xlab="Proportion of Acoustic signals",
         ylab="Number of trials")


############
# Turn level data

turnD = data.frame()

# For each trial
for(trialString in unique(d$trialString)){
  trialData = d[d$trialString==trialString,]
  trialData$turns = paste(trialData$role,trialData$turnNumber)
  # for each turn
  for(turn in unique(trialData$turns)){
    turnData = trialData[trialData$turns == turn,]
    
    numModalitiesUsed = length(unique(turnData$modality))
    useVisual = "Visual" %in% turnData$modality
    useAcoustic = "Acoustic" %in% turnData$modality
    numberOfOverlaps = 0
    propOverlapsActs = 0
    propOverlapsTime = 0
    
    if(numModalitiesUsed==2){
      ol = overlaps(
        sigs1 = turnData[turnData$modality=="Visual",],
        sigs2 = turnData[turnData$modality=="Acoustic",]
      )
      numberOfOverlaps = ol$numberOfOverlaps
      propOverlapsTime = mean(ol$propOverlapsTime)
      propOverlapsActs = ol$propOverlapsActs
    }
    
    ret = cbind(
      turnData[1,],
      numModalitiesUsed,
      numberOfOverlaps,
      propOverlapsTime,
      propOverlapsActs,
      useVisual,
      useAcoustic)
    turnD = rbind(turnD,ret)
  }
}

turnD = turnD[,!names(turnD) %in% c("signalStart",'signalEnd','signalLength','signalType')]

turnD$turnModalityType = "none"
turnD$turnModalityType[turnD$useVisual & turnD$useAcoustic] = "multi"
turnD$turnModalityType[turnD$useVisual & (!turnD$useAcoustic)] = "visual"
turnD$turnModalityType[(!turnD$useVisual) & turnD$useAcoustic] = "acoustic"
turnD$turnModalityType = as.factor(turnD$turnModalityType)

table(turnD$useVisual,turnD$useAcoustic)
table(turnD[turnD$turnType=="T1" & turnD$modalityCondition=='multi',]$turnModalityType,
      turnD[turnD$turnType=="T1" & turnD$modalityCondition=='multi',]$condition)

matrix(
  unlist(
    tapply(
      turnD$turnModalityType, 
      turnD$dyadNumber, 
      table)),
  ncol=3
)

plotmeans(propOverlapsTime~ condition, 
          data=turnD[turnD$modalityCondition=='multi' & turnD$turnModalityType=="multi",])



