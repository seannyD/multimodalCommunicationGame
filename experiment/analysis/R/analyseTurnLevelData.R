library(gplots)
setwd("~/Documents/MPI/ViniciusMultimodal/multimodalCommunicationGame/experiment/analysis/R/")
d = read.csv("../../data/FinalSignalData.csv")

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



