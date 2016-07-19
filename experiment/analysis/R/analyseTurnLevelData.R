library(gplots)
setwd("~/Documents/MPI/ViniciusMultimodal/multimodalCommunicationGame/experiment/analysis/R/")
d = read.csv("../../data/FinalSignalData.csv")

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

overlaps2 = function(t1Start,t1End,t2Start,t2End){
  if(t1Start < t2Start){
    return(t1End - t2Start)
  } else{
    return(t2End - t1Start)
  }
}


############
# Turn level data

turnD = data.frame()

# For each trial
for(turn in unique(d$turnString)){
    turnData = d[d$turnString == turn,]
    
    numModalitiesUsed = length(unique(turnData$modality))
    useVisual = "Visual" %in% turnData$modality
    useAcoustic = "Acoustic" %in% turnData$modality
    numberOfOverlaps = 0
    propOverlapsActs = 0
    propOverlapsTime = 0
    numberOfMultimodalSigs = 0
   # numberOfVisualSigs = sum(turnData$modality=="Visual")
   # numberOfAcousticSigs = sum(turnData$modality=="Acoustic")
    numberOfSignals = nrow(turnData)
    
    if(numModalitiesUsed==2){
      ol = overlaps(
        sigs1 = turnData[turnData$modality=="Visual",],
        sigs2 = turnData[turnData$modality=="Acoustic",]
      )
      numberOfOverlaps = ol$numberOfOverlaps
      propOverlapsTime = mean(ol$propOverlapsTime)
      propOverlapsActs = ol$propOverlapsActs
      
      numberOfMultimodalSigs = numberOfOverlaps
     # numberOfVisualSigs = sum(turnData$modality=="Visual") - numberOfOverlaps
     # numberOfAcousticSigs = sum(turnData$modality=="Acoustic") - numberOfOverlaps
      
      
    }
    
    ret = cbind(
      turnData[1,],
      numModalitiesUsed,
      numberOfOverlaps,
      propOverlapsTime,
      propOverlapsActs,
      useVisual,
      useAcoustic,
      numberOfSignals,
      numberOfMultimodalSigs
      )
    turnD = rbind(turnD,ret)
}


turnD = turnD[,!names(turnD) %in% c("signalStart",'signalEnd','signalLength','signalType')]

turnD$turnModalityType = "none"
turnD$turnModalityType[turnD$numberOfMultimodalSigs>0] = "multi"
turnD$turnModalityType[turnD$numberOfMultimodalSigs==0 & turnD$useVisual & turnD$useAcoustic] = "unimodal mixed"
turnD$turnModalityType[turnD$numberOfMultimodalSigs==0 & turnD$useVisual & (!turnD$useAcoustic)] = "unimodal visual"
turnD$turnModalityType[(!turnD$useVisual) & turnD$useAcoustic] = "unimodal acoustic"
turnD$turnModalityType = as.factor(turnD$turnModalityType)

table(turnD$useVisual,turnD$useAcoustic)
table(turnD[turnD$turnType=="T1" & turnD$modalityCondition=='multi',]$turnModalityType,
      turnD[turnD$turnType=="T1" & turnD$modalityCondition=='multi',]$condition)


range(turnD$numberOfMultimodalSigs)

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

# Write the data out
write.csv(turnD,"../../data/Final_Turn_data.csv", row.names = F)
