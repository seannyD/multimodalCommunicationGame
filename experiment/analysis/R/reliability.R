library(psych)
# FS: "I have one remark about the second part of Dyad 18 (the one with the two guys). I found a few sections more difficult to do. This was because they communicate so fast between the two of them that it isn't always clear who is taking the turn. One of the guys also keeps making these noises to emphasize where he's pointing at - I didn't code this as being a sound to illustrate which image he was looking at."

# All but one of the disagreements in modality assignment involved the primary coder coding a turn as multimodal while the secondary coder coded it as unimodal.  Prior to the reliability analysis, the secondary coder flagged 7 trials which were difficult to code because of the speed of the interaction and that they were unsure how to code "small noises to emphasize where [the participant is] pointing at".  Had these been coded according to the coding definitions, then the modality would match between the coders in 90% of trials.


library(sjPlot)
setwd("~/Documents/MPI/ViniciusMultimodal/multimodalCommunicationGame/experiment/analysis/R/")
d = read.csv("../../data/FinalSignalData.csv", stringsAsFactors = F)

r = read.csv("../../data/FinalSignalData_reliabilityCoding.csv", stringsAsFactors = F)

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

getTurnModality = function(turnData){

  numModalitiesUsed = length(unique(turnData$modality))
  useVisual = "Visual" %in% turnData$modality
  useAcoustic = "Acoustic" %in% turnData$modality
  numberOfOverlaps = 0
  propOverlapsActs = 0
  propOverlapsTime = 0
  numberOfMultimodalSigs = 0
  
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
  
  turnModalityType = "none"
  if(numberOfMultimodalSigs>0){turnModalityType = "multi"}
  if(numberOfMultimodalSigs==0 & useVisual & useAcoustic){turnModalityType = "unimodal mixed"}
  if(numberOfMultimodalSigs==0 & useVisual & (!useAcoustic)){turnModalityType = "unimodal visual"}
  if(numberOfMultimodalSigs==0 & (!useVisual) & useAcoustic){turnModalityType = "unimodal acoustic"}
  
  return(turnModalityType)
  
}
                           




d = d[d$trialString %in% r$trialString,]
r = r[r$trialString %in% d$trialString,]

#d = d[d$filename=='D12_TT_Coded.eaf',]
#r = r[r$filename=='D12_TT_Coded.eaf',]

trials = unique(r$trialString)

res = data.frame(stringsAsFactors = F)
resSignals = data.frame(stringsAsFactors = F)

for(t in trials){
  dx = d[d$trialString ==t,]
  rx = r[r$trialString ==t,]
  
  turn1StartDiff = dx$turnStart[1] - rx$turnStart[1]
  turn1EndDiff = dx$turnEnd[1] - rx$turnEnd[1]
  # T1 length
  turn1LengthDiff = dx$turnLength[1] - rx$turnLength[1] 

  # Num turns
  numTurnDiff = length(unique(dx$turnString)) - length(unique(rx$turnString))
      
  # Multimodal signal
  d.mod = getTurnModality(dx[dx$turnString== dx$turnString[1],])
  r.mod = getTurnModality(rx[rx$turnString== rx$turnString[1],])
      
  modalitySame = d.mod == r.mod
  
  resx = data.frame(dyad = dx$dyadNumber[1],
                    condition = dx$condition[1],
                    trial=t,
                    modalityCondition=dx$modalityCondition[1],
                    turn1LengthDiff=turn1LengthDiff, 
                    dLength =  dx$turnLength[1],
                    rLength =  rx$turnLength[1],
                    numTurnDiff=numTurnDiff,
                    d.numTurn = length(unique(dx$turnString)),
                    r.numTurn = length(unique(rx$turnString)),
                    modalitySame=modalitySame, 
                    d.mod=d.mod,
                    r.mod=r.mod,
                    stringsAsFactors = F)
  
  res = rbind(res,resx)
  
  if(nrow(dx)==nrow(rx)){
    resxS = data.frame(
      dStart = dx$turnStart-dx$signalStart,
      dEnd = dx$turnStart-dx$signalEnd,
      rStart = dx$turnStart-rx$signalStart,
      rEnd = dx$turnStart-rx$signalEnd,
      dModality = dx$modality,
      rModality = rx$modality,
      modalityCondition = dx$modalityCondition[1]
    )
    resSignals = rbind(resSignals,resxS)
  }
}

# Turn length

mean(abs(res$turn1LengthDiff))
# V is longer
plot(res$dLength, res$rLength)
abline(a=0,b=1,col=2)
cor(res$dLength, res$rLength)

# Permuation test
perm = function(X,Y){
  mean(abs(sample(X) -Y))
}
trueDiff = mean(abs(res$dLength - res$rLength))
set.seed(2389)
permDiff = replicate(1001,perm(res$dLength, res$rLength))
sum(permDiff<trueDiff)
mean(permDiff)

z = (trueDiff - mean(permDiff)) / sd(permDiff)
z



# When number of turns are the same
mean(abs(res[res$numTurnDiff==0,]$turn1LengthDiff))
cor(res[res$numTurnDiff==0,]$dLength, res[res$numTurnDiff==0,]$rLength)
# No significant differences between conditions
summary(lm(res$turn1LengthDiff~res$condition*res$modalityCondition))

#####
# Signal timing
resSignals = resSignals[resSignals$dModality==resSignals$rModality,]

resSignals$StartDiff = resSignals$dStart - resSignals$rStart
resSignals$EndDiff = resSignals$dEnd - resSignals$rEnd

hist(resSignals$StartDiff)
trueDiff.start = mean(abs(resSignals$StartDiff))
quantile(abs(resSignals$StartDiff),c(0.25,0.75))

hist(resSignals$EndDiff)
trueDiff.end = mean(abs(resSignals$EndDiff))
quantile(abs(resSignals$EndDiff),c(0.25,0.75))

mean(d$signalLength)
sd(d$signalLength)

summary(lm(abs(StartDiff)~dModality+modalityCondition,data=resSignals))
t.test(abs(StartDiff)~dModality,data=resSignals)

mean(abs(resSignals[resSignals$dModality=="Acoustic",]$StartDiff))
mean(abs(resSignals[resSignals$dModality=="Visual",]$StartDiff))

set.seed(2389)
permDiff.Start = replicate(1001,perm(resSignals$dStart, resSignals$rStart))
z2 = (trueDiff.start - mean(permDiff.Start)) / sd(permDiff.Start)
z2

sum(permDiff.Start<trueDiff.start)

permDiff.End = replicate(1001,perm(resSignals$dEnd, resSignals$rEnd))
z3 = (trueDiff.end - mean(permDiff.End)) / sd(permDiff.End)
z3

sum(permDiff.End<trueDiff.end)

##########
# Modality coding
mt = res$modalityCondition=='multi'
sum(res[mt,]$modalitySame) / sum(mt)
table(res[mt,]$d.mod, as.numeric(as.factor(res[mt,]$r.mod)))

res[!res$modalitySame & mt,]

cohen.kappa(x=cbind(res[mt,]$d.mod,res[mt,]$r.mod))

# ignoring dyad 18
sum(res[mt & !res$dyad=='D18',]$modalitySame)/sum(mt & !res$dyad=='D18')

mt2 = res$modalityCondition=='multi' & !(res$dyad=='D18' & res$condition=='Visual')
sum(res[mt2,]$modalitySame) / sum(mt2)
table(res[mt2,]$d.mod, as.numeric(as.factor(res[mt2,]$r.mod)))

cohen.kappa(x=cbind(res[mt2,]$d.mod,res[mt2,]$r.mod))

#library(party)
#res2=  res[,c('modalitySame','turn1LengthDiff', 'numTurnDiff', 'd.mod')]
#res2$d.mod = as.factor(res2$d.mod)
#res2$modalitySame = as.factor(res2$modalitySame)
#ctx = ctree(modalitySame~., data=res2)
#plot(ctx)



# Number of turns
sum(res$numTurnDiff==0) / nrow(res)

cohen.kappa(x=cbind(res$d.numTurn,res$r.numTurn))

#########
# Reliability coding

# The main dependent variables in the analysis were trial accuracy and trial length.  These were both calculated by computer and did not require coder judgements.  The only measures that depended on the coder reliability were the modality of the director's first turn and two minor independent variables: the number of turns in the trial and the length of the director's first turn. 

# 239 trials were coded by a naive second coder (12.7% of all trials, from 9 dyads, about 30 minutes in randomly chosen sections, covering all combinations of stimulus type and modality condition).

# Number of turns
# For 95% of trials, the two coders agreed on the number of turns in the trial (Cohen's weighted Kappa = 0.84).  

# Director T1 turn length
# When looking at the trials where the coders agreed on the number of turns, the coders' trial turn length codings were correlated with r = 0.98.   

# T1 Modality
# T1 turn modality is only possibly contentious for the multimodal signal condition.  The modality of a turn depends on the timing of signals: if an acoustic signal and a visual signal overlap in time, then the turn is coded as multimodal.  98 multimodal-condition trials were coded by both coders.  The raters showed good agreement (Cohen's Kappa = 0.81, 85% of turns coded identically).  All but one of the disagreements involved the primary coder coding T1 as multimodal while the secondary coder coded T1 as unimodal.  Prior to the reliability analysis, the secondary coder flagged 7 trials which were difficult to code because of the speed of the interaction and that they were unsure how to code "small noises to emphasize where [the participant is] pointing at".  Had these been coded according to the coding definitions, then the modality would match between the coders in 90% of trials.

