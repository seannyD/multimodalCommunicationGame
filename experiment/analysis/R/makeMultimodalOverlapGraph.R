library(ggplot2)
library(RColorBrewer)
library(gridExtra)
setwd("~/Documents/MPI/ViniciusMultimodal/multimodalCommunicationGame/experiment/analysis/R/")
d = read.csv("../../data/FinalSignalData.csv",stringsAsFactors = F)

d = d[d$role=="Director" & d$turnNumber==1 & d$modalityCondition == "multi" & d$condition=='Auditory',]

multimodalSignal = tapply(d$modality, d$trialString, function(X){
  length(unique(X))==2
})

d = d[d$trialString %in% names(multimodalSignal)[multimodalSignal],]

d$signalStart.R = NA
d$signalEnd.R = NA

for(ts in unique(d$trialString)){
  v = d[d$trialString==ts,]$signalStart[1]
  d[d$trialString==ts, ]$signalStart.R = 
    d[d$trialString==ts, ]$signalStart - v
  d[d$trialString==ts, ]$signalEnd.R = 
    d[d$trialString==ts, ]$signalEnd - v
}


####

colx = c(
  rgb(220,117,109,maxColorValue = 255),
  rgb(117,155,252,maxColorValue = 255))

colx = c("#fc8d62","#66c2a5")

aorder = tapply(
  d[d$modality=="Acoustic",]$signalStart.R,
  d[d$modality=="Acoustic",]$trialString,
  head, n=1)

trials = names(sort(aorder))

res = 201
cuts = seq(-0.5,1.5,length.out = res)
counts = rep(0,length.out = res)

for(i in 1:length(trials)){
  ts = trials[i]
  v = d[d$trialString==ts & d$modality=='Visual',][1,]
  a = d[d$trialString==ts & d$modality=='Acoustic',][1,]
  
  aL = a$signalLength / v$signalLength
  aStart = (a$signalStart - v$signalStart) / v$signalLength
  aEnd = aStart + aL
  sel = cuts>aStart & cuts<=aEnd
  counts[sel] = counts[sel] + 1
}

colx = c(
  rgb(117,155,252,maxColorValue = 255),
  rgb(220,117,109,maxColorValue = 255))

colx = c("#66c2a5","#8da0cb")

pdf("../../results/graphs/PropModality/TurnOverlap_Director_Multimodal_AcousticStimuli.pdf", width=5,height = 3)
par(mar=c(3,6,1,1))
barplot(counts, col=colx[2],border=colx[2], 
        space = 0,
        ylim=c(0,300), las=1, ylab="")
text(-60,150,"Number\nof Trials",xpd=T)
rect(which(cuts==0),0,which(cuts==1),300, col='light gray', border=NA)
rect(which(cuts==0),250,which(cuts==1),300, col=colx[1], border=NA)
barplot(counts, col=colx[2],border=colx[2], 
        space = 0,
        ylim=c(0,300), add=T, yaxt='n')
text(which(cuts==0.5),275,"Gestural", col='white')
text(which(cuts==0.5),75,"Vocal", col='white')
arrows(0, y0 = -25 ,x1=length(counts),y1=-25, xpd=T, lwd=2)
text(length(counts)/2, -40,"Time", xpd=T)

dev.off()


### Relative to Acoustic

aorder = tapply(
  d[d$modality=="Visual",]$signalStart.R,
  d[d$modality=="Visual",]$trialString,
  head, n=1)

trials = names(sort(aorder))

res = 201
cuts = seq(-1,1.5,length.out = res)
counts = rep(0,length.out = res)

for(i in 1:length(trials)){
  ts = trials[i]
  # just switch a and v
  a = d[d$trialString==ts & d$modality=='Gesture',][1,]
  v = d[d$trialString==ts & d$modality=='Vocalsat',][1,]
  
  aL = a$signalLength / v$signalLength
  aStart = (a$signalStart - v$signalStart) / v$signalLength
  aEnd = aStart + aL
  sel = cuts>aStart & cuts<=aEnd
  counts[sel] = counts[sel] + 1
}

pdf("../../results/graphs/PropModality/TurnOverlap_Director_Multimodal_AcousticStimuli_RelativeToAcoustic.pdf", width=5,height = 3)
par(mar=c(1,6,1,1))
ymax = 300
barplot(counts, col=colx[1],border=colx[1], 
        space = 0,
        ylim=c(0,ymax), las=1, ylab="")
text(-60,150,"Number\nof Trials",xpd=T)
rect(which(cuts==0),0,which(cuts==1),ymax, col='light gray', border=NA)
rect(which(cuts==0),250,which(cuts==1),ymax, col=colx[2], border=NA)
barplot(counts, col=colx[1],border=colx[1], 
        space = 0,
        ylim=c(0,ymax), add=T, yaxt='n')
text(which(cuts==0.5),275,"Vocalisation", col='white')
text(which(cuts==0.5),75,"Gesture", col='white')
dev.off()








########################
### For Matchers
########################

d = read.csv("../../data/FinalSignalData.csv",stringsAsFactors = F)

d = d[d$role=="Matcher" & d$modalityCondition == "multi" ,]

firstMatcher = tapply(d$turnString,d$trialString, head,n=1)
d = d[d$turnString %in% firstMatcher,]

multimodalSignal = tapply(d$modality, d$trialString, function(X){
  length(unique(X))==2
})

d = d[d$trialString %in% names(multimodalSignal)[multimodalSignal],]

d$signalStart.R = NA
d$signalEnd.R = NA

for(ts in unique(d$trialString)){
  v = d[d$trialString==ts,]$signalStart[1]
  d[d$trialString==ts, ]$signalStart.R = 
    d[d$trialString==ts, ]$signalStart - v
  d[d$trialString==ts, ]$signalEnd.R = 
    d[d$trialString==ts, ]$signalEnd - v
}


####

colx = c(
  rgb(220,117,109,maxColorValue = 255),
  rgb(117,155,252,maxColorValue = 255))

colx = c("#fc8d62","#66c2a5")

aorder = tapply(d[d$modality=="Acoustic",]$signalStart.R,
                d[d$modality=="Acoustic",]$trialString,
                function(X){
                  X[1]
                })

trials = names(sort(aorder))

res = 201
cuts = seq(-0.5,1.5,length.out = res)
counts = rep(0,length.out = res)

for(i in 1:length(trials)){
  ts = trials[i]
  v = d[d$trialString==ts & d$modality=='Visual',][1,]
  a = d[d$trialString==ts & d$modality=='Acoustic',][1,]
  
  aL = a$signalLength / v$signalLength
  aStart = (a$signalStart - v$signalStart) / v$signalLength
  aEnd = aStart + aL
  sel = cuts>aStart & cuts<=aEnd
  counts[sel] = counts[sel] + 1
}





pdf("../../results/graphs/PropModality/TurnOverlap_Matcher_Multimodal_AcousticStimuli.pdf", width=5,height = 3)
##
ymax = 12
par(mar=c(1,6,1,1))
barplot(counts, col=colx[2],border=colx[2], 
        space = 0,
        ylim=c(0,ymax), las=1, ylab="")
text(-60,6,"Number\nof Trials",xpd=T)
rect(which(cuts==0),0,which(cuts==1),12, col='light gray', border=NA)
rect(which(cuts==0),10,which(cuts==1),12, col=colx[1], border=NA)
barplot(counts, col=colx[2],border=colx[2], 
        space = 0,
        ylim=c(0,ymax), add=T, yaxt='n')
text(which(cuts==0.5),11,"Gesture", col='white')
text(which(cuts==0.5),4,"Vocalisation", col='white')
dev.off()
