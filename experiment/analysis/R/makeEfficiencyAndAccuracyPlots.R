library(gplots)
setwd("~/Documents/MPI/ViniciusMultimodal/multimodalCommunicationGame/experiment/analysis/R/")
d = read.csv("../../data/FinalSignalData.csv")

d$game = d$game +1

#d = d[d$correct,]

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
            ylim=c(0,20),
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
    legend(2.5,20,legend=c('Acoustic','Multimodal','Visual'), col=1:3,lty=1,pch=1)
  }
  title(main=stimType)
}
dev.off()


############
# Accuracy

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
    legend(2.5,0.5,legend=c('Acoustic','Multimodal','Visual'), col=1:3,lty=1,pch=1)
  }
  title(main=stimType)
}
dev.off()


###########
#

d2 = read.csv("../../data/Final_Turn_data.csv", stringsAsFactors = F)
d2$game = d2$game +1

pdf("../../results/graphs/Efficiency_MultimodalCondition.pdf", width=10, height=6)
par(mfrow=c(1,2))
for(stimType in unique(d2$condition)){
  
  dx = d2[d2$modalityCondition=='multi' 
          & d2$condition==stimType
          & d2$turnType=="T1",]
  dx = dx[!duplicated(dx$trialString), ]
  
  plotmeans(trialLength/1000~game,
            data = dx[dx$turnModalityType=="multi",],
            col=2,barcol = 1,n.label = F,
            lty=1,
            ylim=c(0,20),
            xlab="Game",
            ylab="Trial length (s)", las=1)
  if(sum(dx$turnModalityType=="unimodal acoustic")>0){
  plotmeans(trialLength/1000~game,
            data = dx[dx$turnModalityType=="unimodal acoustic",],
            col=2,barcol = 1,n.label = F,
            lty=2,
            pch=2,
            add = T,
            ylim=c(0,20),
            xlab="",
            ylab="", las=1)
  }
  
  if(sum(dx$turnModalityType=="unimodal visual")>0){
  plotmeans(trialLength/1000~game,
            data = dx[dx$turnModalityType=="unimodal visual",],
            col=2,barcol = 1,n.label = F,
            lty=3,
            pch=4,
            add = T,
            ylim=c(0,20),
            xlab="",
            ylab="", las=1)
  }
 
  title(main=paste(stimType,"stimuli"))
}
legend(2,20, legend=c("Multi","Acoustic","Visual"), col=2, lty=1:3,pch=c(1,2,4))
dev.off()


########

pdf("../../results/graphs/Signal1_Efficiency.pdf", width=10, height=6)
par(mfrow=c(1,2))
for(stimType in unique(d$condition)){
  
  plotmeans(signalLength/1000~game,
            data = d[d$modalityCondition=='vocal' & !duplicated(d$trialString)
                     & d$condition==stimType,],
            col=1,barcol = 1,n.label = F,
            ylim=c(0,10),
            xlab="Game",
            ylab="Signal length (s)", las=1)
  plotmeans(signalLength/1000~game,
            data = d[d$modalityCondition=='multi' & !duplicated(d$trialString)
                     & d$condition==stimType,],
            add=T,col=2,barcol = 2,
            xaxt='n',
            n.label = F)
  plotmeans(signalLength/1000~game,
            data = d[d$modalityCondition=='visual' & !duplicated(d$trialString)
                     & d$condition==stimType,],
            add=T,col=3,barcol = 3,n.label = F,
            xaxt='n')
  if(stimType=="Auditory"){
    legend(2.5,10,legend=c('Visual','Multimodal','Acoustic'), col=3:1,lty=1,pch=1)
  }
  title(main=stimType)
}
dev.off()


