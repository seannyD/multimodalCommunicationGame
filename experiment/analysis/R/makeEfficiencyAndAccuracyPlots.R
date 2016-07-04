library(gplots)
setwd("~/Documents/MPI/ViniciusMultimodal/multimodalCommunicationGame/experiment/analysis/R/")
d = read.csv("../../data/FinalSignalData.csv")

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
