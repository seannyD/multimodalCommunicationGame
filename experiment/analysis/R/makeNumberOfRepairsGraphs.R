library(gplots)
library(RColorBrewer)
setwd("~/Documents/MPI/ViniciusMultimodal/multimodalCommunicationGame/experiment/analysis/R/")
d = read.csv("../../data/FinalSignalData.csv")

d$game = d$game + 1

numberOfRepairs = tapply(d$turnType, d$trialString, function(X){
  sum(X %in% c("T2","T4","T6",'T8','T10'))
})

condition = tapply(d$condition, d$trialString, head,n=1)
modalityCondition = tapply(d$modalityCondition, d$trialString, head,n=1)
game = tapply(d$game, d$trialString, head,n=1)

categories = paste(modalityCondition,condition,game)

plotmeans(numberOfRepairs[modalityCondition=="vocal" & condition=="Auditory"]~ 
            categories[modalityCondition=="vocal" & condition=="Auditory"],
          connect=T,
          xaxt='n',
          xlab='Condition',
          ylab="Number of repairs")
axis(1,at=c(1.5,3.5,5.5), c("Multimodal",'Visual',"Vocal"))




visual = table(d[d$condition=="Visual",]$turnType %in% c("T2","T4","T6",'T8','T10'), 
          d[d$condition=="Visual",]$game, 
          d[d$condition=="Visual",]$modalityCondition)
auditory = table(d[d$condition=="Auditory",]$turnType %in% c("T2","T4","T6",'T8','T10'), 
               d[d$condition=="Auditory",]$game, 
               d[d$condition=="Auditory",]$modalityCondition)

cols = brewer.pal(3,'Pastel1')
yrange = c(0,35)


pdf("../../results/graphs/Repairs/TotalNumberOfRepairs.pdf")
par(mfrow=c(1,2))

barplot(t(visual[2,,]), beside=T,
        xlab='Game',
        ylab="Total number of repairs",
        col = cols,
        main = "Visual stimuli",
        ylim=yrange)

barplot(t(auditory[2,,]), beside=T,
        xlab='Game',
        ylab="Total number of repairs",
        col = cols,
        main = "Auditory stimuli",
        ylim=yrange)

legend(12, 40, legend=c("Multimodal",'Visual',"Vocal"),
       pch=15,
       col = cols)
dev.off()