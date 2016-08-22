library(lme4)
library(party)
library(sjPlot)
library(gplots)
setwd("~/Documents/MPI/ViniciusMultimodal/multimodalCommunicationGame/experiment/analysis/R/")

d = read.csv("../../data/Final_Turn_data.csv")

d$game = d$game + 1

x = table(d$dyadNumber,d$itemId)
x[,] = 0

d = d[order(d$dyadNumber,d$game,d$trial),]

d$cumNumT2 = 0
for(i in 1:nrow(d)){
  if(d[i,]$turnType=="T2"){
    x[d[i,]$dyadNumber, d[i,]$itemId] = 
      x[d[i,]$dyadNumber, d[i,]$itemId] + 1
  }
  d[i,]$cumNumT2 = x[d[i,]$dyadNumber, d[i,]$itemId]
}

x = table(d$dyadNumber,d$itemId)
x[,] = 0
d$cumNumTurns = 0
for(i in 1:nrow(d)){
  d[i,]$cumNumTurns = x[d[i,]$dyadNumber, d[i,]$itemId]
  x[d[i,]$dyadNumber, d[i,]$itemId] = 
    x[d[i,]$dyadNumber, d[i,]$itemId] + 1
}


d = d[d$turnType=="T1",]
d = d[!duplicated(d$trialString),]

d$turnLength.logcenter = log(d$turnLength)
meanLogTurnLength = mean(d$turnLength.logcenter)
d$turnLength.logcenter  = d$turnLength.logcenter - meanLogTurnLength

d$multi = d$turnModalityType=="multi"

d$trialTotal = d$trial + (d$game * (max(d$trial)+1))
# Convert to proportion of games played, so that estimates reflect change per game.
d$trialTotal = d$trialTotal / 16

# Center the trialTotal variable so intercept reflects after the first game
d$trialTotal = d$trialTotal - 2

# Quadratic effect
d$trialTotalQ = d$trialTotal^2






cx = ctree(turnLength~ 
             trialTotal + trialTotalQ +
             modalityCondition+condition +
             multi + cumNumT2 + cumNumTurns,
           data = d)
plot(cx, terminal_panel=node_barplot)

m0 = lmer(turnLength.logcenter~ 
            trialTotal + trialTotalQ +
            modalityCondition*condition*multi +
            trialTotal:modalityCondition +
            trialTotal:condition +
            trialTotal:multi +
            trialTotalQ:modalityCondition +
            trialTotalQ:condition +
            trialTotalQ:multi +
            #cumNumT2*modalityCondition +
            #cumNumT2:condition +
            #cumNumT2:trialTotal +
            cumNumTurns*modalityCondition*condition +
            cumNumTurns:trialTotal +
            (1 | dyadNumber / playerId) +
            (1 + modalityCondition|itemId),
          data = d)




convertEst = function(X){
  exp(meanLogTurnLength+X) - exp(meanLogTurnLength)
}

x = sjp.lmer(m0, 'fe', fade.ns = T,
             geom.colors = c(1,1),
             prnt.plot = F,
             xlab= "Turn length (ms)",
             p.kr = FALSE,
             show.values = F,
             show.p = F)

x$plot.list[[1]]$data$estimate =convertEst(x$plot.list[[1]]$data$estimate)
x$plot.list[[1]]$data$conf.low = convertEst(x$plot.list[[1]]$data$conf.low)
x$plot.list[[1]]$data$conf.high =  convertEst(x$plot.list[[1]]$data$conf.high)

# Plot the fixed effects 
x


####
# Raw data

pdf(file="../../results/graphs/T1_Efficiency.pdf",width=10, height=6)

par(mfrow=c(1,2))
for(stimType in unique(d$condition)){
  
  plotmeans(turnLength/1000~game,
            data = d[d$modalityCondition=='vocal' & !duplicated(d$trialString)
                     & d$condition==stimType,],
            col=1,barcol = 1,
            xaxt='n',ylim=c(0,20),
            n.label = F,
            xlab='Game', ylab="T1 length (s)")
  plotmeans(turnLength/1000~game,
            data = d[d$modalityCondition=='multi' & !duplicated(d$trialString)
                     & d$condition==stimType,],
            add=T,col=2,barcol = 2,n.label = F,
            xaxt='n')
  
  plotmeans(turnLength/1000~game,
            data = d[d$modalityCondition=='visual' & !duplicated(d$trialString)
                     & d$condition==stimType,],
            add=T,col=3,barcol = 3,n.label = F,
            ylim=c(0,20),
            xlab="Game",
            ylab="T1 length (s)", las=1)
  
  if(stimType=="Visual"){
    legend(1.5,20,legend=c('Acoustic','Multimodal','Visual'), col=1:3,lty=1,pch=1)
  }
  title(main=stimType)
}
dev.off()




