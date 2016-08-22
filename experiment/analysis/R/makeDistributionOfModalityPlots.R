# This file creates mirrored histograms, and also the data file "../../data/Final_Multimodal_data.csv"

library(gplots)
setwd("~/Documents/MPI/ViniciusMultimodal/multimodalCommunicationGame/experiment/analysis/R/")
d = read.csv("../../data/FinalSignalData.csv", stringsAsFactors = F)

# load function to make mirrored plots
source("multiHist.R")

getPropTimeAcoustic = function(acousticSel,visualSel){
  
  # Add the signal time for each modality for each trial
  acousticTime = tapply(d[acousticSel,]$signalLength,
                        d[acousticSel,]$trialString,
                        sum)
  
  visualTime = tapply(d[visualSel,]$signalLength,
                      d[visualSel,]$trialString,
                      sum)
  
  dyads.vis = tapply(d[visualSel,]$dyadNumber,
                      d[visualSel,]$trialString,
                      head,n=1)
  games.vis = tapply(d[visualSel,]$game,
                 d[visualSel,]$trialString,
                 head,n=1)
  dyads.ac = tapply(d[acousticSel,]$dyadNumber,
                     d[acousticSel,]$trialString,
                     head,n=1)
  games.ac = tapply(d[acousticSel,]$game,
                     d[acousticSel,]$trialString,
                     head,n=1)
  
  # Make sure there's a time for each trial
  # if the trial time is NA, set it to zero
  allTrials = unique(d[d$modalityCondition=="multi",]$trialString)
  visualTime= visualTime[allTrials]
  visualTime[is.na(visualTime)] = 0
  acousticTime= acousticTime[allTrials]
  acousticTime[is.na(acousticTime)] = 0
  
  propAcousticSignals = acousticTime / (acousticTime+visualTime)
  propAcousticSignals[is.nan(propAcousticSignals)] = NA
  names(propAcousticSignals) = allTrials
  
 #propAcousticSignals = propAcousticSignals[!is.na(propAcousticSignals)]
  
  #x = hist(propAcousticSignals, plot = F)
  #x$counts = x$counts/length(unique(d[d$modalityCondition=="multi",]$trialString))
  #plot(x, ylab="Proportion of trials")
  
  propAcousticSignals_AuditoryStim = 
    propAcousticSignals[grepl("Auditory",names(propAcousticSignals))]
  propAcousticSignals_VisualStim = 
    propAcousticSignals[grepl("Visual",names(propAcousticSignals))]
  
  return(list(propAcousticSignals_AuditoryStim=propAcousticSignals_AuditoryStim,
              propAcousticSignals_VisualStim=propAcousticSignals_VisualStim,
              dayds.ac= dyads.ac[!is.na(dyads.ac)],
              games.ac = games.ac[!is.na(games.ac)],
              dyads.vis=dyads.vis[!is.na(dyads.vis)],
              games.vis=games.vis[!is.na(games.vis)]
              ))
}

############


mx = d[d$modalityCondition=='multi',]
mx = mx[!duplicated(mx$trialString),]

# which cases are T1s with acoustic/visual signals?
acousticT1s = d$modalityCondition=="multi" & d$turnType=='T1' & d$modality=="Acoustic"
visualT1s = d$modalityCondition=="multi" & d$turnType=='T1' & d$modality=="Visual"

x = getPropTimeAcoustic(acousticT1s,visualT1s)
propAcousticSignals_AuditoryStim = x[[1]]
propAcousticSignals_VisualStim = x[[2]]
dyads_propAcousticSignals_A= x[[3]]
games_propAcousticSignals_A= x[[4]]
dyads_propAcousticSignals_V= x[[5]]
games_propAcousticSignals_V= x[[6]]

mx$T1.propAc = c(propAcousticSignals_AuditoryStim,
                 propAcousticSignals_VisualStim
                 )[mx$trialString]


# Plot histograms
cols= c(rgb(0,1,0,0.5),rgb(1,0,0,0.5))
breaks = seq(0,1,0.05)
hist(propAcousticSignals_AuditoryStim, col=cols[1], border=cols[1], breaks=breaks)
hist(propAcousticSignals_VisualStim, add=T, col=cols[2], border = cols[2], breaks=breaks)

par("../../results/graphs/PropModality/PropModality_T1_2Hist.pdf")
par(mfrow=c(2,1))
hist(propAcousticSignals_AuditoryStim, col='white',  breaks=breaks, main='Auditory Stimuli', xlab='', ylab='Number of cases')
hist(propAcousticSignals_VisualStim, col='white', breaks=breaks, main="Visual Stimuli",xlab='Proportion of acoustic signals', ylab='Number of cases')
dev.off()

# Line distributions
par(mfrow=c(1,1))
audLine = density(propAcousticSignals_AuditoryStim)
visLine = density(propAcousticSignals_VisualStim)

plot(visLine, col=3, lwd=2, xlim=c(0,1))
lines(audLine, lwd=2)
legend(0.3,10, legend = c("Acoustic","Visual"),col= c(1,3), lty=1, lwd=2)





# Mirrored histogram
pdf("../../results/graphs/PropModality/PropModality_T1.pdf")
multhist(propAcousticSignals_AuditoryStim,propAcousticSignals_VisualStim,
         bin.width=0.05,
         col=c(2,3),
         dir=c(1,-1), 
         main="T1",
         legends = c("Auditory","Visual"),
         xlab="Proportion of Acoustic signals",
         ylab="Number of trials")
dev.off()

acousticDirector = d$modalityCondition=="multi" & d$role=="Director" & d$modality=="Acoustic"
visualDirector = d$modalityCondition=="multi" & d$role=="Director" & d$modality=="Visual"

x = getPropTimeAcoustic(acousticDirector ,visualDirector)
propAcousticSignals_AuditoryStim_director = x[[1]]
propAcousticSignals_VisualStim_director = x[[2]]

par("../../results/graphs/PropModality/PropModality_Director_2Hist.pdf")
par(mfrow=c(2,1))
hist(propAcousticSignals_AuditoryStim_director, col='white',  breaks=breaks, main='Auditory Stimuli', xlab='', ylab='Number of cases')
hist(propAcousticSignals_VisualStim_director, col='white', breaks=breaks, main="Visual Stimuli",xlab='Proportion of acoustic signals', ylab='Number of cases')
dev.off()

pdf("../../results/graphs/PropModality/PropModality_Director.pdf")
multhist(propAcousticSignals_AuditoryStim_director,
         propAcousticSignals_VisualStim_director,
         bin.width=0.05,
         col=c(2,3),
         dir=c(1,-1), 
         main="Director Turns",
         legends = c("Auditory","Visual"),
         xlab="Proportion of Acoustic signals",
         ylab="Number of trials")
dev.off()

############
# T2
acousticT2s = d$modalityCondition=="multi" & d$turnType=='T2' & d$modality=="Acoustic"
visualT2s = d$modalityCondition=="multi" & d$turnType=='T2' & d$modality=="Visual"

x = getPropTimeAcoustic(acousticT2s,visualT2s)
propAcousticSignals_AuditoryStimT2 = x[[1]]
propAcousticSignals_VisualStimT2 = x[[2]]

mx$T2.propAc = c(propAcousticSignals_AuditoryStimT2,
                 propAcousticSignals_VisualStimT2
                )[mx$trialString]

# Mirrored histogram
pdf("../../results/graphs/PropModality/PropModality_T2.pdf")
multhist(propAcousticSignals_AuditoryStimT2,propAcousticSignals_VisualStimT2,
         bin.width=0.05,
         col=c(1,2),
         dir=c(1,-1),
         main='T2',
         legends = c("Auditory","Visual"),
         xlab="Proportion of Acoustic signals",
         ylab="Number of trials")
dev.off()
############
# T3 and beyond for Director
acousticT3s = d$modalityCondition=="multi" & 
  d$turnType!='T1' & d$role=="Director" &
  d$modality=="Acoustic"
visualT3s = d$modalityCondition=="multi" &
  d$turnType!='T1' & d$role=="Director" &
  d$modality=="Visual"

x = getPropTimeAcoustic(acousticT3s,visualT3s)
propAcousticSignals_AuditoryStimT3 = x[[1]]
propAcousticSignals_VisualStimT3 = x[[2]]

mx$T3Plus.propAc = c(propAcousticSignals_AuditoryStimT3,
                 propAcousticSignals_VisualStimT3
                )[mx$trialString]

# Mirrored histogram
pdf("../../results/graphs/PropModality/PropModality_Director_postT1.pdf")
multhist(propAcousticSignals_AuditoryStimT3,propAcousticSignals_VisualStimT3,
         bin.width=0.05,
         col=c(1,2),
         dir=c(1,-1),
         main='T3',
         legends = c("Auditory","Visual"),
         xlab="Proportion of Acoustic signals",
         ylab="Number of trials")
dev.off()

# T4 and beyond for Matcher
acousticT4s = d$modalityCondition=="multi" & 
  d$turnType!='T2' & d$role=="Director" &
  d$modality=="Acoustic"
visualT4s = d$modalityCondition=="multi" &
  d$turnType!='T2' & d$role=="Director" &
  d$modality=="Visual"

x = getPropTimeAcoustic(acousticT4s,visualT4s)
propAcousticSignals_AuditoryStimT4 = x[[1]]
propAcousticSignals_VisualStimT4 = x[[2]]

mx$T4Plus.propAc = c(propAcousticSignals_AuditoryStimT4,
                 propAcousticSignals_VisualStimT4
                )[mx$trialString]


# All director turns
acousticD = d$modalityCondition=="multi" & 
  d$role=="Director" &
  d$modality=="Acoustic"
visualD = d$modalityCondition=="multi" &
  d$role=="Director" &
  d$modality=="Visual"

x = getPropTimeAcoustic(acousticD,visualD)
propAcousticSignals_AuditoryStimD = x[[1]]
propAcousticSignals_VisualStimD = x[[2]]

mx$Director.propAc = c(propAcousticSignals_AuditoryStimD,
                     propAcousticSignals_VisualStimD
                    )[mx$trialString]

# All matcher turns
acousticM = d$modalityCondition=="multi" & 
  d$role!="Director" &
  d$modality=="Acoustic"
visualM = d$modalityCondition=="multi" &
  d$role!="Director" &
  d$modality=="Visual"

x = getPropTimeAcoustic(acousticM,visualM)
propAcousticSignals_AuditoryStimM = x[[1]]
propAcousticSignals_VisualStimM = x[[2]]

mx$Matcher.propAc = c(propAcousticSignals_AuditoryStimM,
                       propAcousticSignals_VisualStimM
                      )[mx$trialString]


### How many turns have multimodal utts?
getMultimodalUttsPerTurn <- function(turnSel){
  dx = d[turnSel,]
  # if there's only one signal, or only one modality used
  if(nrow(dx)==1 | length(unique(dx$modality))==1){
    return(list(mutli=0, total=nrow(dx)))
  }
  
  
  
}


for(turn in unique(d$turnString)){
  turnSel = d$modalityCondition=='multi' & d$turnString==turn
}



write.csv(mx,"../../data/Final_Multimodal_Trial_data.csv", row.names = F)

###############
library(lattice)
xyplot(trialLength/1000~game | dyadNumber+condition, 
       data=d[d$modalityCondition=="multi",], 
       type='a', 
       ylim=c(0,40),
       main="Trial length in the multimodal condition")

xyplot(propAcousticSignals_AuditoryStim~games_propAcousticSignals_A|factor(dyads_propAcousticSignals_A), type='p')

xyplot(propAcousticSignals_VisualStim~games_propAcousticSignals_V|factor(dyads_propAcousticSignals_V), type='p')
