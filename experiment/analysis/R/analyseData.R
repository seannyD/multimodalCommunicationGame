# This file loads the data in ../data/csv/*.csv and compiles it into a single file
#  plus some handy variables

library(gplots)

# 16 trials per game
# 64 per block
# 128 per experiment
# 1920 trials in 15 dyads


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

overlaps2 = function(t1Start,t1End,t2Start,t2End){
  if(t1Start < t2Start){
    return(t1End - t2Start)
  } else{
    return(t2End - t1Start)
  }
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


doAnalysis = function(basedir, writefile){
    
    
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
    
    d$playerId = paste(d$dyadNumber,d$signallingPlayer)
    
    d$itemId = paste(d$condition,d$target)
    
    # we don't have info on when the very last trial of each block ends, so assume it's the end of the trial
    d[d$game==3 & d$trial==15,]$startOfNextTrial = d[d$game==3 & d$trial==15,]$trialEnd
    
    # fix stuff
    
    d$turnType  = gsub("\\.","",d$turnType)
    
    # There are some turns with lengths less than 200 milliseconds.  These are coding artefacts which should be removed
    d = d[d$turnLength>200,]
    
    # There are some signals with lengths less than 200 milliseconds.  These are coding artefacts which should be removed
    d = d[d$signalLength>200,]
    
    
    # Exclude cases
    
    ex = read.table("../../data/TrialsToExclude.txt", stringsAsFactors = F, header=T)
    for(i in 1:nrow(ex)){
      d = d[!(d$game==ex[i,]$game & 
              d$dyadNumber==ex[i,]$dyad & 
              d$condition==ex[i,]$condition &
              d$trial==ex[i,]$trial),]
    }
    
    
    # Remove first turn by matcher
    d = d[d$turnType!='T0',]
    
    # Check for gaps
    
    checkForGaps = table(d$dyadNumber,d$game,d$trial)
    sum(checkForGaps==0)
    gaps = which(checkForGaps==0, arr.ind=T)
    if(nrow(gaps)>0){
      for(i in 1:nrow(gaps)){
        print(
          c(sort(unique(d$dyadNumber))[gaps[i,1]],
          "Game",sort(unique(d$game))[gaps[i,2]],
          "Trial",sort(unique(d$trial))[gaps[i,3]])
        )
      }
    }
    
    
    
    
    ##############################
    # Are there any turns that belong to two trials?
    # This can happen if the turns overlap with two trials
    # If so, just match the turn with the trial with which it overlaps the most
    
    d$turnString = paste(d$condition,d$modalityCondition,d$dyadNumber,d$turnStart,d$turnEnd)
    numTrials = tapply(d$trialString, d$turnString,function(X){length(unique(X))})
    
    casesToExclude = c()
    for(turnName in names(numTrials[numTrials>1])){
      dx = d[d$turnString == turnName,]
      
      t1 = dx[dx$trial==min(dx$trial),][1,]
      t2 = dx[dx$trial==max(dx$trial),][1,]
      
      t1and2= rbind(t1,t2)[order(c(t1$trialStart,t2$trialEnd)),]
      
      turnS = t1$turnStart[1]
      turnE = t1$turnEnd[1]
      
      overlap1 = overlaps2(t1$trialStart[1],t1$trialEnd[1],turnS,turnE)
      overlap2 = overlaps2(t2$trialStart[1],t2$trialEnd[1],turnS,turnE)
      
      if(overlap1 > overlap2){
        # delete t2
        casesToExclude  = c(casesToExclude,
                            which(d$turnString==turnName & d$trialString==t2$trialString))
      } else{
        # delete t1
        casesToExclude  = c(casesToExclude,
                            which(d$turnString==turnName & d$trialString==t1$trialString))
      }
    
    }
    
    if(length(casesToExclude)>0){
      # take out cases 
      d = d[- casesToExclude,]
    }
    
    
    # work out trail length
    #  From start of first turn to when the listener selects a meaning.
    #  The trials track the matchers turn, so that the end of the trial is when
    #  the matcher makes a choice.
    trialLengths = tapply(d$trialEnd, d$trialString, max)- tapply(d$turnStart, d$trialString, head,n=1)
    
    d$trialLength = trialLengths[d$trialString]
    
    d = d[d$trialLength> 2000,]
    
    d$correct = d$correct=="Correct"
    
    # Check for gaps again
    
    checkForGaps = table(d$dyadNumber,d$condition,d$game,d$trial)
    sum(checkForGaps==0)
    gaps = which(checkForGaps==0, arr.ind=T)
    if(nrow(gaps)>0){
      cat("",file="../../processing/ListOfTrialsWithoutTurns.txt")
      for(i in 1:nrow(gaps)){
        cat(
        paste(
            sort(unique(d$dyadNumber))[gaps[i,1]],
            "Signals",sort(unique(d$condition))[gaps[i,2]],
            "Game",sort(unique(d$game))[gaps[i,3]],
            "Trial",sort(unique(d$trial))[gaps[i,4]],"\n", sep=' '),
          file = "../../processing/ListOfTrialsWithoutTurns.txt",
          append = T
        )
         
        
      }
    }
    
    # Fix assignments to Director/Matcher.
    # T1, T3, T5 etc always refer to Director
    # T2, T4, T6 etc. always refer to Matcher

    d[d$role=="Director" & d$turnType %in% c("T2","T4",'T6','T8','T10'),]$role = "Matcher"
    d[d$role=="Matcher" & d$turnType %in% c("T1","T3",'T5','T7','T9','T11'),]$role = "Director"
    
    
    # Did matcher respond in this trial?
    
    matcherResponds = tapply(d$turnType, d$trialString, function(X){
      any(X %in% c("T2","T4","T6","T8",'T10'))
    })
    d$matcherResponds = matcherResponds[d$trialString]
    
    
    # Work out cumulative number of trials with interaction
    d$matcherResponds.cumulative = NA
    for(dyadx in unique(d$dyadNumber)){
      for(conditionx in c("Auditory",'Visual')){
        sel = d$dyadNumber== dyadx & d$condition==conditionx
        # second part is to get order correct
        mr2 = tapply(d[sel,]$matcherResponds, d[sel,]$trialString, head,n=1)[unique(d[sel,]$trialString)]
        mr2 = cumsum(mr2)
        d[sel,]$matcherResponds.cumulative = mr2[d[sel,]$trialString]
      }
    }
    
    # Work out cumulative number of extra turns beyond T1
    d$numTurns.cumulative = NA
    for(dyadx in unique(d$dyadNumber)){
      for(conditionx in c("Auditory",'Visual')){
        sel = d$dyadNumber== dyadx & d$condition==conditionx
        # second part is to get order correct
        mr2 = tapply(d[sel,]$turnType, d[sel,]$trialString, function(X){length(unique(X))})[unique(d[sel,]$trialString)]
        # make it number of additional turns, otherwise it's highly correlated with number of turns
        mr2 = mr2 -1
        mr2 = cumsum(mr2)
        d[sel,]$numTurns.cumulative = mr2[d[sel,]$trialString]
      }
    }
    
    # Work out amount of time in each modality
    
    # Fix turn counts
    #tapply(d$trialString,)
    
    # Write the data to a main file
    write.csv(d, writefile)
  }


doAnalysis("../../data/csv/","../../data/FinalSignalData.csv")
doAnalysis("../../data/reliabilityCoding_csv/","../../data/FinalSignalData_reliabilityCoding.csv")