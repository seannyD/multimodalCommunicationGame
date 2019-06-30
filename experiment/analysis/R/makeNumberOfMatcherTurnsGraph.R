setwd("~/Documents/MPI/ViniciusMultimodal/multimodalCommunicationGame/experiment/analysis/R/")
d = read.csv("../../data/Final_Turn_data.csv", stringsAsFactors = F)


directorResponds = tapply(d$turnType, d$trialString, function(X){
  any(X %in% c("T3","T5","T7",'T9',"T11"))
})
prop.table(table(directorResponds))
multiTrials = unique(d[d$modalityCondition=="multi",]$trialString)
prop.table(table(directorResponds[multiTrials]))

dirResp = d[d$trialString %in% names(which(directorResponds[multiTrials])),]
prop.table(table(dirResp[dirResp$turnType %in% c("T1"),]$turnModalityType))
prop.table(table(dirResp[dirResp$turnType %in% c("T3"),]$turnModalityType))

dirFirstSigType = tapply(dirResp[dirResp$turnType %in% c("T1"),]$turnModalityType,
      dirResp[dirResp$turnType %in% c("T1"),]$trialString,
      function(X){X[1]})
dirSecondSigType = tapply(dirResp[dirResp$turnType %in% c("T3"),]$turnModalityType,
                         dirResp[dirResp$turnType %in% c("T3"),]$trialString,
                         function(X){X[1]})


table(dirFirstSigType,dirSecondSigType)

####

matcherResponds = tapply(d$turnType, d$trialString, function(X){
  any(X %in% c("T2","T4","T6","T8",'T10'))
})
condition = tapply(d$condition, d$trialString, head,n=1)
modalityCondition = tapply(d$modalityCondition, d$trialString, head,n=1)

accuracy = tapply(d$correct, d$trialString, head,n=1)
gameNumber = tapply(d$game, d$trialString, head,n=1)

dyadNumber = tapply(d$dyadNumber,  d$trialString, head, n=1)

dx = data.frame(condition=condition,
                modalityCondition = modalityCondition,
                matcherResponds = matcherResponds)

tx = table(dx$matcherResponds, dx$condition,dx$modalityCondition)



tx2 = cbind(t(t(tx[2,,3] / colSums(tx[,,3]))),
      t(t(tx[2,,1] / colSums(tx[,,1]))),
      t(t(tx[2,,2] / colSums(tx[,,2]))))

colnames(tx2) = c("Vocal only","Multimodal","Gesture only")

tx2 = round(tx2 * 100,1)
tx2
write.csv(tx2, file='../../results/tables/PercentOfTrialsWhereMatcherResponds.csv')




dix = data.frame(condition=condition,
                 modalityCondition = modalityCondition,
                 matcherResponds = matcherResponds,
                 accuracy=accuracy,
                 gameNumber=gameNumber,
                 dyadNumber=dyadNumber )

totalMatcherRespondTrials = tapply(dix[dix$gameNumber<3,]$matcherResponds, dix[dix$gameNumber<3,]$dyadNumber, mean)
accuracy.pre = tapply(dix[dix$gameNumber<3,]$accuracy, dix[dix$gameNumber<3,]$dyadNumber, mean)
accuracy.post = tapply(dix[dix$gameNumber==3,]$accuracy, dix[dix$gameNumber==3,]$dyadNumber, mean)

plot(totalMatcherRespondTrials, accuracy.post)

summary(lm(accuracy.post ~ accuracy.pre+totalMatcherRespondTrials))
