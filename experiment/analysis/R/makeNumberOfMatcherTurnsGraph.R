setwd("~/Documents/MPI/ViniciusMultimodal/multimodalCommunicationGame/experiment/analysis/R/")
d = read.csv("../../data/Final_Turn_data.csv", stringsAsFactors = F)


matcherResponds = tapply(d$turnType, d$trialString, function(X){
  any(X %in% c("T2","T4","T6","T8",'T10'))
})
condition = tapply(d$condition, d$trialString, head,n=1)
modalityCondition = tapply(d$modalityCondition, d$trialString, head,n=1)

dx = data.frame(condition=condition,
                modalityCondition = modalityCondition,
                matcherResponds = matcherResponds)

tx = table(dx$matcherResponds, dx$condition,dx$modalityCondition)



tx2 = cbind(t(t(tx[2,,3] / colSums(tx[,,3]))),
      t(t(tx[2,,1] / colSums(tx[,,1]))),
      t(t(tx[2,,2] / colSums(tx[,,2]))))

colnames(tx2) = c("Vocal only","Multimodal","Gesture only")

tx2 = round(tx2 * 100,1)

write.csv(tx2, file='../../results/tables/PercentOfTrialsWhereMatcherResponds.csv')
