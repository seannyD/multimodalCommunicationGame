setwd("~/Documents/MPI/ViniciusMultimodal/multimodalCommunicationGame/experiment/analysis/R/")
d = read.csv("../../data/Final_Turn_data.csv", stringsAsFactors = F)


matcherResponds = tapply(d$turnNumber, d$trialString, function(X){
  length(X)>1
})
condition = tapply(d$condition, d$trialString, head,n=1)
modalityCondition = tapply(d$modalityCondition, d$trialString, head,n=1)

dx = data.frame(condition=condition,
                modalityCondition = modalityCondition,
                matcherResponds = matcherResponds)

table(dx$matcherResponds, dx$condition,dx$modalityCondition)
