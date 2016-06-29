library(gplots)

setwd("~/Documents/MPI/ViniciusMultimodal/multimodalCommunicationGame/experiment/analysis/R/")
d = read.csv("../../data/csv/D19_TT_Coded.csv")

d$trialString = paste(d$condition, d$game,d$trial)

getTimes = function(d,prod,stim){
  ret = tapply(d[d$modality==prod & d$condition==stim,]$signalLength,d[d$modality==prod & d$condition==stim,]$game,sum)[c("0",'1','2','3')]
  ret[is.na(ret)] = 0
  return(ret)
}

aa = getTimes(d,"Acoustic" ,"Auditory")
va = getTimes(d,"Visual","Auditory")
av = getTimes(d,"Acoustic","Visual")
vv = getTimes(d,"Visual","Visual")


barplot(cbind(aa,va,av,vv),beside=T)



