library(ggplot2)
library(RColorBrewer)
library(gridExtra)
setwd("~/Documents/MPI/ViniciusMultimodal/multimodalCommunicationGame/experiment/analysis/R/")
d = read.csv("../../data/Final_Turn_data.csv",stringsAsFactors = F)

d = d[d$modalityCondition=='multi',]

d$turnModalityType[d$turnModalityType=="unimodal mixed"] = "multi"

transitions = tapply(d$turnModalityType, d$trialString, function(X){
  if(length(X)>1){
    return(paste(X[1:(length(X)-1)],X[2:length(X)],sep=","))
  } else{
    return("None")
  }
})

modalityCondition = tapply(d$modalityCondition, d$trialString, function(X){
  if(length(X)>1){
    return(X[1:(length(X)-1)])
  } else{
    return(X[1])
  }
})

condition = tapply(d$modalityCondition, d$trialString, function(X){
  if(length(X)>1){
    return(X[1:(length(X)-1)])
  } else{
    return(X[1])
  }
})

dx = data.frame(transition=unlist(transitions),
                modalityCondition = unlist(modalityCondition),
                condiiton = unlist(condition),
                stringsAsFactors = F)


dx = dx[dx$transition!="None",]
tx = table(dx$transition)/nrow(dx)

mat = matrix(0,ncol=3,nrow=3)
rownames(mat) = c("unimodal acoustic",'multi','unimodal visual')
colnames(mat) = c("unimodal acoustic",'multi','unimodal visual')

for(i in strsplit(dx$transition,',')){
  mat[i[1],i[2]] = mat[i[1],i[2]] + 1
}

x = rbind(c("","To","",""),
      c("From",colnames(mat)),
      c(rownames(mat)[1],mat[,1]),
      c(rownames(mat)[2],mat[,2]),
      c(rownames(mat)[3],mat[,3]))

write.table(x, '../../results/tables/Transitions.csv', row.names = F, col.names = F, quote=T,sep=',')

t(apply(mat,1,function(X){X/sum(X)}))


