library(xlsx)
setwd("~/Documents/MPI/ViniciusMultimodal/multimodalCommunicationGame/experiment/analysis/")


d = read.xlsx("../data/13428_2012_267_MOESM1_ESM.xls",1)
d2  = read.xlsx("../data/13428_2010_38_MOESM1_ESM.xls",1)

words = c("rock","fruit","predator","water","tree","hole",'apple','banana','bear','emotion','anger')

dx = d[d$Noun %in% words,]

dx = dx[,c("Noun",'Visual_mean','Auditory_mean')]


d2a = d2[,grepl("1",names(d2))]
d2b = d2[,grepl("2",names(d2))]
names(d2b) = names(d2a)
d2 = rbind(d2a,d2b)

d3 = d2[d2$Concept1 %in% words,]
d3 = d3[!duplicated(d3$Concept1),c("Concept1","MeanVis1", "MeanAud1")]
names(d3) = c("Noun","Visual_mean","Auditory_mean")


write.csv(rbind(dx,d3),"../results/tables/ModalityNormComparison.csv")


sum(d3[d3$Noun=='tree',]$Visual_mean <= d2$MeanVis1) / nrow(d2)
sum(d3[d3$Noun=='tree',]$Auditory_mean>= d2$MeanAud1) / nrow(d2)
