library(gplots)

setwd("~/Documents/MPI/ViniciusMultimodal/experiment/analysis/")


d = read.csv("../data/csv/Cod - Ex5.csv", stringsAsFactors = F)


plotmeans( signalLength ~ signalType, data = d)


t.test( d[d$signalType=="C",]$signalLength, d[d$signalType=="S",]$signalLength)
