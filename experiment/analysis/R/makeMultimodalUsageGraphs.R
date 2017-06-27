library(ggplot2)
library(RColorBrewer)
library(gridExtra)
setwd("~/Documents/MPI/ViniciusMultimodal/multimodalCommunicationGame/experiment/analysis/R/")
d = read.csv("../../data/Final_Turn_data.csv",stringsAsFactors = F)

d_director = d[d$role=="Director" & d$turnNumber==1 & d$modalityCondition=="multi",]

tx.pre = table(d_director$turnModalityType,d_director$condition)

tx.pre[,1]/sum(tx.pre[,1])
tx.pre[,2]/sum(tx.pre[,2])

d_director = d_director[d_director$turnModalityType!='unimodal mixed',]

d_matcher = d[d$role=="Matcher",]
d_matcher = d_matcher[d_matcher$turnModalityType!='unimodal mixed',]

d_director_all = d[d$role=="Director" & d$modalityCondition=="multi" & d$turnModalityType!='unimodal mixed',]

plotMMDist = function(d, filename){
  
  tx = table(d$turnModalityType,d$condition)
  
  tx = tx[c(2,1,3),]
  dx = data.frame(count=c(tx[,1],tx[,2]), 
                  turnModalityType = rep(rownames(tx,2)),
                  condition = rep(colnames(tx),each=3))
  
  dx$turnModalityType = relevel(factor(dx$turnModalityType),'unimodal acoustic')
  dx$turnModalityType = factor(dx$turnModalityType, 
                               levels = c("unimodal acoustic","multi",'unimodal visual'),
                               labels = c("Vocal only",'Multimodal','Gesture only'))
  dx$condition = factor(dx$condition, labels = c("Auditory stimuli",'Visual stimuli'))
  cols = 1:3
  
 # barplot(tx, beside=T,
 #         col=cols, border=NA,names.arg = c("Auditory Stimuli", "Visual Stimuli"))
 # abline(h=0)
  
  pdf(filename, width=4.5, height=4)
  g = ggplot(data=dx, aes(x=condition,y=count,fill=turnModalityType)) +
    geom_bar(position="dodge",stat="identity") + 
    theme(legend.position="top", panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y = element_text(angle=0, vjust = 0.5))  + 
    labs(fill="") + ylab("Count") + xlab("") + geom_vline(xintercept = 1.5, color='white')
  print(g)
  dev.off()
  return(dx)
}

########




plotMMDist(d_director,"../../results/graphs/PropModality/Director_T1_TurnTypes.pdf")
dx_director = plotMMDist(d_director_all,"../../results/graphs/PropModality/Director_all_TurnTypes.pdf")

dx_matcher = plotMMDist(d_matcher,"../../results/graphs/PropModality/Matcher_TurnTypes.pdf")

pdf("../../results/graphs/PropModality/DirectorAndMatcher_TurnTypes.pdf", width=8, height=4)
g1 = ggplot(data=dx_director, aes(x=condition,y=count,fill=turnModalityType)) +
  geom_bar(position="dodge",stat="identity") + 
  theme(legend.position="left", panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(angle=0, vjust = 0.5),
        plot.title = element_text(hjust=0.5),)  + 
  ylim(0,320) +
  labs(fill="") + ylab("Count") + xlab("") + geom_vline(xintercept = 1.5, color='white') + 
  ggtitle("Directors")
g2 = ggplot(data=dx_matcher, aes(x=condition,y=count,fill=turnModalityType)) +
  geom_bar(position="dodge",stat="identity") + 
  theme(legend.position="none", panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(angle=0, vjust = 0.5),
        plot.title = element_text(hjust=0.5))  + 
  ylim(0,320) +
  labs(fill="") + ylab("") + xlab("") + geom_vline(xintercept = 1.5, color='white') + 
  ggtitle("Matchers")

grid.arrange(g1, g2, ncol=2, widths=c(1,0.6))
dev.off()



