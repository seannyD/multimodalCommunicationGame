try(setwd("~/Documents/MPI/ViniciusMultimodal/multimodalCommunicationGame/experiment/analysis/R/"))
d = read.csv("../../data/FinalSignalData.csv")

T1L = tapply(d[d$turnType=="T1",]$turnLength,
             d[d$turnType=="T1",]$trialString, head, n=1)
d$T1Length = T1L[d$trialString]
d$T1Length[is.na(d$T1Length)] = mean(d$T1Length,na.rm=T)
d$T1Length.log = log(d$T1Length)
d$T1Length.log = d$T1Length.log - mean(d$T1Length.log)

d = d[!duplicated(d$trialString),]

summary = d %>% 
  group_by(condition, modalityCondition,game) %>% 
  summarise(Efficiency=mean(trialLength),
            sd=sd(trialLength),
            ci.w =           qnorm(0.95)*sd/sqrt(length(trialLength)),
            upper=Efficiency+ci.w,
            lower = Efficiency-ci.w)
summary$game = summary$game +1

summary$modalityCondition =
  factor(summary$modalityCondition,
         levels = c("visual",'multi','vocal'),
         labels=c("Gestural","Multimodal","Vocal"))


summaryA = d %>% 
  group_by(condition, modalityCondition,game) %>% 
  summarise(Accuracy=mean(correct),
            sd=sd(correct),
            ci.w =           qnorm(0.95)*sd/sqrt(length(correct)),
            upper=Accuracy+ci.w,
            lower = Accuracy-ci.w)
summaryA$game = summaryA$game +1

summaryA$modalityCondition =
  factor(summaryA$modalityCondition,
         levels = c("visual",'multi','vocal'),
         labels=c("Gestural","Multimodal","Vocal"))

names(summaryA)[5:8] = paste0("Acc.",names(summaryA)[5:8])

summaryAll = cbind(as.data.frame(summary),as.data.frame(summaryA[,4:8]))

summaryAll$color = paste(summaryAll$modalityCondition,summaryAll$game)

gx1 = ggplot(summaryAll[summaryAll$game==4,], aes(x=Efficiency, y=Accuracy, group=game, colour=modalityCondition)) + 
  geom_errorbar(aes(ymin=Acc.lower, ymax=Acc.upper,group=modalityCondition), width=2) +
  geom_errorbarh(aes(xmin=lower, xmax=upper,group=modalityCondition), width=0.5) +
  facet_grid(. ~ condition) +
  #stat_summary(fun.y="mean", geom="line", aes(group=modalityCondition),position=pd) +
  geom_path(aes(group=modalityCondition)) +
  geom_point(aes(group=modalityCondition,shape=modalityCondition)) +
  scale_colour_brewer(palette="Set2", name="Condition") +
  scale_shape(name="Condition") +
  #theme(panel.grid.major.x = element_blank()) +
  xlab("Trial length (ms)") +
  ylab("Accuracy")




summaryP = d %>% 
  group_by(condition, modalityCondition,game,dyadNumber) %>% 
  summarise(Accuracy=mean(correct),
            Efficiency=mean(trialLength),
            Acc.sd=sd(correct),
            Acc.ci.w = qnorm(0.95)*Acc.sd/sqrt(length(correct)),
            sd=sd(trialLength),
            ci.w = qnorm(0.95)*sd/sqrt(length(trialLength)),
            Acc.upper=Accuracy+Acc.ci.w,
            Acc.lower = Accuracy-Acc.ci.w,
            upper = Efficiency+ci.w,
            lower = Efficiency-ci.w)
summaryP$game = summaryP$game +1


ggplot(summaryP[summaryP$game==4,],aes(x=Efficiency, y=Accuracy, group=modalityCondition, colour=modalityCondition)) +
  geom_point() +
  facet_grid(.~condition)


####

gx1 = ggplot(summaryAll, aes(x=Efficiency, y=Accuracy, group=game, colour=color)) + 
  geom_errorbar(aes(ymin=Acc.lower, ymax=Acc.upper,group=modalityCondition), width=0.5) +
  geom_errorbarh(aes(xmin=lower, xmax=upper,group=modalityCondition), width=0.5) +
  facet_grid(. ~ condition) +
  geom_path(aes(group=modalityCondition,colour=color)) +
  geom_point(aes(group=modalityCondition,shape=modalityCondition,colour=color)) +
  scale_colour_manual(values = 
                        c("#bae4b3","#74c476","#31a354","#006d2c",
                          "#fdbe85","#fd8d3c","#e6550d","#a63603",
                          "#cbc9e2","#9e9ac8","#756bb1","#54278f"
                          )) +
  scale_shape(name="Condition") +
  #theme(panel.grid.major.x = element_blank()) +
  xlab("Trial length (ms)") +
  ylab("Accuracy")

gx1
