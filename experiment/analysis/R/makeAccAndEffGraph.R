# Combine accuracy and efficiency graphs

## Load libraries

library(lme4)
library(sjPlot)
library(ggplot2)
library(lattice)
library(influence.ME)
library(party)
library(dplyr)
library(gridExtra)


try(setwd("~/Documents/MPI/ViniciusMultimodal/multimodalCommunicationGame/experiment/analysis/R/"))

## Load data

d = read.csv("../../data/FinalSignalData.csv")


d = d[!duplicated(d$trialString),]

summary = d %>% 
group_by(condition, modalityCondition,game) %>% 
summarise(Accuracy=mean(correct),
sd=sd(correct),
ci.w =           qnorm(0.95)*sd/sqrt(length(correct)),
upper=Accuracy+ci.w,
lower = Accuracy-ci.w)
summary$game = summary$game +1

summary$modalityCondition =
factor(summary$modalityCondition,
levels = c("visual",'multi','vocal'),
labels=c("Gestural","Multimodal","Vocal"))

#ggplot(d, aes(x=trialTotal, y=as.numeric(correct),colour=modalityCondition)) +
#  geom_smooth() + facet_grid(.~condition)

#ggplot(d, aes(x=trialTotal, y=as.numeric(correct),colour=condition)) +
#  geom_smooth() + #facet_grid(.~modalityCondition)

ggplot(summary, aes(x=game, y=Accuracy, group=condition, colour=modalityCondition)) + 
geom_point() +
geom_errorbar(aes(ymin=lower, ymax=upper)) +
facet_grid(. ~ condition) +
stat_summary(fun.y="mean", geom="line", aes(group=modalityCondition))

gx = ggplot(summary, aes(x=game, y=Accuracy, group=condition, colour=modalityCondition)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper,group=modalityCondition), width=0.5,position = pd) +
  stat_summary(fun.y="mean", geom="line", aes(group=modalityCondition),position = pd) +
  geom_point(aes(group=modalityCondition,shape=modalityCondition),position=pd) +
  scale_colour_brewer(palette="Set2", name="Condition") +
  scale_shape(name="Condition") +
  ggtitle("Accuracy") +
  theme(panel.grid.major.x = element_blank(),
        legend.position = 'none') +
  facet_grid(. ~ condition) +
  xlab("Game")


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


summary$Efficiency = summary$Efficiency/1000
summary$upper = summary$upper/1000
summary$lower = summary$lower/1000


pd = position_dodge(width=0.5)
gx1 = ggplot(summary, aes(x=game, y=Efficiency, group=condition, colour=modalityCondition)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper,group=modalityCondition), width=0.5,position=pd) +
  facet_grid(. ~ condition) +
  stat_summary(fun.y="mean", geom="line", aes(group=modalityCondition),position=pd) +
  geom_point(aes(group=modalityCondition,shape=modalityCondition),position=pd) +
  scale_colour_brewer(palette="Set2", name="Condition") +
  scale_shape(name="Condition") +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle("Efficiency") +
  xlab("Game") +
  ylab("Trial length (seconds)")


pdf("../../results/graphs/Accuracy_and_Efficiency.pdf",
    width=7,height=3)
grid.arrange(gx,gx1,nrow=1,widths=c(0.75,1))
dev.off()


