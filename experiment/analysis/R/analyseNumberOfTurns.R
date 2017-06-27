library(sjPlot)
d = read.csv("../../data/FinalSignalData.csv")


numTurns = tapply(d$turnString, d$trialString, 
                  function(X){length(unique(X))})
d$numberOfTurns = numTurns[d$trialString]

d = d[!duplicated(d$trialString),]
d$incorrect = !d$correct

# Make a variable that represents the number of trials played
d$trialTotal = d$trial + (d$game * (max(d$trial)+1))
# Convert to proportion of games played, so that estimates reflect change per game.
d$trialTotal = d$trialTotal / 16
# Center the trialTotal variable so intercept reflects after the first game
d$trialTotal = d$trialTotal - 2


turnD = read.csv("../../data/Final_Turn_data.csv")
turnD = turnD[turnD$turnType=="T1",]
turnD = turnD[turnD$role == "Director",]
d$multimodal = turnD[match(d$trialString, turnD$trialString),]$turnModalityType == "multi"
d$multimodal[is.na(d$multimodal)] = F



rowSums(table(turnD[turnD$modalityCondition=='multi',]$turnModalityType, turnD[turnD$modalityCondition=='multi',]$trialString) > 0)


rowSums(table(turnD[turnD$modalityCondition=='multi' & turnD$condition=='Visual',]$turnModalityType, turnD[turnD$modalityCondition=='multi' & turnD$condition=='Visual',]$trialString)>0)

rowSums(table(turnD[turnD$modalityCondition=='multi' & turnD$condition=='Auditory',]$turnModalityType, turnD[turnD$modalityCondition=='multi' & turnD$condition=='Auditory',]$trialString)>0)



coXmoXin = lmer(I(numberOfTurns-1) ~ 1 + modalityCondition*condition*trialTotal+
                  (1 |dyadNumber/playerId) + 
                  (1 |itemId),
                data=d)
summary(coXmoXin)

sjp.lmer(coXmoXin, 'fe',p.kr = FALSE, show.intercept = T)




interact = lmer(I(numberOfTurns-1) ~ 1 + modalityCondition*condition*trialTotal+
                  (1 |dyadNumber/playerId) + 
                  (1 |itemId),
                data=d)
sjp.glmer(interact, 'fe', show.intercept = T, fade.ns = T)




x = table( d$numberOfTurns, d$modalityCondition, d$condition)
round(t(t(x[,,1])/colSums(x[,,1])), 3)*100
round(t(t(x[,,2])/colSums(x[,,2])), 3)*100

xyplot(numberOfTurns ~ game, groups = modalityCondition, data=d[d$condition=="Visual",], type='a', ylim=c(0,2), auto.key = T)
xyplot(numberOfTurns ~ game, groups = modalityCondition, data=d[d$condition=="Auditory",], type='a', ylim=c(0,2), auto.key = T)


d[d$modality=='Acoustic' & d$modalityCondition=="visual",]
d[d$modality=='Acoustic' & d$modalityCondition=="visual",]
