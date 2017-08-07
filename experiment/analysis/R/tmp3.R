

matcherResponds.cumulative.range = 
  seq(range(d$matcherResponds.cumulative)[1],
      range(d$matcherResponds.cumulative)[2],length.out=20)

dx = data.frame(
  modalityCondition = "multi",
  condition = "Visual",
  trialTotal  =0,
  matcherResponds = T,
  matcherResponds.cumulative = matcherResponds.cumulative.range,
  incorrect = T,
  multimodal= F,
  firstBlock= "Visual"
)

p = predict(block, newdata=dx, re.form=NA)
exp(p+meanLogTrialLength)


x = sjp.lmer(block, 'pred',vars = c("matcherResponds.cumulative"), show.ci = T, show.scatter = F)

x$data$y = exp(x$data$y + meanLogTrialLength)
x$data$ci.low = exp(x$data$ci.low + meanLogTrialLength)
x$data$ci.high = exp(x$data$ci.high + meanLogTrialLength)

ggplot(x$data, aes(x=x,y=y)) + 
  geom_smooth(method=lm, colour='black') +
  xllab
