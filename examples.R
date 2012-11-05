# Plot examples

zzz<-effect_prof_mfx(mymod,500)

library(ggplot2)
library(eeptools)

summary(zzz$z.ratio)

ggplot(zzz,aes(var, marginal.effect,ymin = me - 2*se,ymax= me + 2*se)) +
  scale_x_discrete('Variable') +
  scale_y_continuous('Marginal Effect',limits=c(-0.5,1)) +
  theme_dpi() + 
  geom_errorbar(aes(x = var, y = me),size=.3,width=.2) + 
  geom_point(aes(x = var, y = me)) +
  geom_hline(yintercept=0) + 
  coord_flip() +
  labs(title="Marginal Effects with 95% Confidence Intervals")


ggplot(zzz,aes(var, me*mean,ymin = me*(mean-sd),ymax= me*(mean+sd))) +
  scale_x_discrete('Variable') +
  scale_y_continuous('Marginal Effect',limits=c(-0.5,1)) +
  theme_dpi() + 
  geom_errorbar(aes(x = var, y = me*mean),size=.3,width=.2) + 
  geom_point(aes(x = var, y = me*mean)) +
  geom_hline(yintercept=0) + 
  coord_flip() +
  labs(title="Effect Across 1SD change in Var")

sim<-data.frame(x=seq(zzz[zzz$var=="xtest",]$lower,zzz[zzz$var=="xtest",]$upper,
                      length.out=200))

sim$est<-sim$x*zzz[zzz$var=="xtest",]$me

sim$est2<-sim$est*mean(dnorm(predict(mymod,type="link")))




library(effects)

t<-glm(ytest~I(xtest^2)+x4test*x3test+log(ztest)+ztest,data=mydat,family='binomial')

plot(allEffects(t), ask = FALSE, rescale.axis = FALSE)


zzz<-effect_prof(mymod,500)

qplot(var,est,data=zzz,geom='boxplot')

names(zzz)