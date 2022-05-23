library(readxl)
capm <- read_excel("D:/Programming Guide/data/capm.xls")

capm$rsandp = c(NA,100*diff(log(capm$SANDP)))
capm$rford = c(NA,100*diff(log(capm$FORD)))
capm$rge = c(NA,100*diff(log(capm$GE)))
capm$rmsoft = c(NA,100*diff(log(capm$MICROSOFT)))
capm$roracle = c(NA,100*diff(log(capm$ORACLE)))

capm$USTB3M = capm$USTB3M/12
capm$ersandp = capm$rsandp - capm$USTB3M
capm$ermsoft = capm$rmsoft - capm$USTB3M
capm$erford = capm$rford - capm$USTB3M

par(cex.axis = 1.5, cex.lab = 1.5, lwd =  2)
plot(capm$Date,capm$ersandp,type='l',col = "red",ylim=c(-100,100),ylab="")
lines(capm$Date,capm$erford, lwd = 1)
legend("topright",c("SP500","Ford"), col = c("red","black"),lty=1,cex = 1.5)

plot(capm$ersandp,capm$erford,pch=19)

lm_capm = lm(erford ~ ersandp, data = capm)
summary(lm_capm)

library(car)
linearHypothesis(lm_capm,c("ersandp=1"))
linearHypothesis(lm_capm,hypothesis.matrix = diag(2),rhs = c(1,1))
linearHypothesis(lm_capm,c("(Intercept)=1","ersandp=1"))

############################## Quantile regression ##############################
load(file = "capm.RData")
library("quantreg")
qreg = rq(erford ~ ersandp, data = capm)
summary(qreg)
qreg = rq(erford ~ ersandp, data = capm, tau=seq(0.1,0.9,0.1))
plot(summary(qreg), level = 0.95)
q10 = rq(erford ~ ersandp, data = capm, tau=0.1)
q20 = rq(erford ~ ersandp, data = capm, tau=0.2)
q30 = rq(erford ~ ersandp, data = capm, tau=0.3)
q40 = rq(erford ~ ersandp, data = capm, tau=0.4)
q50 = rq(erford ~ ersandp, data = capm, tau=0.5)
q60 = rq(erford ~ ersandp, data = capm, tau=0.6)
q70 = rq(erford ~ ersandp, data = capm, tau=0.7)
q80 = rq(erford ~ ersandp, data = capm, tau=0.8)
q90 = rq(erford ~ ersandp, data = capm, tau=0.9)
anova(q10,q20,q30,q40,q50,q60,q70,q80,q90,joint = T)
