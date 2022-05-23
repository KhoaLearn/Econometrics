library(readxl)

SandPhedge <- read_excel("D:/Programming Guide/data/SandPhedge.xls", 
                         col_types = c("date", "numeric", "numeric"))


SandPhedge=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQxQrLZ4BIKJdiDJPfz4u3_NP-W-Vj4MvJdMHwJ1GC2arhdbZLBMqvTaOrKSx5W5Q/pub?gid=1375445279&single=true&output=csv")

SandPhedge$rspot = c(NA,100*diff(log(SandPhedge$Spot)))
SandPhedge$rfutures = c(NA,100*diff(log(SandPhedge$Futures))) 

summary(SandPhedge[c("rspot","rfutures")])
lm_returns = lm(rspot ~ rfutures,data = SandPhedge)
summary(lm_returns)
lm_prices = lm(Spot ~ Futures,data = SandPhedge)
summary(lm_prices)

library(car)
linearHypothesis(lm_returns,c("rfutures=1"))
linearHypothesis(lm_prices,c("Futures=1"))

SandPhedge$lspot = log(SandPhedge$Spot)
SandPhedge$lfutures = log(SandPhedge$Futures)

log_lm = lm(lspot ~ lfutures, data = SandPhedge)

par(lwd=2,cex.axis = 2)
plot(SandPhedge$Date,SandPhedge$lspot,type = "l",xlab = "",ylab = "",col="red")
lines(SandPhedge$Date,log_lm$fitted.values)
par(new=T)
plot(SandPhedge$Date,log_lm$residuals,col="blue",axes=F,type="l",xlab = "",ylab = "")
axis(side=4, at = pretty(range(log_lm$residuals)))
legend("bottomleft", legend=c("Actual", "Fitted"),col=c("black","red"),lty= 1)
legend("bottomright", legend=c("Resid"),col=c("blue"),lty= 1)

library(fUnitRoots)
urersTest(log_lm$residuals, type = "DF-GLS", model = "trend", lag.max = 12)@test$test@teststat
urersTest(log_lm$residuals, type = "DF-GLS", model = "trend", lag.max = 12)@test$test@cval

summary(lm(SandPhedge$rspot[-1] ~ SandPhedge$rfutures[-1] + log_lm$residuals[-247]))



