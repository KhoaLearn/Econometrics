set.seed(123456)
tnone = tconst = ttrend = NULL

for (i in 1:50000){
  y = c(0, cumsum(rnorm(1199)))
  dy = c(NA,diff(y))
  
  lmnone = lm(dy[-(1:200)] ~ 0 + y[200:1199])
  tnone = c(tnone, coef(summary(lmnone))[1]/coef(summary(lmnone))[2])
  
  lmconst = lm(dy[-(1:200)] ~ 1 + y[200:1199])
  tconst = c(tconst,coef(summary(lmconst))[2,1]/coef(summary(lmconst))[2,2])
  
  lmtrend = lm(dy[-(1:200)] ~ 1 + y[200:1199] + c(1:1000))
  ttrend = c(ttrend,coef(summary(lmtrend))[2,1]/coef(summary(lmtrend))[2,2])
}
quantile(tnone,c(0.01,0.05,0.1))
quantile(tconst,c(0.01,0.05,0.1))
quantile(ttrend,c(0.01,0.05,0.1))