# Script to generate critical values for Dickey Fuller test
set.seed(123456)
tnone = tconst = ttrend = NULL

for (i in 1:500){
  y = c(0, cumsum(rnorm(1199)))
  dy = c(NA,diff(y))
  lagy = c(NA,y[1:1199])
  # Model with no intercept
  lmnone = lm(dy[-(1:200)] ~ 0 + lagy[-(1:200)])
  tnone = c(tnone, coef(summary(lmnone))[1]/coef(summary(lmnone))[2])
  # Model with intercept, no trend  
  lmconst = lm(dy[-(1:200)] ~ 1 + lagy[-(1:200)])
  tconst = c(tconst,coef(summary(lmconst))[2,1]/coef(summary(lmconst))[2,2])
  # Model with intercept and trend
  lmtrend = lm(dy[-(1:200)] ~ 1 + lagy[-(1:200)] + c(1:1000))
  ttrend = c(ttrend,coef(summary(lmtrend))[2,1]/coef(summary(lmtrend))[2,2])
}
quantile(tnone,c(0.01,0.05,0.1))
quantile(tconst,c(0.01,0.05,0.1))
quantile(ttrend,c(0.01,0.05,0.1))