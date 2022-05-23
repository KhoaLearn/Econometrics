# VaR estimation using bootstrapping
load("D:/Programming Guide/R Guide/code/sp500.RData")
library(rugarch)

set.seed(12345)
spec = ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(garchOrder=c(1,1),model="sGARCH"))
garch11 = ugarchfit(spec,data = sp500$ret)
mu = garch11@fit$coef["mu"]; omega = garch11@fit$coef["omega"]
alpha = garch11@fit$coef["alpha1"]; beta = garch11@fit$coef["beta1"]

h = garch11@fit$var
resid  = (garch11@fit$residuals - mu)
sres = resid/h^0.5

N = length(h)
mcrr = NULL

for (n in 1:1000) {
  h_fc = ret_fc = sp500_fc = NULL
  random = sample(sres,size = 10,replace = T)
  
  h_fc = omega + alpha*resid[N]^2 + beta*h[N]
  ret_fc = mu + sqrt(h_fc[1])*random[1]
  sp500_fc = sp500$sp500[N]*exp(ret_fc)

  for (i  in 1:9){
    h_fc = c(h_fc, omega+(alpha+beta)*h_fc[i])
    ret_fc = c(ret_fc,mu+sqrt(h_fc[i+1])*random[i+1])
    sp500_fc = c(sp500_fc,sp500_fc[i]*exp(ret_fc[i+1]))
  }
  mcrr = rbind(mcrr, log(c(min(sp500_fc),max(sp500_fc))/sp500$sp500[N]))
}

mcrrl = 1 - exp(mean(mcrr[,1]) - 1.645*sd(mcrr[,1]))
mcrrs = exp(mean(mcrr[,2]) + 1.645*sd(mcrr[,2])) - 1

mcrrl
mcrrs


# h_fc = ugarchforecast(garch11, n.ahead = 10)@forecast$sigmaFor
# ret_fc = mu + sqrt(h_fc)*random
# sp500_fc = sp500$sp500[N]*exp(cumsum(ret_fc))