load("sp500.RData")
library(extremefit)
U=-0.025
y = -sp500$ret[sp500$ret<U]
hill = hill(y)
alpha=0.01
var = -sort(y,decreasing = T)*(nrow(sp500)*alpha/length(y))^(-hill$hill)
# Hill plot of Value-at-Risk
par(lwd=2,cex.axis = 2, cex.lab =1.5)
plot(var,pch="+",xlab="Order Statistic",ylab="VaR")
title("Hill Plot")

