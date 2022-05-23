load("D:/Programming Guide/R Guide/code/sp500.RData")
library(fExtremes)
U=-0.025; alpha=0.01;

y = abs(sp500$ret[sp500$ret<U])
hill = hill(y)
var = sort(-y)*(nrow(sp500)*alpha/length(y))^(-hill$hill)

# Hill plot of Value-at-Risk
par(lwd=2,cex.axis = 2, cex.lab =1.5)
plot(var,pch="+",xlab="Order Statistic",ylab="VaR")
title("Hill Plot")