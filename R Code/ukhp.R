library(readxl)
UKHP = read_excel("D:/Programming Guide/data/UKHP.xls", col_types = c("date", "numeric"))
names(UKHP)[2] = "hp"
UKHP$dhp = c(NA, 100*diff(UKHP$hp)/UKHP$hp[1:nrow(UKHP)-1])

par(cex.axis = 1.5, cex.lab = 1.5, lwd = 2)
plot(UKHP$Month,UKHP$hp,type = 'l',xlab="Date",ylab="House price")
hist(UKHP$dhp)
box()


############################## ARMA models #######################

UKHP = UKHP[-1,]

ac = acf(UKHP$dhp,lag.max = 12)
pac = pacf(UKHP$dhp,lag.max = 12)

ar11 = arima(UKHP$dhp,order = c(1,0,1))

AIC(ar11)
AIC(ar11, k = log(nrow(UKHP)))

aic_table = array(NA,c(6,6,2))
for (ar in 0:5) {
  for (ma in 0:5) {
    arma = arima(UKHP$dhp,order = c(ar,0,ma))
    aic_table[ar+1,ma+1,1] = AIC(arma)
    aic_table[ar+1,ma+1,2] = AIC(arma, k = log(nrow(UKHP)))
  }
}

ar2 = arima(UKHP$dhp[UKHP$Month <="2015-12-01"], order = c(2,0,0))
dynamic_fc = predict(ar2,n.ahead = 27)
static_fc = ar2$coef[3]+ar2$coef[1]*UKHP$dhp[299:325]+ar2$coef[2]*UKHP$dhp[298:324]

par(lwd=2,cex.axis = 2)
plot(UKHP$Month[300:326],UKHP$dhp[300:326],type = "l",xlab = "",ylab = "")
lines(UKHP$Month[300:326],dynamic_fc$mean,col="blue")
lines(UKHP$Month[300:326],static_fc,col="red")
legend("topright", legend=c("Actual", "Dynamic", "Static"),col=c("black","blue","red"),lty= 1)

library(smooth)
smooth_fc = es(data = UKHP$dhp[1:299],h = 27)

############################## Unit Root Test #######################

library(fUnitRoots)

adfTest(UKHP$hp,lags = 10,type = "c")

adfTest(UKHP$dhp,lags = 10,type = "c")

adfgls = urersTest(UKHP$hp, type = "DF-GLS", model = "trend", lag.max = 10)
adfgls@test$test@teststat
adfgls@test$test@cval

############################## Markov Switching Models #######################

library(MSwM)
msmodel = msmFit(lm(UKHP$dhp~1),k=2,sw=c(T,T))
summary(msmodel)
par(lwd=2,cex.axis = 2)
plot(UKHP$Month,msmodel@Fit@filtProb[,2],type="l",xlab="",ylab="")









