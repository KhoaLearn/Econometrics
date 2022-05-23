library(readxl)
fred <- read_excel("D:/Programming Guide/data/fred.xls")

pca = prcomp(fred[c("GS3M","GS6M","GS1","GS3","GS5","GS10")],scale. = T,retx = T)
summary(pca)

library(urca)
summary(ur.df(fred$GS3M, selectlags = "BIC",type = "drift"))
summary(ur.df(fred$GS6M, selectlags = "BIC",type = "drift"))
summary(ur.df(fred$GS1, selectlags = "BIC",type = "drift"))
summary(ur.df(fred$GS3, selectlags = "BIC",type = "drift"))
summary(ur.df(fred$GS5, selectlags = "BIC",type = "drift"))
summary(ur.df(fred$GS10, selectlags = "BIC",type = "drift"))

par(lwd=2,cex.axis = 2)
plot(fred$Date,fred$GS3M,type="l",xlab="",ylab="")
lines(fred$Date,fred$GS6M,col="red")
lines(fred$Date,fred$GS1,col="blue")
lines(fred$Date,fred$GS3,col="brown")
lines(fred$Date,fred$GS5,col="orange")
lines(fred$Date,fred$GS10,col="darkgreen")

library(vars)
VARselect(fred[c("GS3M","GS6M","GS1","GS3","GS5","GS10")],lag.max = 12)

summary(ca.jo(fred[c("GS3M","GS6M","GS1","GS3","GS5","GS10")],K=2,ecdet = "const",type = "trace"))
vecm = ca.jo(fred[c("GS3M","GS6M","GS1","GS3","GS5","GS10")],K=2,ecdet = "const",type = "trace")
a = cajorls(vecm,r=3)
H = matrix(c(0,0,1,0,0,0,0), c(7,1))
A = matrix(c(0,0,1,0,0,0,0),c(6,1))

summary(blrtest(vecm,H=H,r=1))

############################## Panel Unit Root Test #######################

Y = cbind(fred$GS3M,fred$GS6M,fred$GS1,fred$GS3,fred$GS5,fred$GS10)
purtest(Y, test = "levinlin", exo = "intercept", lags = "SIC")
purtest(Y, test = "madwu", exo = "intercept", lags = "SIC", pmax = 5)
purtest(Y, test = "ips", exo = "intercept", lags = "SIC", pmax = 5)




