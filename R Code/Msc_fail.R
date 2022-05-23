library(readxl)
MSc_fail <- read_excel("D:/Programming Guide/data/MSc_fail.xls")
names(MSc_fail)[4] = "WorkExperience"
names(MSc_fail)[6] = "CountryCode"
names(MSc_fail)[7] = "PGDegree"

linear = lm(Fail ~ Age + English + Female + WorkExperience + Agrade + BelowBGrade + PGDegree + Year2004 + Year2005 + Year2006 + Year2007, data = MSc_fail)

logit = glm(formula(linear),data = MSc_fail, family = binomial("logit"))
summary(logit)

probit = glm(formula(linear),data = MSc_fail, family = binomial("probit"))
summary(probit)

par(cex.axis=1.5)
plot(probit$fitted.values,type="l",xlab="",ylab="")

library("margins")
margins(logit)
margins(probit)
