library(readxl)
sp500 <- read_excel("D:/Programming Guide/data/sp500.xlsx")

sp500$ret = c(NA,diff(log(sp500$sp500)))
sp500 = sp500[-1,]

quantile(sp500$ret,c(0.01,0.05,0.1))
qnorm(c(0.01,0.05,0.1),mean = mean(sp500$ret), sd = sd(sp500$ret))
