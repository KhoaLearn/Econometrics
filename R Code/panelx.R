library(readxl)
panelx <- read_excel("D:/Programming Guide/data/panelx.xls",col_types = c("numeric","numeric","numeric","numeric"),na = 'NA')
library(plm)
data = pdata.frame(panelx, index=c("firm_ident", "year"))
pdim(data)

pooled = plm(return~beta, model="pooling", data=data)
summary(pooled)

fixed = plm(return~beta, model="within", data=data)
summary(fixed)

random = plm(return~beta, model="random", data=data)
summary(random)

phtest(fixed, random)