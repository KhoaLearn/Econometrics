# Fama MacBeth two stage procedure
library(readxl)
# Read data and create joint data set
monthlyfactors <- read_excel("D:/Programming Guide/data/monthlyfactors.xlsx")
vw_sizebm_25groups <- read_excel("D:/Programming Guide/data/vw_sizebm_25groups.xlsx") 
vw_sizebm_25groups[,-1] = vw_sizebm_25groups[,-1] - monthlyfactors$rf
data = data.frame(vw_sizebm_25groups,monthlyfactors[,2:7])
data = data[1:363,] # Adjust data set to Gregeory et al. period


# First stage regressions
betas = NULL
for (var in 2:26) {
  lr = lm(data[,var] ~ rmrf + smb + hml + umd, data = data)
  betas = rbind(betas,lr$coefficients[2:5])

}

# Second stage regressions
lambdas = R2 = NULL
for (t in 1:nrow(data)) {
  lr = lm(t(data[t,2:26]) ~ betas)
  lambdas = rbind(lambdas, lr$coefficients)
  R2 = c(R2,summary(lr)$r.squared)
}

# Compute market prices of risk and t-statistics
colMeans(lambdas)*100
nrow(data)^0.5*colMeans(lambdas)/apply(lambdas, 2, sd)