rm(list=ls())
library(mice)
library(leaps)
source("MissingDataAnalysis.R")

##############
#Exploratory missing data
##############
md.pattern(lbw_data)
p <- md.pairs(lbw_data)
p

##############
#Imputation
#############
ini <- mice(lbw_data, maxit = 0)
pred <- ini$predictorMatrix
pred[, c("iqfull","iqverb","iqperf","rcomp","rrate","racc","tomifull","code")] <- 0
#Specify that these variables should not be used to impute other values
imp <- mice(lbw_data, seed = 17, method=c("","","","","","","","pmm","pmm","pmm","logreg",
                                          "logreg","logreg","pmm","pmm","polr","polr",""),
            predictorMatrix=pred, visitSequence = "monotone", m=50)
#Data are imputed by sorting in increasing amount of missingness
#Specify that response variables should not be imputed.
#Treat benef as a continuous variable (not ordered, because it is the number of aids)
#Socstat and mcig are treated as ordered variables
print(imp)
#Check quality of imputation
stripplot(imp, educage+fed+benef+mcig+socstat~.imp, pch = 20, cex = 1.2)
#Check if algoirthm converged
plot(imp, c("educage", "fed", "benef","mcig","socstat"))

##################
#Linear regression
##################
fit <- with(imp, lm(iqfull ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat))
summary(pool(fit))
