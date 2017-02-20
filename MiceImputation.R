rm(list=ls())
library(mice)
library(leaps)
library(glmnet)
library(plyr)
source("MissingDataAnalysis.R")
source("utilityFunctions.R")

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
m <- 50
imp <- mice(lbw_data, seed = 17, method=c("","","","","","","","pmm","pmm","pmm","logreg",
                                          "logreg","logreg","pmm","pmm","polr","polr",""),
            predictorMatrix=pred, visitSequence = "monotone", m=m)
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
#Compute R^2
coef <- pool(fit)$qbar
X <- model.matrix(iqfull ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat, data = lbw_data)
Y <- lbw_data[complete.cases(lbw_data),"iqfull"] #remove all rows where there are missing data. Otherwise we can not compute R squared
computeRsquared(coef,X,Y)
# $R2
# [1] 0.353818
# 
# $adj_R2
# [1] 0.2216444

##################
#Subset selection
##################

all_varnames <- vector()
for(i in 1:m) {
  dataset <- complete(imp, action=i) #Get ith dataset
  fit_subset = regsubsets(iqfull ~ . -iqverb -iqperf -rcomp -rrate -racc -tomifull -bw -code,
                          data = dataset, method = "exhaustive", nvmax = 20)
  fit_subset_summary = summary(fit_subset)
  best_model_index <- which.min(fit_subset_summary$cp) #Select model that minimise Cp
  best_model_outmat <- fit_subset_summary$outmat[best_model_index,]
  all_varnames <- c(all_varnames, names(best_model_outmat[best_model_outmat=="*"]))
}
#Summary of selected variables
table(all_varnames) 
# all_varnames
# benef2   benef3   benef4 educage2     fed2       ga    mcig2      rbw     sex2 socstat2 socstat3 socstat4 socstat5 
# 46       49       28       33        2       10        2       50       24       30        2       37       33 
#Note that mice rename some factor names (check  ciao <- data.matrix(complete(imp)) to see)

#Fit again a linear model using only benef, educage, rbw, socstat (>50%)
fit_sub <- with(imp, lm(iqfull ~ rbw+educage+benef+socstat))
summary(pool(fit_sub))
coef2 <- pool(fit_sub)$qbar
X <- model.matrix(iqfull ~ rbw+educage+benef+socstat, data = lbw_data)
Y <- lbw_data[!is.na(rbw) & !is.na(educage) & !is.na(benef) & !is.na(socstat),"iqfull"] 
computeRsquared(coef2,X,Y)
# $R2
# [1] 0.3152292
# 
# $adj_R2
# [1] 0.251232

################
#Compare different sizes of imputation
################
fitted_lm <- vector("list")
counter <- 1
for (m in c(2,5,10,50,100)) {
  imputation <- mice(lbw_data, seed = 17, method=c("","","","","","","","pmm","pmm","pmm","logreg",
                                                   "logreg","logreg","pmm","pmm","polr","polr",""),
                     predictorMatrix=pred, visitSequence = "monotone", m=m)
  fitted_lm[[counter]] <- with(imputation, lm(iqfull ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat))
  counter <- counter+1
}

summary(fitted_lm[[1]])
summary(pool(fitted_lm[[5]]))

#Compute R^2 for each model and compare
R2 <- numeric(length(fitted_lm))
adj_R2 <- numeric(length(fitted_lm))
X <- model.matrix(iqfull ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat, data = lbw_data)
Y <- lbw_data[complete.cases(lbw_data),"iqfull"] #remove all rows where there are missing data. Otherwise we can not compute R squared
for (i in 1:length(fitted_lm)) {
  pool_fit <- pool(fitted_lm[[i]])
  coef <- pool_fit$qbar
  list_R2 <- computeRsquared(coef,X,Y)
  R2[i] <- list_R2[[1]]
  adj_R2[i] <- list_R2[[2]]
}
print(R2)
print(adj_R2)

################################################################
#Imputation and subset selection for iqverb, iqperf, tomifull
###############################################################

########
#iqverb
########
m<-50
rm(all_varnames, fit_sub)
all_varnames <- vector()
for(i in 1:m) {
  dataset <- complete(imp, action=i) #Get ith dataset
  fit_subset = regsubsets(iqverb ~ . -iqfull -iqperf -rcomp -rrate -racc -tomifull -bw -code,
                          data = dataset, method = "exhaustive", nvmax = 20)
  fit_subset_summary = summary(fit_subset)
  best_model_index <- which.min(fit_subset_summary$cp) #Select model that minimise Cp
  best_model_outmat <- fit_subset_summary$outmat[best_model_index,]
  all_varnames <- c(all_varnames, names(best_model_outmat[best_model_outmat=="*"]))
}
#Summary of selected variables
table(all_varnames) 
# benef2   benef3   benef4 educage2     fed2   matage    mcig2    mcig3    mcig4      rbw     sex2 socstat2 socstat3 
# 42       50       37       40        5        1       33       17       13       50       44       49        3 
# socstat4 socstat5 
# 48       48 

#Fit again a linear model using only benef, educage, mcig, rbw, socstat, sex
fit_sub <- with(imp, lm(iqverb ~ rbw+educage+benef+socstat+mcig+sex))
summary(pool(fit_sub))
coef2 <- pool(fit_sub)$qbar
X <- model.matrix(iqverb ~ rbw+educage+benef+socstat+mcig+sex, data = lbw_data)
Y <- lbw_data[!is.na(rbw) & !is.na(educage) & !is.na(benef) & !is.na(socstat) & !is.na(mcig) & !is.na(sex),"iqverb"] 
computeRsquared(coef2,X,Y) 
# $R2
# [1] 0.4251156
# 
# $adj_R2
# [1] 0.3446317

############
#iqperf
############
m<-50
rm(all_varnames, fit_sub)
all_varnames <- vector()
for(i in 1:m) {
  dataset <- complete(imp, action=i) #Get ith dataset
  fit_subset = regsubsets(iqperf ~ . -iqfull -iqverb -rcomp -rrate -racc -tomifull -bw -code,
                          data = dataset, method = "exhaustive", nvmax = 20)
  fit_subset_summary = summary(fit_subset)
  best_model_index <- which.min(fit_subset_summary$cp) #Select model that minimise Cp
  best_model_outmat <- fit_subset_summary$outmat[best_model_index,]
  all_varnames <- c(all_varnames, names(best_model_outmat[best_model_outmat=="*"]))
}
#Summary of selected variables
table(all_varnames) 
# benef2   benef3   benef4 educage2       ga      rbw socstat4 socstat5 
# 40       35       12       18        5       50       26       15  

#Fit again a linear model using only benef, rbw, socstat
fit_sub <- with(imp, lm(iqperf ~ rbw+benef+socstat))
summary(pool(fit_sub))
coef2 <- pool(fit_sub)$qbar
X <- model.matrix(iqperf ~ rbw+benef+socstat, data = lbw_data)
Y <- lbw_data[!is.na(rbw) & !is.na(benef) & !is.na(socstat),"iqperf"] 
computeRsquared(coef2,X,Y) 
# $R2
# [1] 0.1493153
# 
# $adj_R2
# [1] 0.08095673

#############
#tomifull
############
m<-50
rm(all_varnames, fit_sub)
all_varnames <- vector()
for(i in 1:m) {
  dataset <- complete(imp, action=i) #Get ith dataset
  fit_subset = regsubsets(tomifull ~ . -iqfull -iqverb -rcomp -rrate -racc -iqperf -bw -code,
                          data = dataset, method = "exhaustive", nvmax = 20)
  fit_subset_summary = summary(fit_subset)
  best_model_index <- which.min(fit_subset_summary$cp) #Select model that minimise Cp
  best_model_outmat <- fit_subset_summary$outmat[best_model_index,]
  all_varnames <- c(all_varnames, names(best_model_outmat[best_model_outmat=="*"]))
}
#Summary of selected variables
table(all_varnames) 
# benef2   benef3   benef4 educage2     fed2       ga      rbw     sex2 socstat4 
# 6        6       23       18        9       26       26       33        2 
#Fit again a linear model using only ga,rbw, sex
fit_sub <- with(imp, lm(tomifull ~ rbw+ga+sex))
summary(pool(fit_sub))
coef2 <- pool(fit_sub)$qbar
X <- model.matrix(tomifull ~ rbw+ga+sex, data = lbw_data)
Y <- lbw_data[!is.na(rbw) & !is.na(ga) & !is.na(sex) & !is.na(tomifull),"tomifull"] 
computeRsquared(coef2,X,Y) 

# $R2
# [1] 0.04972638
# 
# $adj_R2
# [1] 0.0272347

#########
#COMMENTS:
#All this subset selection seems a little bit sketchy. Moreover, it is a bad habit (see http://www.stata.com/support/faqs/statistics/stepwise-regression-problems/)
#Lasso have been proved to perform better
#########

############
#LASSO
############
rm(list=setdiff(ls(), c("imp")))

formula_exp <- "iqfull ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat-1"
var_exp <- "iqfull"

m <- 50
set.seed(17)
all_varnames <- vector()
for(i in 1:m) {
  dataset <- as.data.frame(data.matrix(complete(imp, action=i)))
  #Convert to factors
  dataset$sex = factor(dataset$sex)
  dataset$fed = factor(dataset$fed)
  dataset$educage = factor(dataset$educage)
  dataset$benef = factor(dataset$benef)
  dataset$mcig = factor(dataset$mcig)
  dataset$socstat = factor(dataset$socstat)
  X <- model.matrix(as.formula(formula_exp), data = dataset)
  X <- subset(X, select=-sex2) #Remove sex2 (I have no idea why it creates it)
  colnames(X)[colnames(X)=="sex1"] <- "sex0" #Rename so it is consistent
  Y <- dataset[!is.na(dataset[var_exp]),var_exp]
  fit_lasso = glmnet(x=X, y=Y,family="gaussian", alpha=1, nlambda = 100)
  cvfit = cv.glmnet(x=X, y=Y,family="gaussian", alpha=1, lambda=fit_lasso$lambda) 
  coef_lasso <- data.matrix(coef(fit_lasso, s=cvfit$lambda.min))
  all_varnames <- c(all_varnames, rownames(coef_lasso)[which(coef_lasso!=0)])
}
table(all_varnames) 
