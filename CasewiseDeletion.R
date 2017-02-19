##############################
##### CASEWISE DELETION ######
##############################
#Import clean dataset
rm(list=ls())
library(glmnet)
library(leaps)
source("MissingDataAnalysis.R")

#Remove all rows where there are NAs
lbw_data_casewise = lbw_data[complete.cases(lbw_data),] #107 observations

#Linear regression
lm_casewise = lm(iqfull ~ . -iqverb -iqperf -rcomp -rrate -racc -tomifull -bw -code,
                 data = lbw_data_casewise)
summary(lm_casewise)
plot(lm_casewise) #Everything OK :D

#Lasso regression
fit = glmnet(x=data.matrix(lbw_data_casewise[,c(-seq(1,8),-18)]), y=data.matrix(lbw_data_casewise[,1]),
             family="gaussian", alpha=1, nlambda = 100)
set.seed(17)
cvfit = cv.glmnet(x=data.matrix(lbw_data_casewise[,-seq(1,8)]), y=data.matrix(lbw_data_casewise[,1]),
                  family="gaussian", alpha=1, lambda=fit$lambda) 
plot(cvfit)
coef(fit, s=cvfit$lambda.min)
coef(fit, s=1) #lambda.lse = 1 but for some reasons it is null if I call it directly...

#Best subset selection
fit_subset = regsubsets(iqfull ~ . -iqverb -iqperf -rcomp -rrate -racc -tomifull -bw -code,
                        data = lbw_data_casewise, method = "exhaustive")
fit_subset_summary = summary(fit_subset)
fit_subset_summary$cp
plot(fit_subset,scale="adjr2")
plot(fit_subset,scale="Cp")
plot(fit_subset,scale="bic")

