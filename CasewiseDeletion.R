##############################
##### CASEWISE DELETION ######
##############################
#Import clean dataset
rm(list=ls())
library(glmnet)
library(leaps)
library(xtable)
library(stargazer)
source("MissingDataAnalysis.R")

#Remove all rows where there are NAs
lbw_data_casewise = lbw_data[complete.cases(lbw_data),] #107 observations


##########
#IQ FULL
##########

#Linear regression
lm_casewise_iqfull = lm(iqfull ~ . -iqverb -iqperf -rcomp -rrate -racc -tomifull -code,
                 data = lbw_data_casewise)
summary(lm_casewise_iqfull)
plot(lm_casewise_iqfull) #Everything OK :D

#Lasso regression
fit_iqfull = glmnet(x=data.matrix(lbw_data_casewise[,c(-seq(1,7),-18)]), y=data.matrix(lbw_data_casewise[,1]),
             family="gaussian", alpha=1, nlambda = 100)
set.seed(17)
cvfit_iqfull = cv.glmnet(x=data.matrix(lbw_data_casewise[,-seq(1,7)]), y=data.matrix(lbw_data_casewise[,1]),
                  family="gaussian", alpha=1, lambda=fit_iqfull$lambda) 
plot(cvfit)
lambdaMin_fit_iqfull = as.matrix(coef(fit_iqfull, s=cvfit_iqfull$lambda.min))
#coef(fit, s=cvfit$lambda.min)
#coef(fit, s=1) #lambda.lse = 1 but for some reasons it is null if I call it directly...

# #Best subset selection
# fit_subset = regsubsets(iqfull ~ . -iqverb -iqperf -rcomp -rrate -racc -tomifull -bw -code,
#                         data = lbw_data_casewise, method = "exhaustive")
# fit_subset_summary = summary(fit_subset)
# fit_subset_summary$cp
# plot(fit_subset,scale="adjr2")
# plot(fit_subset,scale="Cp")
# plot(fit_subset,scale="bic")


##########
#IQ VERB
##########

#Linear regression
lm_casewise_iqverb = lm(iqverb ~ . -iqfull -iqperf -rcomp -rrate -racc -tomifull -code,
                        data = lbw_data_casewise)
summary(lm_casewise_iqverb)
#plot(lm_casewise_iqverb) #Everything OK :D


#Lasso regression
fit_iqverb = glmnet(x=data.matrix(lbw_data_casewise[,c(-seq(1,7),-18)]), y=data.matrix(lbw_data_casewise[,2]),
             family="gaussian", alpha=1, nlambda = 100)
set.seed(17)
cvfit_iqverb = cv.glmnet(x=data.matrix(lbw_data_casewise[,-seq(1,7)]), y=data.matrix(lbw_data_casewise[,2]),
                  family="gaussian", alpha=1, lambda=fit_iqverb$lambda) 
plot(cvfit)
lambdaMin_fit_iqverb = as.matrix(coef(fit_iqverb, s=cvfit_iqverb$lambda.min))
#coef(fit, s=cvfit$lambda.min)
#coef(fit, s=1) #lambda.lse = 1 but for some reasons it is null if I call it directly...

# #Best subset selection
# fit_subset = regsubsets(iqverb ~ . -iqfull -iqperf -rcomp -rrate -racc -tomifull -code,
#                         data = lbw_data_casewise, method = "exhaustive")
# fit_subset_summary = summary(fit_subset)
# fit_subset_summary$cp
# plot(fit_subset,scale="adjr2")
# plot(fit_subset,scale="Cp")
# plot(fit_subset,scale="bic")
# 

##########
#IQ PERF
##########

#Linear regression
lm_casewise_iqperf = lm(iqperf ~ . -iqverb -iqfull -rcomp -rrate -racc -tomifull -code,
                        data = lbw_data_casewise)
summary(lm_casewise_iqperf)
#plot(lm_casewise_iqperf) #Everything OK :D


#Lasso regression
fit_iqperf = glmnet(x=data.matrix(lbw_data_casewise[,c(-seq(1,7),-18)]), y=data.matrix(lbw_data_casewise[,3]),
                    family="gaussian", alpha=1, nlambda = 100)
set.seed(17)
cvfit_iqperf = cv.glmnet(x=data.matrix(lbw_data_casewise[,-seq(1,7)]), y=data.matrix(lbw_data_casewise[,3]),
                         family="gaussian", alpha=1, lambda=fit_iqperf$lambda) 
plot(cvfit)
lambdaMin_fit_iqperf = as.matrix(coef(fit_iqperf, s=cvfit_iqperf$lambda.min))


##########
#TOMI FULL
##########

#Linear regression
lm_casewise_tomifull = lm(tomifull ~ . -iqverb -iqfull -iqperf -rcomp -rrate -racc -code,
                        data = lbw_data_casewise)
summary(lm_casewise_tomifull)
#plot(lm_casewise_tomifull) #Everything OK :D


#Lasso regression
fit_tomi = glmnet(x=data.matrix(lbw_data_casewise[,c(-seq(1,7),-18)]), y=data.matrix(lbw_data_casewise[,7]),
                    family="gaussian", alpha=1, nlambda = 100)
set.seed(17)
cvfit_tomi = cv.glmnet(x=data.matrix(lbw_data_casewise[,-seq(1,7)]), y=data.matrix(lbw_data_casewise[,7]),
                         family="gaussian", alpha=1, lambda=fit_tomi$lambda) 
plot(cvfit)
lambdaMin_fit_tomi = as.matrix(coef(fit_tomi, s=cvfit_tomi$lambda.min))


############################ Tables for report ##############################

# Table for liner regression with casewise delection
stargazer(lm_casewise_iqfull, 
          lm_casewise_iqverb, 
          lm_casewise_iqperf, 
          lm_casewise_tomifull, 
          type = "latex", 
          dep.var.labels  = c("Full IQ", "Verbal IQ", "Performance IQ", "TOMI"),
          dep.var.caption = "",
          covariate.labels = c("Birth weight ratio", "Birth weight", "Gestational age", 
                               "Sex", "Mother edu. <= 16", "Father edu. <= 16",
                               "2 social benefits", "3 social benefits",
                               "4 social benefits", "Mother age",
                               "Cig. < 10", "Cig. 10-19", "Cig. >= 20",
                               "Socio economic status 2",
                               "Socio economic status 3",
                               "Socio economic status 4",
                               "Socio economic status 5",
                               "Intercept"),
          ci = FALSE,
          title = "Linear regression with casewise delection",
          single.row = FALSE, 
          report = "vc*",
          no.space = TRUE,
          omit.stat=c("f", "ser"))

lasso_results = cbind(lambdaMin_fit_iqfull, 
                       lambdaMin_fit_iqverb, 
                       lambdaMin_fit_iqperf, 
                       lambdaMin_fit_tomi)
stargazer(lasso_results)
# Table for LASSO with casewise delection
stargazer(lasso_results,
          type = "latex", 
          dep.var.labels  = c("Full IQ", "Verbal IQ", "Performance IQ", "TOMI"),
          dep.var.caption = "",
          covariate.labels = c("Intercept", "Birth weight", "Birth weight ratio", "Gestational age", 
                               "Sex", "Mother edu. <= 16", "Father edu. <= 16",
                               "2 social benefits", "3 social benefits",
                               "4 social benefits", "Mother age",
                               "Cig. < 10", "Cig. 10-19", "Cig. >= 20",
                               "Socio economic status 2",
                               "Socio economic status 3",
                               "Socio economic status 4",
                               "Socio economic status 5",
                               "Intercept"),
          ci = FALSE,
          title = "Linear regression with casewise delection",
          single.row = FALSE, 
          report = "vc*",
          no.space = TRUE,
          omit.stat=c("f", "ser"))
