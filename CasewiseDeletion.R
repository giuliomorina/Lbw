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
#plot(lm_casewise_iqfull) #Everything OK :D

#Lasso regression
X <- model.matrix(as.formula(iqfull ~ . -iqverb -iqperf -rcomp -rrate -racc -tomifull -code -1), data = lbw_data_casewise)
X <- subset(X, select=-sex1) #remove sex1 -> I have no idea why it puts it
Y <- as.matrix(lbw_data_casewise["iqfull"])
fit_iqfull = glmnet(x=X, y=Y, family="gaussian", alpha=1, nlambda = 100)
set.seed(17)
cvfit_iqfull = cv.glmnet(x=X, y=Y,family="gaussian", alpha=1, lambda=fit_iqfull$lambda) 
plot(cvfit_iqfull)
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

#Linear regression with selected vars from LASSO
mcig_new = lbw_data_casewise$mcig
mcig_new[mcig_new != 1] = 2
mcig_new = factor(mcig_new)

lbw_data_casewise_LASSO_iqfull = cbind(lbw_data_casewise, mcig_new)
lm_casewise_iqfull_LASSO = lm(iqfull ~ . -iqverb -iqperf -rcomp -rrate -racc -bw -tomifull -mcig -code,
                        data = lbw_data_casewise_LASSO_iqfull)
summary(lm_casewise_iqfull_LASSO)

##########
#IQ VERB
##########

#Linear regression
lm_casewise_iqverb = lm(iqverb ~ . -iqfull -iqperf -rcomp -rrate -racc -tomifull -code,
                        data = lbw_data_casewise)
summary(lm_casewise_iqverb)
#plot(lm_casewise_iqverb) #Everything OK :D

#Lasso regression
X <- model.matrix(as.formula(iqverb ~ . -iqfull -iqperf -rcomp -rrate -racc -tomifull -code -1), data = lbw_data_casewise)
X <- subset(X, select=-sex1)
Y <- as.matrix(lbw_data_casewise["iqverb"])
fit_iqverb = glmnet(x=X, y=Y, family="gaussian", alpha=1, nlambda = 100)
set.seed(17)
cvfit_iqverb = cv.glmnet(x=X, y=Y, family="gaussian", alpha=1, lambda=fit_iqverb$lambda) 
plot(cvfit_iqverb)
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

#Linear regression with selected vars from LASSO
lm_casewise_iqverb_LASSO = lm(iqverb ~ . -iqfull -iqperf -rcomp -rrate -racc -tomifull -bw -code,
                              data = lbw_data_casewise)
summary(lm_casewise_iqverb_LASSO)


##########
#IQ PERF
##########

#Linear regression
lm_casewise_iqperf = lm(iqperf ~ . -iqverb -iqfull -rcomp -rrate -racc -tomifull -code,
                        data = lbw_data_casewise)
summary(lm_casewise_iqperf)
#plot(lm_casewise_iqperf) #Everything OK :D

#Lasso regression
X <- model.matrix(as.formula(iqperf ~ . -iqfull -iqverb -rcomp -rrate -racc -tomifull -code -1), data = lbw_data_casewise)
X <- subset(X, select=-sex1)
Y <- as.matrix(lbw_data_casewise["iqperf"])
fit_iqperf = glmnet(x=X, y=Y,
                    family="gaussian", alpha=1, nlambda = 100)
set.seed(17)
cvfit_iqperf = cv.glmnet(x=X, y=Y,
                         family="gaussian", alpha=1, lambda=fit_iqperf$lambda) 
plot(cvfit_iqperf)
lambdaMin_fit_iqperf = as.matrix(coef(fit_iqperf, s=cvfit_iqperf$lambda.min))



#Linear regression with selected vars from LASSO
socstat_new = lbw_data_casewise$socstat
socstat_new[socstat_new != 4 & socstat_new != 5]= 1 #Now socstat is 1 if < 4 
socstat_new = factor(socstat_new)

lbw_data_casewise_LASSO_iqperf = cbind(lbw_data_casewise, socstat_new)
lm_casewise_iqperf_LASSO = lm(iqperf ~ . -iqfull -iqverb -rcomp -rrate -racc -tomifull -ga -sex -mcig -socstat -code,
                              data = lbw_data_casewise_LASSO_iqperf)
summary(lm_casewise_iqperf_LASSO)


##########
#TOMI FULL
##########

#Linear regression
lm_casewise_tomifull = lm(tomifull ~ . -iqverb -iqfull -iqperf -rcomp -rrate -racc -code,
                        data = lbw_data_casewise)
summary(lm_casewise_tomifull)
#plot(lm_casewise_tomifull) #Everything OK :D


#Lasso regression
X <- model.matrix(as.formula(tomifull ~ . -iqfull -iqverb -iqperf -rcomp -rrate -racc -code -1), data = lbw_data_casewise)
X <- subset(X, select=-sex1)
Y <- as.matrix(lbw_data_casewise["tomifull"])
fit_tomi = glmnet(x=X, y=Y,
                    family="gaussian", alpha=1, nlambda = 100)
set.seed(17)
cvfit_tomi = cv.glmnet(x=X, y=Y,
                         family="gaussian", alpha=1, lambda=fit_tomi$lambda) 
plot(cvfit_tomi)
lambdaMin_fit_tomi = as.matrix(coef(fit_tomi, s=cvfit_tomi$lambda.min))


#Linear regression with selected vars from LASSO

lm_casewise_tomi_LASSO = lm(tomifull ~ . -iqfull -iqverb -iqperf -rcomp -rrate -racc -rbw -ga -sex -educage
                            -fed -benef -matage -mcig -socstat -code,
                              data = lbw_data_casewise)
summary(lm_casewise_tomi_LASSO)


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


# Table for LASSO with casewise delection
lasso_results = cbind(lambdaMin_fit_iqfull, 
                      lambdaMin_fit_iqverb, 
                      lambdaMin_fit_iqperf, 
                      lambdaMin_fit_tomi)
stargazer(lasso_results,
          type = "latex", 
          dep.var.labels  = c("Full IQ", "Verbal IQ", "Performance IQ", "TOMI"),
          dep.var.caption = "",
          ci = FALSE,
          title = "LASSO with casewise delection",
          single.row = FALSE, 
          report = "vc*",
          no.space = TRUE,
          omit.stat=c("f", "ser"))


# Table for liner regression with vars selected by LASSO
stargazer(lm_casewise_iqfull_LASSO, 
          lm_casewise_iqverb_LASSO, 
          lm_casewise_iqperf_LASSO, 
          lm_casewise_tomi_LASSO, 
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
          title = "Linear regression with variables selected by LASSO",
          single.row = FALSE, 
          report = "vc*",
          no.space = TRUE,
          omit.stat=c("f", "ser"))