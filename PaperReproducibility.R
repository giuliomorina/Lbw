###################################
##### REPRODUCE PAPER RESULT ######
###################################

#Import clean dataset
rm(list=ls())
library(glmnet)
library(leaps)
library(xtable)
library(stargazer)
library(ggfortify)
source("MissingDataAnalysis.R")

paper_data = lbw_data #initialization

#Convert benefit in binary variable
paper_data$benef = as.numeric(paper_data$benef)
paper_data$benef[paper_data$benef>1] = 2
paper_data$benef = factor(paper_data$benef, ordered = FALSE)
#Create missing category as an additional level in the factor variable
levels(paper_data$benef) <- c(levels(paper_data$benef),3) 
paper_data$benef[is.na(paper_data$benef)] = 3
levels(paper_data$educage) <- c(levels(paper_data$educage),3) 
paper_data$educage[is.na(paper_data$educage)] = 3
levels(paper_data$fed) <- c(levels(paper_data$fed),3) 
paper_data$fed[is.na(paper_data$fed)] = 3
levels(paper_data$mcig) <- c(levels(paper_data$mcig),5) #change to assign 5 to missing mcig NA values
paper_data$mcig[is.na(paper_data$mcig)] = 5
levels(paper_data$socstat) <- c(levels(paper_data$socstat),6) 
paper_data$socstat[is.na(paper_data$socstat)] = 6


###################################################################
########################## IQ FULL ################################
###################################################################


#Remove rows where iqfull is missing
paper_data_iqfull <- paper_data[!is.na(paper_data$iqfull),]

#Linear regression (check if it works)
lm_casewise_iqfull = lm(iqfull ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat,
                 data = paper_data_iqfull)
summary(lm_casewise_iqfull)
plot(lm_casewise_iqfull)
autoplot(lm_casewise_iqfull, label.size = 3)


#Best subset
fit_subset_iqfull = regsubsets(iqfull ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat,
                        data = paper_data_iqfull, method = "exhaustive", nvmax = 20)
fit_subset_iqfull_summary = summary(fit_subset_iqfull)

fit_subset_iqfull_summary$cp
plot(fit_subset_iqfull,scale="Cp")

lm_subset_iqfull = lm(iqfull ~ rbw+ga+educage+benef,
                 data = paper_data_iqfull)
summary(lm_subset_iqfull)

#Lasso regression
X = paper_data_iqfull[,c(-2,-3,-4,-5,-6,-7)] #need to remove the cols before defining the model.matrix
X = X[,-12]
X <- model.matrix(as.formula(iqfull ~ . -1), data = X)
X <- subset(X, select=-sex1) #remove sex1 -> I have no idea why it puts it
Y <- as.matrix(paper_data_iqfull["iqfull"])
fit_iqfull = glmnet(x=X, y=Y, family="gaussian", alpha=1, nlambda = 100)
set.seed(17)
cvfit_iqfull = cv.glmnet(x=X, y=Y,family="gaussian", alpha=1, lambda=fit_iqfull$lambda) 
plot(cvfit_iqfull)
lambdaMin_fit_iqfull = as.matrix(coef(fit_iqfull, s=cvfit_iqfull$lambda.min))
coef(fit_iqfull, s=cvfit_iqfull$lambda.min)
#coef(fit, s=1) #lambda.lse = 1 but for some reasons it is null if I call it directly...

#Linear regression with selected vars from LASSO
socstat_new = paper_data_iqfull$socstat
socstat_new[socstat_new != 4 & socstat_new != 5 & socstat_new != 6]= 1 #Now socstat is 1 if < 4 and 4 or 5 
socstat_new = factor(socstat_new)

fed_new = paper_data_iqfull$fed
fed_new[fed_new != 3]= 2 #Now fed is 2 if non missing and 3 if missing 
fed_new = factor(fed_new)

mcig_new = paper_data_iqfull$mcig
mcig_new[mcig_new != 5]= 1 #Now mcig is 1 if non missing and 5 if missing 
mcig_new = factor(mcig_new)

lbw_data_casewise_LASSO_iqfull = cbind(paper_data_iqfull, socstat_new, fed_new, mcig_new)
lm_casewise_iqfull_LASSO = lm(iqfull ~ bw+rbw++sex+educage+fed_new+benef+mcig_new+socstat_new,
                              data = lbw_data_casewise_LASSO_iqfull)
summary(lm_casewise_iqfull_LASSO)


###################################################################
########################## IQ VERB ################################
###################################################################

#Remove rows where iqverb is missing
paper_data_iqverb <- paper_data[!is.na(paper_data$iqverb),]

#Linear regression (check if it works)
lm_casewise_iqverb = lm(iqverb ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat,
                        data = paper_data_iqverb)
summary(lm_casewise_iqverb)
autoplot(lm_casewise_iqverb, label.size = 3)


#Best subset
fit_subset_iqverb = regsubsets(iqverb ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat,
                        data = paper_data_iqverb, method = "exhaustive", nvmax = 20)
fit_subset_iqverb_summary = summary(fit_subset_iqverb)

fit_subset_iqverb_summary$cp
plot(fit_subset_iqverb,scale="Cp")
lm_subset_iqverb = lm(iqverb ~ rbw+ga+sex+educage+benef+matage,
               data = paper_data_iqverb)
summary(lm_subset_iqverb)


#Lasso regression
X = paper_data_iqverb[,c(-1,-3,-4,-5,-6,-7)] #need to remove the cols before defining the model.matrix
X = X[,-12]
X <- model.matrix(as.formula(iqverb ~ . -1), data = X)
X <- subset(X, select=-sex1)
Y <- as.matrix(paper_data_iqverb["iqverb"])
fit_iqverb = glmnet(x=X, y=Y, family="gaussian", alpha=1, nlambda = 100)
set.seed(17)
cvfit_iqverb = cv.glmnet(x=X, y=Y, family="gaussian", alpha=1, lambda=fit_iqverb$lambda) 
plot(cvfit_iqverb)
lambdaMin_fit_iqverb = as.matrix(coef(fit_iqverb, s=cvfit_iqverb$lambda.min))
coef(fit_iqverb, s=cvfit_iqverb$lambda.min)
#coef(fit, s=1) #lambda.lse = 1 but for some reasons it is null if I call it directly...


#Linear regression with selected vars from LASSO
fed_new = paper_data_iqverb$fed
fed_new[fed_new != 3]= 2 #Now fed is 2 if non missing and 3 if missing 
fed_new = factor(fed_new)

lbw_data_casewise_LASSO_iqverb = cbind(paper_data_iqverb, fed_new)
lm_casewise_iqverb_LASSO = lm(iqverb ~ rbw+ga+sex+educage+fed_new+benef+matage+mcig+socstat,
                              data = paper_data_iqverb)
summary(lm_casewise_iqverb_LASSO)


###################################################################
########################## IQ PERF ################################
###################################################################

#Remove rows where iqfull is missing
paper_data_iqperf <- paper_data[!is.na(paper_data$iqperf),]


#Linear regression (check if it works)
lm_casewise_iqperf = lm(iqperf ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat,
                        data = paper_data_iqperf)
summary(lm_casewise_iqperf)
autoplot(lm_casewise_iqperf, label.size = 3)

#Best subset
fit_subset_iqperf = regsubsets(iqperf ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat,
                               data = paper_data_iqperf, method = "exhaustive", nvmax = 20)
fit_subset_iqperf_summary = summary(fit_subset_iqperf)

fit_subset_iqperf_summary$cp
plot(fit_subset_iqperf,scale="Cp")
lm_subset_iqperf = lm(iqperf ~ rbw+educage+benef,
                      data = paper_data_iqperf)
summary(lm_subset_iqperf)

#Lasso regression
X <- model.matrix(as.formula(iqperf ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat-1), data = paper_data_iqperf)
X <- subset(X, select=-sex1)
Y <- as.matrix(paper_data_iqperf["iqperf"])
fit_iqperf = glmnet(x=X, y=Y,
                    family="gaussian", alpha=1, nlambda = 100)
set.seed(17)
cvfit_iqperf = cv.glmnet(x=X, y=Y,
                         family="gaussian", alpha=1, lambda=fit_iqperf$lambda) 
plot(cvfit_iqperf)
lambdaMin_fit_iqperf = as.matrix(coef(fit_iqperf, s=cvfit_iqperf$lambda.min))
coef(fit_iqperf, s=cvfit_iqperf$lambda.min)

#Linear regression with selected vars from LASSO
fed_new = paper_data_iqperf$fed
fed_new[fed_new != 3]= 2 #Now fed is 2 if non missing and 3 if missing 
fed_new = factor(fed_new)

socstat_new = paper_data_iqperf$socstat
socstat_new[socstat_new != 4 & socstat_new != 5 & socstat_new != 6]= 1 #Now socstat is 1 if < 4, and 4 or 5 or NA 
socstat_new = factor(socstat_new)

lbw_data_casewise_LASSO_iqperf = cbind(paper_data_iqperf, fed_new, socstat_new)
lm_casewise_iqperf_LASSO = lm(iqverb ~ bw+rbw+educage+fed_new+benef+socstat_new,
                              data = lbw_data_casewise_LASSO_iqperf)
summary(lm_casewise_iqperf_LASSO)

###################################################################
########################## TOMIFULL ###############################
###################################################################


#Remove rows where iqfull is missing
paper_data_tomifull = paper_data[!is.na(paper_data$tomifull),]

ggplot(paper_data_tomifull, aes(x=tomifull)) +  geom_histogram(binwidth=1, colour="black", fill="grey") +
  xlab("TOMI") + ylab("Count") + ggtitle("Distribution of TOMI")

#Linear regression (check if it works)
lm_casewise_tomifull = lm(tomifull ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat,
                        data = paper_data_tomifull)
summary(lm_casewise_tomifull)
autoplot(lm_casewise_tomifull, label.size = 3)

#Best subset
fit_subset_tomifull = regsubsets(tomifull ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat,
                               data = paper_data_tomifull, method = "exhaustive", nvmax = 20)
fit_subset_tomifull_summary = summary(fit_subset_tomifull)

fit_subset_tomifull_summary$cp
plot(fit_subset_tomifull,scale="Cp")
lm_subset_tomifull = lm(tomifull ~ bw+rbw+ga+educage+benef, #only educage missing and benef missing are included
                      data = paper_data_tomifull)
summary(lm_subset_tomifull)

#Lasso regression
X = paper_data_tomifull[,c(-1,-2,-3,-4,-5,-6)] #need to remove the cols before defining the model.matrix
X = X[,-12]

X <- model.matrix(as.formula(tomifull ~ . -1), data = X)
X <- subset(X, select=-sex1)
Y <- as.matrix(paper_data_tomifull["tomifull"])
fit_tomi = glmnet(x=X, y=Y,
                  family="gaussian", alpha=1, nlambda = 100)
set.seed(17)
cvfit_tomi = cv.glmnet(x=X, y=Y,
                       family="gaussian", alpha=1, lambda=fit_tomi$lambda) 
plot(cvfit_tomi)
lambdaMin_fit_tomi = as.matrix(coef(fit_tomi, s=cvfit_tomi$lambda.min))
coef(fit_tomi, s=cvfit_tomi$lambda.min)

#Linear regression with selected vars from LASSO
fed_new = paper_data_tomifull$fed
fed_new[fed_new != 3]= 2 #Now fed is 2 if non missing and 3 if missing 
fed_new = factor(fed_new)

educage_new = paper_data_tomifull$educage
educage_new[educage_new!= 3]= 2 #Now educage is 2 if non missing and 3 if missing 
educage_new = factor(educage_new)

benef_new = paper_data_tomifull$benef
benef_new[benef_new!= 3]= 2 #Now benef is 2 if non missing and 3 if missing 
benef_new = factor(benef_new)

lbw_data_casewise_LASSO_tomifull = cbind(paper_data_tomifull, fed_new, educage_new, benef_new)
lm_casewise_tomifull_LASSO = lm(tomifull ~ bw+educage_new+fed_new+benef_new,
                              data = lbw_data_casewise_LASSO_tomifull)
summary(lm_casewise_tomifull_LASSO)

###################################################################
################### Tables for report #############################
###################################################################

# Table for liner regression with casewise delection
stargazer(lm_casewise_iqfull, 
          lm_casewise_iqverb, 
          lm_casewise_iqperf, 
          lm_casewise_tomifull, 
          type = "latex", 
          dep.var.labels  = c("Full IQ", "Verbal IQ", "Performance IQ", "TOMI"),
          dep.var.caption = "",
          covariate.labels = c("Birth weight", "Birth weight ratio", "Gestational age",
                              "Sex", "Mother edu. <= 16","Mother edu. NA", "Father edu. <= 16",
                              "Father edu. NA", "Social benefit > 1", "Social benefits NA",
                              "4 social benefits", "Mother age",
                              "Cig. < 10", "Cig. 10-19", "Cig. >= 20",
                              "Socio economic status 2",
                              "Socio economic status 3",
                              "Socio economic status 4",
                              "Socio economic status 5",
                              "Socio economic status 6","Intercept"),
          ci = FALSE,
          title = "Linear regression with NA indicators",
          single.row = FALSE, 
          report = "vc*",
          no.space = TRUE,
          omit.stat=c("f", "ser"))


# Table for liner regression with subset selection
stargazer(lm_subset_iqfull, 
          lm_subset_iqverb, 
          lm_subset_iqperf, 
          lm_subset_tomifull, 
          type = "latex", 
          dep.var.labels  = c("Full IQ", "Verbal IQ", "Performance IQ", "TOMI"),
          dep.var.caption = "",
          covariate.labels = c("Intercept","Birth weight", "Birth weight ratio", "Gestational age",
                              "Sex", "Mother edu. <= 16","Mother edu. NA", "Father edu. <= 16",
                              "Father edu. NA", "Social benefit > 1", "Social benefits NA",
                              "4 social benefits", "Mother age",
                              "Cig. < 10", "Cig. 10-19", "Cig. >= 20",
                              "Socio economic status 2",
                              "Socio economic status 3",
                              "Socio economic status 4",
                              "Socio economic status 5"),
          ci = FALSE,
          title = "Linear regression with subset selection and NA indicators (casewise delection)",
          single.row = FALSE, 
          report = "vc*",
          no.space = TRUE,
          omit.stat=c("f", "ser"))


# Table for LASSO with NA indicators
# lasso_results = cbind(lambdaMin_fit_iqfull, 
#                       lambdaMin_fit_iqverb, 
#                       lambdaMin_fit_iqperf, 
#                       lambdaMin_fit_tomi)
# stargazer(lasso_results,
#           type = "latex", 
#           dep.var.labels  = c("Full IQ", "Verbal IQ", "Performance IQ", "TOMI"),
#           dep.var.caption = "",
#           ci = FALSE,
#           title = "LASSO with NA indicators",
#           single.row = FALSE, 
#           report = "vc*",
#           no.space = TRUE,
#           omit.stat=c("f", "ser"))


# Table for liner regression with vars selected by LASSO
stargazer(lm_casewise_iqfull_LASSO, 
          lm_casewise_iqverb_LASSO, 
          lm_casewise_iqperf_LASSO, 
          lm_casewise_tomifull_LASSO, 
          type = "latex", 
          dep.var.labels  = c("Full IQ", "Verbal IQ", "Performance IQ", "TOMI"),
          dep.var.caption = "",
          covariate.labels = c("Birth weight", "Birth weight ratio", "Gestational age",
                              "Sex", "Mother edu. <= 16","Mother edu. NA", "Father edu. <= 16",
                              "Father edu. NA", "Social benefit > 1", "Social benefits NA",
                              "4 social benefits", "Mother age",
                              "Cig. < 10", "Cig. 10-19", "Cig. >= 20",
                              "Socio economic status 2",
                              "Socio economic status 3",
                              "Socio economic status 4",
                              "Socio economic status 5","Intercept"),
          ci = FALSE,
          title = "Linear regression with NA indicator variables selected by LASSO (casewise delection)",
          single.row = FALSE, 
          report = "vc*",
          no.space = TRUE,
          omit.stat=c("f", "ser"))
