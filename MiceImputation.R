rm(list=ls())
library(mice)
library(leaps)
library(glmnet)
library(plyr)
library(stargazer)
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
m <- 500
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
fit_iqfull <- with(imp, lm(iqfull ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat))
fit_iqfull_summ <- data.frame(summary(pool(fit_iqfull)))
fit_iqfull_summ[fit_iqfull_summ$Pr...t..<0.01,]
fit_iqfull_summ[(fit_iqfull_summ$Pr...t..>=0.01 & fit_iqfull_summ$Pr...t..<0.05),]
fit_iqfull_summ[(fit_iqfull_summ$Pr...t..>=0.05 & fit_iqfull_summ$Pr...t..<0.1),]

fit_iqfull_summ <- fit_iqfull_summ[,"est"]
#Compute R^2
pool.r.squared(fit_iqfull, adjusted=FALSE) #0.2833940
pool.r.squared(fit_iqfull, adjusted=TRUE)  #0.1960677

fit_iqverb <- with(imp, lm(iqverb ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat))
fit_iqverb_summ <- data.frame(summary(pool(fit_iqverb)))
fit_iqverb_summ[fit_iqverb_summ$Pr...t..<0.01,]
fit_iqverb_summ[(fit_iqverb_summ$Pr...t..>=0.01 & fit_iqverb_summ$Pr...t..<0.05),]
fit_iqverb_summ[(fit_iqverb_summ$Pr...t..>=0.05 & fit_iqverb_summ$Pr...t..<0.1),]
fit_iqverb_summ <- fit_iqverb_summ[,"est"]
#Compute R^2
pool.r.squared(fit_iqverb, adjusted=FALSE) #0.3187244
pool.r.squared(fit_iqverb, adjusted=TRUE)  #0.2368466

fit_iqperf <- with(imp, lm(iqperf ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat))
fit_iqperf_summ <- data.frame(summary(pool(fit_iqperf)))
fit_iqperf_summ[fit_iqperf_summ$Pr...t..<0.01,]
fit_iqperf_summ[(fit_iqperf_summ$Pr...t..>=0.01 & fit_iqperf_summ$Pr...t..<0.05),]
fit_iqperf_summ[(fit_iqperf_summ$Pr...t..>=0.05 & fit_iqperf_summ$Pr...t..<0.1),]
fit_iqperf_summ <- fit_iqperf_summ[,"est"]
#Compute R^2
pool.r.squared(fit_iqperf, adjusted=FALSE) #0.1988763
pool.r.squared(fit_iqperf, adjusted=TRUE)  #0.1011408

fit_tomifull <- with(imp, lm(tomifull ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat))
fit_tomifull_summ <- data.frame(summary(pool(fit_tomifull)))
fit_tomifull_summ[fit_tomifull_summ$Pr...t..<0.01,]
fit_tomifull_summ[(fit_tomifull_summ$Pr...t..>=0.01 & fit_tomifull_summ$Pr...t..<0.05),]
fit_tomifull_summ[(fit_tomifull_summ$Pr...t..>=0.05 & fit_tomifull_summ$Pr...t..<0.1),]
fit_tomifull_summ <- fit_tomifull_summ[,"est"]
#Compute R^2
pool.r.squared(fit_tomifull, adjusted=FALSE) #0.117340
pool.r.squared(fit_tomifull, adjusted=TRUE)  #NA


mice_lm_results = cbind(fit_iqfull_summ, 
                        fit_iqverb_summ, 
                        fit_iqperf_summ, 
                        fit_tomifull_summ)
rownames(mice_lm_results) <- c("Intercept","Birth weight ratio", "Birth weight", "Gestational age", 
"Sex", "Mother edu. <= 16", "Father edu. <= 16",
"2 social benefits", "3 social benefits",
"4 social benefits", "Mother age",
"Cig. < 10", "Cig. 10-19", "Cig. >= 20",
"Socio economic status 2",
"Socio economic status 3",
"Socio economic status 4",
"Socio economic status 5")
colnames(mice_lm_results) <- c("Full IQ", "Verbal IQ", "Performance IQ", "TOMI")

stargazer(mice_lm_results,
          type = "latex", 
          dep.var.labels  = c("Full IQ", "Verbal IQ", "Performance IQ", "TOMI"),
          dep.var.caption = "",
          ci = FALSE,
          rownames = TRUE,
          colnames = TRUE,
          title = "Linear regression with multiple imputation using mice",
          no.space = TRUE,
          omit.stat=c("f", "ser"))

# \begin{table}[!htbp] \centering 
# \caption{Linear regression with multiple imputation using \texttt{mice}}
# \label{} 
# \begin{tabular}{@{\extracolsep{5pt}} lcccc} 
# \\[-1.8ex]\hline 
# \hline \\[-1.8ex] 
# & Full IQ & Verbal IQ & Performance IQ & TOMI \\ 
# \hline \\[-1.8ex] 
# Intercept & $4.989$ & $28.672$ & $$-$10.068$ & $40.191^*$ \\ 
# Birth weight ratio & $47.101^{**}$ & $33.791$ & $55.519^{**}$ & $$-$10.533$ \\ 
# Birth weight  & $$-$16.783$ & $$-$12.314$ & $$-$18.721$ & $4.428$ \\ 
# Gestational age & $2.733$ & $2.033$ & $3.116$ & $$-$1.125$ \\ 
# Sex & $$-$3.894$ & $$-$3.906$ & $$-$2.017$ & $$-$0.497$ \\ 
# Mother edu. \textless = 16 & $$-$4.369$ & $$-$5.721$ & $$-$2.295$ & $$-$0.993$ \\ 
# Father edu. \textless = 16 & $0.631$ & $0.829$ & $0.835$ & $0.255$ \\ 
# 2 social benefits & $$-$6.928^{**}$ & $$-$6.497^{**}$ & $$-$5.613$ & $$-$0.279$ \\ 
# 3 social benefits & $$-$10.074^{**}$ & $$-$11.208^{***}$ & $$-$6.186$ & $0.089$ \\ 
# 4 social benefits & $$-$8.412$ & $$-$10.130^*$ & $$-$4.725$ & $$-$1.591$ \\ 
# Mother age & $0.006$ & $0.221$ & $$-$0.203$ & $0.088$ \\ 
# Cig. \textless  10 & $3.804$ & $6.980^*$ & $$-$1.625$ & $$-$0.378$ \\ 
# Cig. 10-19 & $2.274$ & $4.278$ & $$-$0.854$ & $$-$0.365$ \\ 
# Cig. \textgreater = 20 & $2.052$ & $3.337$ & $$-$0.766$ & $$-$1.097$ \\ 
# Socio economic status 2 & $$-$5.705$ & $$-$8.814^{**}$ & $$-$1.704$ & $$-$0.302$ \\ 
# Socio economic status 3 & $$-$1.236$ & $$-$1.889$ & $$-$0.364$ & $0.604$ \\ 
# Socio economic status 4 & $$-$9.071^{**}$ & $$-$10.418^{**}$ & $$-$6.555$ & $1.453$ \\ 
# Socio economic status 5 & $$-$7.138^*$ & $$-$9.996^{**}$ & $$-$3.501$ & $1.361$ \\ 
# \hline \\[-1.8ex] 
# Observations & 182 & 182 & 182 & 182 \\ 
# R$^{2}$ & 0.283 & 0.319 & 0.199 & 0.117 \\ 
# Adjusted R$^{2}$ & 0.196 & 0.237 & 0.101 & 0.026 \\ 
# \hline 
# \hline \\[-1.8ex] 
# \textit{Note:}  & \multicolumn{4}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
# \end{tabular} 
# \end{table} 

###################
# Ordinal logit ##
##################
lbw_data_ordered <- lbw_data
lbw_data_ordered$tomifull <- ordered(floor(lbw_data_ordered$tomifull)) #Remove .5 and floor them
imp2 <- mice(lbw_data_ordered, seed = 17, method=c("","","","","","","","pmm","pmm","pmm","logreg",
                                          "logreg","logreg","pmm","pmm","polr","polr",""),
            predictorMatrix=pred, visitSequence = "monotone", m=m)
fit_ordered <- with(imp2, polr(tomifull ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat, Hess=TRUE))
summary(pool(fit_ordered))

##################
#Subset selection
##################
m <- 500
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
all_varnames_iqfull <- data.matrix(table(all_varnames))
# all_varnames
# benef2   benef3   benef4 educage2     fed2       ga    mcig2      rbw     sex2 socstat2 socstat3 socstat4 socstat5 
# 46       49       28       33        2       10        2       50       24       30        2       37       33 
#Note that mice rename some factor names (check  ciao <- data.matrix(complete(imp)) to see)

#Fit again a linear model using only benef, educage, rbw, socstat (>50%)
# fit_sub <- with(imp, lm(iqfull ~ rbw+educage+benef+socstat))
# summary(pool(fit_sub))
# coef2 <- pool(fit_sub)$qbar
# X <- model.matrix(iqfull ~ rbw+educage+benef+socstat, data = lbw_data)
# Y <- lbw_data[!is.na(rbw) & !is.na(educage) & !is.na(benef) & !is.na(socstat),"iqfull"] 
# computeRsquared(coef2,X,Y)
# $R2
# [1] 0.3152292
# 
# $adj_R2
# [1] 0.251232

################
#Compare different sizes of imputation
################
# fitted_lm <- vector("list")
# counter <- 1
# for (m in c(2,5,10,50,100)) {
#   imputation <- mice(lbw_data, seed = 17, method=c("","","","","","","","pmm","pmm","pmm","logreg",
#                                                    "logreg","logreg","pmm","pmm","polr","polr",""),
#                      predictorMatrix=pred, visitSequence = "monotone", m=m)
#   fitted_lm[[counter]] <- with(imputation, lm(iqfull ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat))
#   counter <- counter+1
# }
# 
# summary(fitted_lm[[1]])
# summary(pool(fitted_lm[[5]]))
# 
# #Compute R^2 for each model and compare
# R2 <- numeric(length(fitted_lm))
# adj_R2 <- numeric(length(fitted_lm))
# X <- model.matrix(iqfull ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat, data = lbw_data)
# Y <- lbw_data[complete.cases(lbw_data),"iqfull"] #remove all rows where there are missing data. Otherwise we can not compute R squared
# for (i in 1:length(fitted_lm)) {
#   pool_fit <- pool(fitted_lm[[i]])
#   coef <- pool_fit$qbar
#   list_R2 <- computeRsquared(coef,X,Y)
#   R2[i] <- list_R2[[1]]
#   adj_R2[i] <- list_R2[[2]]
# }
# print(R2)
# print(adj_R2)

################################################################
#Imputation and subset selection for iqverb, iqperf, tomifull
###############################################################

########
#iqverb
########
m<-500
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
all_varnames_iqverb <- data.matrix(table(all_varnames))
# benef2   benef3   benef4 educage2     fed2   matage    mcig2    mcig3    mcig4      rbw     sex2 socstat2 socstat3 
# 42       50       37       40        5        1       33       17       13       50       44       49        3 
# socstat4 socstat5 
# 48       48 

#Fit again a linear model using only benef, educage, mcig, rbw, socstat, sex
# fit_sub <- with(imp, lm(iqverb ~ rbw+educage+benef+socstat+mcig+sex))
# summary(pool(fit_sub))
# coef2 <- pool(fit_sub)$qbar
# X <- model.matrix(iqverb ~ rbw+educage+benef+socstat+mcig+sex, data = lbw_data)
# Y <- lbw_data[!is.na(rbw) & !is.na(educage) & !is.na(benef) & !is.na(socstat) & !is.na(mcig) & !is.na(sex),"iqverb"] 
# computeRsquared(coef2,X,Y) 
# $R2
# [1] 0.4251156
# 
# $adj_R2
# [1] 0.3446317

############
#iqperf
############
m<-500
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
all_varnames_iqperf <- data.matrix(table(all_varnames))
# benef2   benef3   benef4 educage2       ga      rbw socstat4 socstat5 
# 40       35       12       18        5       50       26       15  

#Fit again a linear model using only benef, rbw, socstat
# fit_sub <- with(imp, lm(iqperf ~ rbw+benef+socstat))
# summary(pool(fit_sub))
# coef2 <- pool(fit_sub)$qbar
# X <- model.matrix(iqperf ~ rbw+benef+socstat, data = lbw_data)
# Y <- lbw_data[!is.na(rbw) & !is.na(benef) & !is.na(socstat),"iqperf"] 
# computeRsquared(coef2,X,Y) 
# $R2
# [1] 0.1493153
# 
# $adj_R2
# [1] 0.08095673

#############
#tomifull
############
m<-500
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
all_varnames_tomifull <- data.matrix(table(all_varnames))
# benef2   benef3   benef4 educage2     fed2       ga      rbw     sex2 socstat4 
# 6        6       23       18        9       26       26       33        2 
#Fit again a linear model using only ga,rbw, sex
# fit_sub <- with(imp, lm(tomifull ~ rbw+ga+sex))
# summary(pool(fit_sub))
# coef2 <- pool(fit_sub)$qbar
# X <- model.matrix(tomifull ~ rbw+ga+sex, data = lbw_data)
# Y <- lbw_data[!is.na(rbw) & !is.na(ga) & !is.na(sex) & !is.na(tomifull),"tomifull"] 
# computeRsquared(coef2,X,Y) 

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

all_varnames_aux <- rbind(all_varnames_iqfull, all_varnames_iqverb, all_varnames_iqperf, all_varnames_tomifull)
all_varnames_combined <- matrix(data = 0, nrow=length(unique(rownames(all_varnames_aux))), ncol=4)
rownames(all_varnames_combined) <- unique(rownames(all_varnames_aux))
colnames(all_varnames_combined) <- c("Full IQ", "Verbal IQ", "Performance IQ", "TOMI")
counter <- 1
for (aux in list(all_varnames_iqfull, all_varnames_iqverb, all_varnames_iqperf, all_varnames_tomifull)) {
  all_varnames_combined[rownames(aux),counter] <- aux
  counter <- counter+1
}
all_varnames_combined_subset <- all_varnames_combined/m*100



############
#LASSO
############
rm(list=setdiff(ls(), c("imp","lbw_data","all_varnames_combined_subset")))

find_variables <- function(imp,formula_exp, var_exp, m) {
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
return(all_varnames)
}

set.seed(17)
all_varnames_list <- vector("list")
counter <- 1
for (var_exp in c("iqfull","iqverb","iqperf","tomifull")) {
  formula_exp <- paste(var_exp," ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat-1", sep = "")
  all_varnames <- find_variables(imp,formula_exp, var_exp, m = 500)
  print(var_exp)
  print(table(all_varnames))
  all_varnames_list[[counter]] <- data.matrix(table(all_varnames))
  counter <- counter+1
}

all_varnames_aux <- rbind(all_varnames_list[[1]],all_varnames_list[[2]],all_varnames_list[[3]],all_varnames_list[[4]])
all_varnames_combined <- matrix(data = 0, nrow=length(unique(rownames(all_varnames_aux))), ncol=4)
rownames(all_varnames_combined) <- unique(rownames(all_varnames_aux))
colnames(all_varnames_combined) <- c("Full IQ", "Verbal IQ", "Performance IQ", "TOMI")
counter <- 1
for (aux in all_varnames_list) {
  all_varnames_combined[rownames(aux),counter] <- aux
  counter <- counter+1
}
all_varnames_combined_lasso <- all_varnames_combined/500*100

all_varnames_whole <- matrix(data = 0, nrow=length(unique(rownames(all_varnames_combined_lasso))), ncol=8)
rownames(all_varnames_whole) <- unique(rownames(all_varnames_combined_lasso))
rownames(all_varnames_whole)[rownames(all_varnames_whole) == "sex0"] <- "sex2"
colnames(all_varnames_whole) <- c("Full IQ", "Verbal IQ", "Performance IQ", "TOMI","Full IQ", "Verbal IQ", "Performance IQ", "TOMI")
all_varnames_whole[,c(5,6,7,8)] <- all_varnames_combined_lasso
for (j in 1:4) {
  all_varnames_whole[rownames(all_varnames_combined_subset),j] <- all_varnames_combined_subset[,j]
}
all_varnames_whole["(Intercept)",] <- 100
all_varnames_whole <- all_varnames_whole[,c(1,5,2,6,3,7,4,8)]


all_varnames_whole <- all_varnames_whole[c("(Intercept)","rbw","bw","ga","sex2","educage2","fed2","benef2",
                                           "benef3","benef4","matage","mcig2","mcig3","mcig4","socstat2",
                                           "socstat3","socstat4","socstat5"),]
rownames(all_varnames_whole) <- c("Intercept","Birth weight ratio", "Birth weight", "Gestational age", 
                               "Sex", "Mother edu. <= 16", "Father edu. <= 16",
                               "2 social benefits", "3 social benefits",
                               "4 social benefits", "Mother age",
                               "Cig. < 10", "Cig. 10-19", "Cig. >= 20",
                               "Socio economic status 2",
                               "Socio economic status 3",
                               "Socio economic status 4",
                               "Socio economic status 5")

stargazer(all_varnames_whole,
          type = "latex", 
          ci = FALSE,
          rownames = TRUE,
          colnames = TRUE,
          digits = 1,
          title = "How many times (%) each variable is selected by subset selection and LASSO in 500 imputed datasets",
          no.space = TRUE,
          omit.stat=c("f", "ser"))

# \begin{table}[!htbp] \centering 
# \caption{How many times (\%) each variable is selected by subset selection and LASSO in 500 imputed datasets with \texttt{mice}} 
# \label{tbl:subset_lasso_mice} 
# \begin{tabular}{@{\extracolsep{5pt}} lcccccccc} 
# \\[-1.8ex]\hline 
# \hline \\[-1.8ex] 
# & \multicolumn{2}{c}{Full IQ} & \multicolumn{2}{c}{Verbal IQ} & \multicolumn{2}{c}{Performance IQ} & \multicolumn{2}{c}{TOMI}\\ 
# & Subset & LASSO & Subset & LASSO & Subset & LASSO & Subset & LASSO \\ 
# \hline \\[-1.8ex] 
# Intercept & $\textbf{100}$ & $\textbf{100}$ & $\textbf{100}$ & $\textbf{100}$ & $\textbf{100}$ & $\textbf{100}$ & $\textbf{100}$ & $\textbf{100}$ \\ 
# Birth weight ratio & $\textbf{100}$ & $\textbf{100}$ & $\textbf{98.8}$ & $\textbf{100}$ & $\textbf{100}$ & $\textbf{100}$ & $42$ & $1.8$ \\ 
# Birth weight & $0$ & $\textbf{63.6}$ & $0$ & $30.6$ & $0$ & $\textbf{56.2}$ & $0$ & $39.6$ \\ 
# Gestational age & $24$ & $41.6$ & $0.8$ & $\textbf{82.2}$ & $9.2$ & $1$ & $43.2$ & $27.6$ \\ 
# Sex & $42.6$ & $\textbf{82.6}$ & $\textbf{88.4}$ & $\textbf{98.4}$ & $0$ & $0.4$ & $\textbf{80.6}$ & $3.8$ \\ 
# Mother edu. \textless = 16 & $\textbf{67.8}$ & $\textbf{99.6}$ & $\textbf{76.4}$ & $\textbf{100}$ & $33.2$ & $\textbf{72.2}$ & $18.6$ & $14.4$ \\ 
# Father edu. \textless = 16 & $8$ & $31.6$ & $12.8$ & $\textbf{76.2}$ & $1.8$ & $9.4$ & $11.4$ & $5$ \\ 
# 2 social benefits & $\textbf{93.4}$ & $\textbf{100}$ & $\textbf{91.4}$ & $\textbf{99.8}$ & $\textbf{78.4}$ & $\textbf{78.4}$ & $13.4$ & $4.2$ \\ 
# 3 social benefits & $\textbf{97.2}$ & $\textbf{100}$ & $\textbf{98.6}$ & $\textbf{100}$ & $\textbf{67.6}$ & $\textbf{77.6}$ & $13.2$ & $7.6$ \\ 
# 4 social benefits & $\textbf{56.8}$ & $\textbf{88.8}$ & $\textbf{83.6}$ & $\textbf{99.2}$ & $27.8$ & $27.4$ & $40.4$ & $12$ \\ 
# Mother age & $0$ & $6$ & $2.4$ & $99.8$ & $0.4$ & $12.2$ & $0$ & $11.6$ \\ 
# Cig. \textless  10 & $7.6$ & $63$ & $\textbf{56.6}$ & $\textbf{99.4}$ & $0$ & $0.4$ & $0.4$ & $0.4$ \\ 
# Cig. 10-19 & $0.2$ & $21.6$ & $31.4$ & $\textbf{94.4}$ & $0$ & $2.6$ & $1$ & $0.4$ \\ 
# Cig. \textgreater = 20 & $0$ & $14.2$ & $26.6$ & $\textbf{68.2}$ & $0$ & $10.2$ & $1.2$ & $1.6$ \\ 
# Socio economic status 2 & $\textbf{56.6}$ & $\textbf{55.8}$ & \textbf{}$\textbf{92.8}$ & $\textbf{98.2}$ & $0$ & $0.2$ & $0$ & $12.2$ \\ 
# Socio economic status 3 & $8.8$ & $27.8$ & $10.4$ & $\textbf{55}$ & $1.2$ & $3.4$ & $0.8$ & $0.8$ \\ 
# Socio economic status 4 & $\textbf{73.2}$ & $\textbf{100}$ & $\textbf{92.6}$ & $\textbf{100}$ & $\textbf{52}$ & $85.8$ & $1.2$ & $1.8$ \\ 
# Socio economic status 5 & $\textbf{64.4}$ & $\textbf{100}$ & $\textbf{92.6}$ & $\textbf{100}$ & $29.8$ & $\textbf{72.2}$ & $0$ & $2.6$ \\ 
# \hline \\[-1.8ex] 
# \end{tabular} 
# \end{table} 

########
#IQFULL
#########
#Create binary variable for the number of cigarettes (1 = none, 2= >0)
lbw_data_iqfull <- lbw_data
lbw_data_iqfull$mcig[lbw_data_iqfull$mcig != 1 & !is.na(lbw_data_iqfull$mcig)] = 2
lbw_data_iqfull$mcig <- factor(lbw_data_iqfull$mcig)
ini <- mice(lbw_data_iqfull, maxit = 0)
pred <- ini$predictorMatrix
pred[, c("iqfull","iqverb","iqperf","rcomp","rrate","racc","tomifull","code")] <- 0
imp_iqfull <- mice(lbw_data_iqfull, seed = 17, method=c("","","","","","","","pmm","pmm","pmm","logreg",
                                          "logreg","logreg","pmm","pmm","logreg","polr",""),
            predictorMatrix=pred, visitSequence = "monotone", m=500)
#Fit linear regression
fit_iqfull <- with(imp_iqfull, lm(iqfull ~ bw+rbw+sex+educage+benef+mcig+socstat))
summary(pool(fit_iqfull))
#R^2
pool.r.squared(fit_iqfull, adjusted=FALSE) #0.2697740
pool.r.squared(fit_iqfull, adjusted=TRUE)  #0.2091387

########
#IQverb
#########
fit_iqverb <- with(imp, lm(iqverb ~ rbw+ga+sex+educage+fed+benef+matage+mcig+socstat))
summary(pool(fit_iqverb))
#Compute R^2
pool.r.squared(fit_iqverb, adjusted=FALSE) #0.3148783
pool.r.squared(fit_iqverb, adjusted=TRUE)  #0.2379170

########
#IQperf
#########
fit_iqperf <- with(imp, lm(iqperf ~ benef+bw+educage+rbw+socstat))
summary(pool(fit_iqperf))
#Compute R^2
pool.r.squared(fit_iqperf, adjusted=FALSE) #0.1753846
pool.r.squared(fit_iqperf, adjusted=TRUE)  #0.1190427

########
#tomifull
#########
fit_tomifull <- with(imp, lm(tomifull ~ bw+ga)) 
summary(pool(fit_tomifull))
#Compute R^2
pool.r.squared(fit_tomifull, adjusted=FALSE) #0.03575897
pool.r.squared(fit_tomifull, adjusted=TRUE)  #0.02448129
#note that bw and ga are never missing -> it is the same as a liner regression
fit_tomifull_classic <- lm(tomifull ~ bw+ga, data=lbw_data)
summary(fit_tomifull_classic) #Ok, same results :)
