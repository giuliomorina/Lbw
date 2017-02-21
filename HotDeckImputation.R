 rm(list=ls())
library(mice)
library(leaps)
library(glmnet)
library(plyr)
library("VIM")
library("Amelia")
library("Hmisc")
library("hot.deck")
source("MissingDataAnalysis.R")
source("utilityFunctions.R")

m<-500
lbw_hot_deck_imputation = hot.deck(lbw_data, m=m,  method = "p.draw", cutoff = 7, impContinuous = "mice", 
                                   IDvars = c("code","iqfull","iqverb","iqperf","rcomp","rrate","racc","tomifull"))
lbwImputed = lbw_hot_deck_imputation$data

#########
#Iqfull
#########
lm_list <- vector("list")
for (i in 1:m) {
  lm_list[[i]] = lm(iqfull ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat,data = lbwImputed[[i]])
}
lm_iqfull <- as.mira(lm_list) #Convert it so that mice can handle it
summary(pool(lm_iqfull))
summary_iqfull = as.matrix(summary(pool(lm_iqfull)))[,c(1,5)]
colnames(summary_iqfull) = c("coefficient","p-value")
#Compute R^2
pool.r.squared(lm_iqfull, adjusted=FALSE) #0.
pool.r.squared(lm_iqfull, adjusted=TRUE)  #0.
r_adjusted_iqfull = as.matrix(pool.r.squared(lm_iqfull, adjusted=TRUE))[1,1]

#########
#Iqverb
#########
lm_list <- vector("list")
for (i in 1:m) {
  lm_list[[i]] = lm(iqverb ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat,data = lbwImputed[[i]])
}
lm_iqverb <- as.mira(lm_list) #Convert it so that mice can handle it
summary(pool(lm_iqverb))
summary_iqverb = as.matrix(summary(pool(lm_iqverb)))[,c(1,5)]
colnames(summary_iqverb) = c("coefficient","p-value")
#Compute R^2
pool.r.squared(lm_iqverb, adjusted=FALSE) #0.
pool.r.squared(lm_iqverb, adjusted=TRUE)  #0.
r_adjusted_iqverb = as.matrix(pool.r.squared(lm_iqverb, adjusted=TRUE))[1,1]

#########
#Iqperf
#########
lm_list <- vector("list")
for (i in 1:m) {
  lm_list[[i]] = lm(iqperf ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat,data = lbwImputed[[i]])
}
lm_iqperf <- as.mira(lm_list) #Convert it so that mice can handle it
summary(pool(lm_iqperf))
summary_iqperf = as.matrix(summary(pool(lm_iqperf)))[,c(1,5)]
colnames(summary_iqperf) = c("coefficient","p-value")
#Compute R^2
pool.r.squared(lm_iqperf, adjusted=FALSE) #0.
pool.r.squared(lm_iqperf, adjusted=TRUE)  #0.
r_adjusted_iqperf = as.matrix(pool.r.squared(lm_iqperf, adjusted=TRUE))[1,1]
r_NONadjusted_iqperf = as.matrix(pool.r.squared(lm_iqperf, adjusted=FALSE))[1,1]

#########
#Tomifull
#########
lm_list <- vector("list")
for (i in 1:m) {
  lm_list[[i]] = lm(tomifull ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat,data = lbwImputed[[i]])
}
lm_tomifull <- as.mira(lm_list) #Convert it so that mice can handle it
summary(pool(lm_tomifull))
summary_tomifull = as.matrix(summary(pool(lm_tomifull)))[,c(1,5)]
colnames(summary_tomifull) = c("coefficient","p-value")
#Compute R^2
pool.r.squared(lm_tomifull, adjusted=FALSE) #0.
pool.r.squared(lm_tomifull, adjusted=TRUE)  #0.
r_NONadjusted_tomifull = as.matrix(pool.r.squared(lm_tomifull, adjusted=FALSE))[1,1]
r_adjusted_tomifull = 1 - ((1-r_NONadjusted_tomifull)*((171-1)/(174-17-1)))

##################### Tables for report ###############

hot_deck_results = cbind(summary_iqfull, summary_iqverb, summary_iqperf, summary_tomifull)
stargazer(hot_deck_results,
          type = "latex", 
          dep.var.labels  = c("Full IQ", "Verbal IQ", "Performance IQ", "TOMI"),
          dep.var.caption = "",
          ci = FALSE,
          title = "Linear regression with multiple hot deck imputation",
          no.space = TRUE,
          omit.stat=c("f", "ser"))
