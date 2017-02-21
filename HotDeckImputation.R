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

m<-5
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
#Compute R^2
pool.r.squared(lm_iqfull, adjusted=FALSE) #0.3206815
pool.r.squared(lm_iqfull, adjusted=TRUE)  #0.2377976
