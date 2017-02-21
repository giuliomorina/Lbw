##############################
##### CASEWISE DELETION ######
##############################
#Import clean dataset
rm(list=ls())
library(glmnet)
library(leaps)
library(xtable)
library(stargazer)
require(ggplot2)

source("MissingDataAnalysis.R")

#Remove all rows where there are NAs
lbw_data_casewise = lbw_data[complete.cases(lbw_data),] #107 observations

#################################################################
######################## Boxplot to compare #####################
#################################################################

lbw_data_boxplot <- lbw_data
lbw_data_boxplot$missing <- 1
lbw_data_boxplot[complete.cases(lbw_data),"missing"] <- 0
lbw_data_boxplot$missing <- factor(lbw_data_boxplot$missing)

ggplot(data = lbw_data_boxplot, aes(x=missing, y=iqfull)) + geom_boxplot()
ggplot(data = lbw_data_boxplot, aes(x=missing, y=iqverb)) + geom_boxplot()
ggplot(data = lbw_data_boxplot, aes(x=missing, y=iqperf)) + geom_boxplot()
ggplot(data = lbw_data_boxplot, aes(x=missing, y=tomifull)) + geom_boxplot() #Totally different distribution!


#################################################################
############# Check response variables distribution #############
#################################################################

hist(lbw_data_casewise$iqfull, prob=TRUE) 
lines(density(lbw_data_casewise$iqfull, na.rm=TRUE)) #Seems normal
shapiro.test(lbw_data_casewise$iqfull) #0.2365 -> do not reject!

hist(lbw_data_casewise$iqverb, prob=TRUE) 
lines(density(lbw_data_casewise$iqverb, na.rm=TRUE)) #Seems skewed to the right
shapiro.test(lbw_data_casewise$iqverb) #0.08164 -> do not reject!

hist(lbw_data_casewise$iqperf, prob=TRUE) 
lines(density(lbw_data_casewise$iqperf, na.rm=TRUE)) #Seems normal
shapiro.test(lbw_data_casewise$iqperf) #0.9048 -> do not reject!

hist(lbw_data_casewise$tomifull, prob=TRUE) 
lines(density(lbw_data_casewise$tomifull, na.rm=TRUE)) #Horrible
shapiro.test(lbw_data_casewise$tomifull) #1.188e-05 -> reject!

##############################################################
############## Ordered logit model ###########################
##############################################################

#http://www.ats.ucla.edu/stat/r/dae/ologit.htm
lbw_data_casewise$tomifull <- ordered(floor(lbw_data_casewise$tomifull)) #Remove .5 and floor them
fit_ordered <- polr(tomifull ~ bw+rbw+ga+sex+educage+fed+benef+matage+mcig+socstat, data = lbw_data_casewise, Hess=TRUE)
summary(fit_ordered)
exp(coef(fit_ordered)) #odds ratio

#Test proportional odds assumption
