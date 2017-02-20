###################################
##### REPRODUCE PAPER RESULT ######
###################################

#Import clean dataset
rm(list=ls())
source("MissingDataAnalysis.R")

paper_data = lbw_data #initialization

#Convert benefit in binary variable
paper_data$benef = as.numeric(paper_data$benef)
paper_data$benef[paper_data$benef>1] = 2
paper_data$benef = factor(paper_data$benef, ordered = FALSE)
#Create missing category
levels(paper_data$benef) <- c(levels(paper_data$benef),3) 
paper_data$benef[is.na(paper_data$benef)] = 3
levels(paper_data$educage) <- c(levels(paper_data$educage),3) 
paper_data$educage[is.na(paper_data$educage)] = 3
levels(paper_data$fed) <- c(levels(paper_data$fed),3) 
paper_data$fed[is.na(paper_data$fed)] = 3
levels(paper_data$mcig) <- c(levels(paper_data$mcig),3) 
paper_data$mcig[is.na(paper_data$mcig)] = 3
levels(paper_data$socstat) <- c(levels(paper_data$socstat),3) 
paper_data$socstat[is.na(paper_data$socstat)] = 3

##########
#IQ FULL  
##########

#Remove rows where iqfull is missing
paper_data_iqfull <- paper_data[!is.na(paper_data$iqfull),]

#Linear regression (check if it works)
lm_casewise = lm(iqfull ~ . -iqverb -iqperf -rcomp -rrate -racc -tomifull -code,
                 data = paper_data_iqfull)
summary(lm_casewise)

#Best subset
fit_subset = regsubsets(iqfull ~ . -iqverb -iqperf -rcomp -rrate -racc -tomifull -code,
                        data = paper_data_iqfull, method = "exhaustive", nvmax = 20)
fit_subset_summary = summary(fit_subset)

fit_subset_summary$cp
plot(fit_subset,scale="Cp")

lm_subset = lm(iqfull ~ rbw+ga+educage+benef,
                 data = paper_data_iqfull)
summary(lm_subset)

##########
#IQ VERBAL
##########

#Remove rows where iqverb is missing
paper_data_iqverb <- paper_data[!is.na(paper_data$iqverb),]

#Best subset
fit_subset_iqverb = regsubsets(iqverb ~ . -iqfull -iqperf -rcomp -rrate -racc -tomifull -code,
                        data = paper_data_iqverb, method = "exhaustive", nvmax = 20)
fit_subset_summary_iqverb = summary(fit_subset_iqverb)

fit_subset_summary_iqverb$cp
plot(fit_subset_iqverb,scale="Cp")
lm_subset_iqverb = lm(iqverb ~ rbw+ga+educage+benef,
               data = paper_data_iqverb)
summary(lm_subset_iqverb)

##############
#IQPerformance
###############
#Remove rows where iqfull is missing
paper_data_iqperf <- paper_data[!is.na(paper_data$iqperf),]

#Best subset
fit_subset_iqperf = regsubsets(iqperf ~ . -iqfull -iqverb -rcomp -rrate -racc -tomifull -code,
                               data = paper_data_iqperf, method = "exhaustive", nvmax = 20)
fit_subset_summary_iqperf = summary(fit_subset_iqperf)

fit_subset_summary_iqperf$cp
plot(fit_subset_iqperf,scale="Cp")
lm_subset_iqperf = lm(iqperf ~ rbw+educage+benef,
                      data = paper_data_iqperf)
summary(lm_subset_iqperf)

###########
#Tomifull
###########

#Remove rows where iqfull is missing
paper_data_tomifull <- paper_data[!is.na(paper_data$tomifull),]

#Best subset
fit_subset_tomifull = regsubsets(tomifull ~ . -iqfull -iqverb -rcomp -rrate -racc -iqperf -code,
                               data = paper_data_tomifull, method = "exhaustive", nvmax = 20)
fit_subset_summary_tomifull = summary(fit_subset_tomifull)

fit_subset_summary_tomifull$cp
plot(fit_subset_tomifull,scale="Cp")
lm_subset_tomifull = lm(tomifull ~ bw+rbw+educage+benef+mcig,
                      data = paper_data_tomifull)
summary(lm_subset_tomifull)
