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

#http://stackoverflow.com/questions/14604439/plot-multiple-boxplot-in-one-graph
lbw_data_boxplot <- lbw_data
lbw_data_boxplot$dataset <- "Original"
lbw_data_boxplot <- subset(lbw_data_boxplot, select = -c(sex,educage, fed, benef, mcig, socstat, code)) #remove discrete variables
df.m1 <- melt(lbw_data_boxplot, id.var = "dataset")
df.m1 <- df.m1[!is.na(df.m1$value),]

lbw_data_boxplot <- lbw_data_casewise
lbw_data_boxplot$dataset <- "Casewise deletion"
lbw_data_boxplot <- subset(lbw_data_boxplot, select = -c(sex,educage, fed, benef, mcig, socstat, code))
df.m2 <- melt(lbw_data_boxplot, id.var = "dataset")

df.m <- rbind(df.m1,df.m2)
df.m$dataset <- factor(df.m$dataset)
df.m$value <- as.numeric(df.m$value)

p<-ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill = dataset))
p<-p + facet_wrap( ~ variable, scales="free")
p <- p + xlab("Variable") + ylab("Value") + ggtitle("Comparison between the original dataset and after performing casewise deletion")
p <- p + guides(fill=guide_legend(title="Legend"))
p

#Discrete variables
lbw_data_boxplot <- lbw_data
lbw_data_boxplot <- subset(lbw_data_boxplot, select = -c(iqfull,iqverb,iqperf,rcomp,rrate,racc,bw,rbw,ga,matage,code)) #remove continuous variables
summary(lbw_data_boxplot)

lbw_data_boxplot_casewise <- lbw_data_casewise
lbw_data_boxplot_casewise <- subset(lbw_data_boxplot_casewise, select = -c(iqfull,iqverb,iqperf,rcomp,rrate,racc,bw,rbw,ga,matage,code)) #remove continuous variables
summary(lbw_data_boxplot_casewise)

table(lbw_data_boxplot$sex)/sum(table(lbw_data_boxplot$sex))
table(lbw_data_boxplot_casewise$sex)/sum(table(lbw_data_boxplot_casewise$sex))

table(lbw_data_boxplot$educage)/sum(table(lbw_data_boxplot$educage))
table(lbw_data_boxplot_casewise$educage)/sum(table(lbw_data_boxplot_casewise$educage))

table(lbw_data_boxplot$fed)/sum(table(lbw_data_boxplot$fed))
table(lbw_data_boxplot_casewise$fed)/sum(table(lbw_data_boxplot_casewise$fed))

table(lbw_data_boxplot$benef)/sum(table(lbw_data_boxplot$benef))
table(lbw_data_boxplot_casewise$benef)/sum(table(lbw_data_boxplot_casewise$benef))

table(lbw_data_boxplot$mcig)/sum(table(lbw_data_boxplot$mcig))
table(lbw_data_boxplot_casewise$mcig)/sum(table(lbw_data_boxplot_casewise$mcig))

table(lbw_data_boxplot$socstat)/sum(table(lbw_data_boxplot$socstat))
table(lbw_data_boxplot_casewise$socstat)/sum(table(lbw_data_boxplot_casewise$socstat))


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
