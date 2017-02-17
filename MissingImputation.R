# Import package for missing data analysis
library("VIM")
library("Amelia")
library("mice")
library("Hmisc")
library("hot.deck")

source("MissingDataAnalysis.R")

# Hot deck imputation base on Cranmer and Gill (2012) on lbw_data
# Notice that this package account for discrete data but not for their ordinal nature  

# lbw_missing = lbw_data[,c("educage","fed","benef","matage","mcig","socstat", "code")]
# is.discrete(lbw_missing) #verify which var are considered discrete 
# hot_deck_imputation = hot.deck(lbw_missing, m=5,  method = "p.draw", IDvars = c("code"))$data
# lbw_missing_imputation = as.data.frame(hot_deck_imputation$data)

# Verify which var are considered discrete. The cut off need to be 7 so that ga is considered contin.
is.discrete(lbw_data, cutoff=7) 

# Impute the categorical values using hot deck and the continuous using MI
# Produce 5 datasets used to run 5 regressions
lbw_hot_deck_imputation = hot.deck(lbw_data, m=5,  method = "p.draw", cutoff = 7, impContinuous = "mice", IDvars = c("code"))
lbwImputed = lbw_hot_deck_imputation$data


# Hot deck imputation base on Cranmer and Gill (2012) on GCSE_subset data
GCSE_hot_deck_imputation = hot.deck(GCSE_subset_clean, m=5,  method = "best.cell", IDvars = c("code"))
GCSE_1Data = as.data.frame(GCSE_hot_deck_imputation[[1]][1])
GCSE_2Data = as.data.frame(GCSE_hot_deck_imputation[[1]][2])
GCSE_3Data = as.data.frame(GCSE_hot_deck_imputation[[1]][3])
GCSE_4Data = as.data.frame(GCSE_hot_deck_imputation[[1]][4])
GCSE_5Data = as.data.frame(GCSE_hot_deck_imputation[[1]][5])

GCSE_1Data$grade = as.integer(rowMeans(GCSE_1Data[,c(2,3,4)]))
GCSE_2Data$grade = as.integer(rowMeans(GCSE_2Data[,c(2,3,4)]))
GCSE_3Data$grade = as.integer(rowMeans(GCSE_3Data[,c(2,3,4)]))
GCSE_4Data$grade = as.integer(rowMeans(GCSE_4Data[,c(2,3,4)]))
GCSE_5Data$grade = as.integer(rowMeans(GCSE_5Data[,c(2,3,4)]))

# Need to 
# The idea could be to match the lbw dataset with the 5 datasets 
# obtained throught the hot deck imputation in such a way that only 
# observation for which grade is available (new Y) are kept. Then estimate the 
# coefficient and aggregate.

# Same thing could be done with Amelia (other imputation method). Jack is doing that. 

# Try to impute using the function BBPMM in the package 
# BaBooN :Imputation through Bayesian Bootstrap Predictive Mean

# Question: How can we compare the performance of different imputation method?
# Maybe we can see if, using different imputation methods, we get stable results
# or results that are close to the one in the papers
