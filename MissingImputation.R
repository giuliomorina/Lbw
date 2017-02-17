# Import package for missing data analysis
library("VIM")
library("Amelia")
library("mice")
library("Hmisc")
library("hot.deck")

source("MissingDataAnalysis.R")



# Hot deck imputation base on Cranmer and Gill (2012) on lbw_data

# lbw_missing = lbw_data[,c("educage","fed","benef","matage","mcig","socstat", "code")]
# is.discrete(lbw_missing) #verify which var are considered discrete 
# hot_deck_imputation = hot.deck(lbw_missing, m=5,  method = "p.draw", IDvars = c("code"))$data
# lbw_missing_imputation = as.data.frame(hot_deck_imputation$data)

# Verify which var are considered discrete. The cut off need to be 7 so that ga is considered contin.
is.discrete(lbw_data, cutoff=7) 

# Impute the categorical values using hot deck and the continuous using MI
# Produce 5 datasets used to run 5 regressions
hot_deck_imputation = hot.deck(lbw_data, m=5,  method = "p.draw", cutoff = 7, impContinuous = "mice", IDvars = c("code"))


# Hot deck imputation base on Cranmer and Gill (2012) on GCSE_subset data


GCSE_hot_deck_imputation = hot.deck(GCSE_subset_clean, m=5,  method = "best.cell", IDvars = c("code"))
