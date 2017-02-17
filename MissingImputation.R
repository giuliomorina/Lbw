# Import package for missing data analysis
library("VIM")
library("Amelia")
library("mice")
library("Hmisc")
library("hot.deck")

source("MissingDataAnalysis.R")




# Hot deck imputation

# lbw_missing = lbw_data[,c("educage","fed","benef","matage","mcig","socstat", "code")]
# is.discrete(lbw_missing) #verify which var are considered discrete 
# hot_deck_imputation = hot.deck(lbw_missing, m=5,  method = "p.draw", IDvars = c("code"))$data
# lbw_missing_imputation = as.data.frame(hot_deck_imputation$data)

#verify which var are considered discrete. The cut off need to be 7 so that ga is considered contin.
is.discrete(lbw_data, cutoff=7) 
hot_deck_imputation = hot.deck(lbw_data, m=5,  method = "p.draw", cutoff = 7, impContinuous = "mice", IDvars = c("code"))
