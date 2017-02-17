# Import package for missing data analysis
library("VIM")
library("Amelia")
library("mice")
library("Hmisc")
library("hot.deck")

############################ Import the 3 datasets #################################

#setwd("C://Users//Utente 2//Documents//GitHub//Lbw")
lbw_data = read.csv("oxwaspLbw.csv")
bp_data = read.csv("oxwaspbp.csv", sep=" ")
GCSE_data = read.csv("oxwaspGCSE.csv")


##################### Clean and prepare lbw_data for analysis #######################

# Erase income variable - considered as not reliable
drops <- c("incf","incm")
lbw_data = lbw_data[,!(names(lbw_data) %in% drops)]

# Remove missing data in the response variable iqfull (MNAR - cannot do anything)
lbw_data = lbw_data[!is.na(lbw_data$iqfull),]

# Clean the variables educage and fed
lbw_data$educage[lbw_data$educage == 99] = NA
lbw_data$fed[lbw_data$fed == 9] = NA

# Make mother education (educage) and father education (fed) consistent
lbw_data$educage[lbw_data$educage > 16] = "Greater"
lbw_data$educage[lbw_data$educage <= 16] = "Smaller"
lbw_data$educage[lbw_data$educage == "Greater"] = 1
lbw_data$educage[lbw_data$educage == "Smaller"] = 2
lbw_data$educage = as.numeric(lbw_data$educage)


################################ Merge datasets ####################################

# Merge lbw data with bp data
data_1_2 = merge(lbw_data,bp_data,by="code")

# Merge lbw data with GCSE data
data_1_3 = merge(lbw_data, GCSE_data, by="code")

# Merge all 3 datasets keeping only common obs
total_data = merge(data_1_2,GCSE_data,by="code")


############################### Summarize datasets ###############################

# Summary of lbw dataset
attach(lbw_data)

summary(lbw_data)

hist(iqfull)
hist(rcomp)
hist(rrate)
hist(racc)
hist(tomifull)
hist(bw)
table(bw)
hist(rbw)
hist(mcig)
hist(socstat)

table(incm)
table(incf)
table(mcig)
table(socstat)
table(benef)

#lbw_data[is.na(lbw_data$rcomp),20]
#lbw_data[is.na(lbw_data$rrate),20]
lbw_data[is.na(lbw_data$racc),20]
lbw_data[is.na(lbw_data$iqfull),20]
lbw_data[is.na(lbw_data$tomifull),20]
lbw_data[is.na(lbw_data$fed),20] == lbw_data[is.na(lbw_data$educage),20]
lbw_data[lbw_data$fed==9 & !is.na(lbw_data$fed),20] == lbw_data[lbw_data$educage==99 & !is.na(lbw_data$educage),20]

lbw_data[is.na(lbw_data$incm),20] == lbw_data[is.na(lbw_data$incf),20]
lbw_data[is.na(lbw_data$incm),20] == lbw_data[is.na(lbw_data$educage),20]

lbw_data[is.na(lbw_data$socstat),20] == lbw_data[is.na(lbw_data$mcig),20]
lbw_data[is.na(lbw_data$mcig),20] == lbw_data[is.na(lbw_data$educage),20]



######################## Missing data plots (lbw data) with VIM #####################

# Missing data pattern
lbw_aggr = aggr(lbw_data, col=mdc(1:2), numbers=TRUE, prop = FALSE, sortVars=TRUE, labels=names(lbw_data), cex.axis=.7, gap=3, ylab=c("Frequency of missingness","Missingness Pattern"))

# Margin plot (scatter plot + box plots)

#marginplot(lbw_data[, c("benef", "educage", "socstat","mcig", "incm", "incf")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)
#marginplot(lbw_data[, c("benef", "educage")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)

# distribution of the educage for benefit missing and benefit not missing.
# no big difference : the fact that benef is missing does not depend on the educational level
marginplot(lbw_data[, c("educage", "benef")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)

marginplot(lbw_data[, c("educage", "socstat")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)

# The socstat for non observed educage seems to be higher thus socstat (only 9 NA) could be used to infer educage
marginplot(lbw_data[, c("socstat", "educage")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)
marginplot(lbw_data[, c("matage", "socstat")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)

# The mother age is higher when educage educage is missing 
marginplot(lbw_data[, c("matage", "educage")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)

# Difference in the number of cigarette when educage is missing.  
marginplot(lbw_data[, c("mcig", "educage")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)


# Matrix plot : no big correlation
matrixplot(lbw_data, interactive = F, sortby = "matage")
matrixplot(lbw_data, interactive = F, sortby = "ga")
matrixplot(lbw_data, interactive = F, sortby = "mcig")
matrixplot(lbw_data, interactive = F, sortby = "socstat")


