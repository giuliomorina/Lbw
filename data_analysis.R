setwd("C://Users//Utente 2//Documents//GitHub//Lbw")
lbw_data = read.csv("oxwaspLbw.csv")
bp_data = read.csv("oxwaspbp.csv", sep=" ")
GCSE_data = read.csv("oxwaspGCSE.csv")

attach(lbw_data)
summary(lbw_data)
table(code)
hist(iqfull)
hist(rcomp)
hist(rrate)
hist(racc)
#lbw_data[is.na(lbw_data$rcomp),20]
#lbw_data[is.na(lbw_data$rrate),20]
lbw_data[is.na(lbw_data$racc),20]
lbw_data[is.na(lbw_data$iqfull),20]
hist(tomifull)
lbw_data[is.na(lbw_data$tomifull),20]
hist(bw)
table(bw)
hist(rbw)
lbw_data[is.na(lbw_data$fed),20] == lbw_data[is.na(lbw_data$educage),20]
lbw_data[lbw_data$fed==9 & !is.na(lbw_data$fed),20] == lbw_data[lbw_data$educage==99 & !is.na(lbw_data$educage),20]
table(incm)
table(incf)

lbw_data[is.na(lbw_data$incm),20] == lbw_data[is.na(lbw_data$incf),20]
lbw_data[is.na(lbw_data$incm),20] == lbw_data[is.na(lbw_data$educage),20]
table(benef)

hist(mcig)
table(mcig)

table(socstat)
hist(socstat)
lbw_data[is.na(lbw_data$socstat),20] == lbw_data[is.na(lbw_data$mcig),20]

total_data = merge(lbw_data,bp_data,by="code")
data_1_3 = merge(lbw_data, GCSE_data, by="code")
total_data = merge(total_data,GCSE_data,by="code")
