boxplot(lbw_data$bw)
boxplot(lbw_data$rbw)
boxplot(lbw_data$ga)
boxplot(lbw_data$matage)

lbw_data_amelia <- amelia(lbw_data, idvars = c(1:7,18), noms = c(11:13), ords = c(14,16,17), p2s = 0)
