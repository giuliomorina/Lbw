computeRsquared <- function(coefficients,X, Y) {
  pred_val <- X%*%coefficients
  p <- ncol(X)
  n <- nrow(X)
  SStot <- sum((Y-mean(Y))^2)
  SSres <- sum((Y-pred_val)^2)
  R2 <- 1-SSres/SStot
  adj_R2 <- 1-(1-R2)*(n-1)/(n-p-1)
  return(list(R2 = R2, adj_R2 = adj_R2))
}

multipleImputationRsquared <- function(coef_im,list_X, Y) {
  #SEE pool.r.squared
  #list_X is a list whose elemnts are the imputed datasets
  #This function is based on http://www.ats.ucla.edu/stat/stata/faq/mi_r_squared.htm
  # z <- numeric(length(list_X))
  # adj_z <- numeric(length(list_X))
  # counter <- 1
  # for (X in list_X) {
  #   #Compute R^2 and adjusted R^2
  #   init_R_adjR <- computeRsquared(coef_im,X,Y)
  #   #Compute z
  #   z[counter] <- atanh(sqrt(init_R_adjR$R2))
  #   adj_z[counter] <- atanh(sqrt(init_R_adjR$adj_R2))
  #   counter <- counter+1
  # }
  # R2 <- tanh(mean(z))^2
  # adj_R2 <- tanh(mean(adj_z))^2
  # return(list(R2 = R2, adj_R2 = adj_R2))
}
