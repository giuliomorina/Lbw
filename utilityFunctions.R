computeRsquared <- function(coef,X, Y) {
  pred_val <- X%*%coef
  p <- ncol(X)
  n <- nrow(X)
  SStot <- sum((Y-mean(Y))^2)
  SSres <- sum((Y-pred_val)^2)
  R2 <- 1-SSres/SStot
  adj_R2 <- 1-(1-R2)*(n-1)/(n-p-1)
  return(list(R2 = R2, adj_R2 = adj_R2))
}