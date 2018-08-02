opt_arma <- function(mbase){
#ARMA Modeling minimum AIC value of `p,d,q`
  fit <- auto.arima(mbase)
  arimaorder(fit)
  }

