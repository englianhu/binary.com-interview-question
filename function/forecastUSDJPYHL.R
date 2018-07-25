forecastUSDJPYHL <- function(mbase, .preCat = 'Op', .preCat2 = NULL, 
                             .setPrice = 'Cl', currency = 'JPY=X', ahead = 1){
  
  source('function/forecastUSDJPY.R')
  source('function/calC.R')
  source('function/filterFX.R')
  
  if(!is.xts(mbase)) mbase <- as.xts(dplyr::select(
    mbase, -Date), order.by = mbase$Date)
  
  pr <- c('Op', 'Hi', 'Lo', 'Cl')
  if(.preCat %in% pr) {
    .preCat <- .preCat
  } else {
    stop(".preCat = 'Op', .preCat = 'Hi', .preCat = 'Lo', .preCat = 'Cl'.")
  }
  
  if (is.null(.preCat2)) {
    .preCat2 <- .preCat2
    
  } else if (.preCat2 %in% pr) {
    .preCat2 <- .preCat2
    
  } else {
    stop(".preCat = 'Op', .preCat = 'Hi', .preCat = 'Lo', .preCat = 'Cl'.")
  }
  
  fx1 <- forecastUSDJPY(mbase, currency = currency, 
                        ahead = ahead, price = .preCat)
  
  fx3 <- forecastUSDJPY(mbase, currency = currency, 
                        ahead = ahead, price = .setPrice)
  
  if (!is.null(.preCat2) && .preCat2 %in% pr) {
    fx2 <- forecastUSDJPY(mbase, currency = currency, 
                          ahead = ahead, price = .preCat2)
    
    fx3 <- merge(fx2, fx3, by = c('LatestDate.GMT', 'ForecastDate.GMT'))
  }
  
  fxm <- merge(fx1, fx3, by = c('LatestDate.GMT', 'ForecastDate.GMT'))
  suppressWarnings(rm(fx1, fx2, fx3))
  
  if (ncol(fxm) == 6) {
    fxm <- fxm[c(1, 3, 5, 2, 4, 6)]
  } else {
    fxm <- fxm[c(1, 3, 5, 7, 2, 4, 6, 8)]
  }
  
  return(fxm)
}
