forecastUSDJPYHL <- function(mbase, .preCat = 'Op', .preCat2 = NULL, 
                             .preCat3 = NULL, .preCat4 = NULL, 
							 currency = 'JPY=X', ahead = 1){
  
  source('function/forecastUSDJPY.R')
  
  pr <- c('Op', 'Hi', 'Lo', 'Cl')
  
  if (!is.xts(mbase)) mbase <- as.xts(dplyr::select(
    mbase, -Date), order.by = mbase$Date)
  
  if (.preCat %in% pr) {
    .preCat <- .preCat
    
  } else {
    stop(".preCat = 'Op', .preCat = 'Hi', .preCat = 'Lo', .preCat = 'Cl'.")
  }
  
  if (is.null(.preCat2)) {
    .preCat2 <- .preCat2
    
  } else if (.preCat2 %in% pr) {
    .preCat2 <- .preCat2
    
  } else {
    stop(".preCat2 = 'Op', .preCat2 = 'Hi', .preCat2 = 'Lo', .preCat2 = 'Cl'.")
  }
  
  if (is.null(.preCat3)) {
    .preCat3 <- .preCat3
    
  } else if (.preCat3 %in% pr) {
    .preCat3 <- .preCat3
    
  } else {
    stop(".preCat3 = 'Op', .preCat3 = 'Hi', .preCat3 = 'Lo', .preCat3 = 'Cl'.")
  }
  if (is.null(.preCat4)) {
    .preCat4 <- .preCat4
    
  } else if (.preCat4 %in% pr) {
    .preCat4 <- .preCat4
    
  } else {
    stop(".preCat4 = 'Op', .preCat4 = 'Hi', .preCat4 = 'Lo', .preCat4 = 'Cl'.")
  }
  
  
  ## forecast data.
  fx1 <- forecastUSDJPY(mbase, currency = currency, 
                        ahead = ahead, price = .preCat)
  
  if (!is.null(.preCat2) && .preCat2 %in% pr) {
    fx2 <- forecastUSDJPY(mbase, currency = currency, 
                          ahead = ahead, price = .preCat2)
    
    fx1 <- merge(fx1, fx2, by = c('LatestDate.GMT', 'ForecastDate.GMT'))
	  rm(fx2)
  }
  
  if (!is.null(.preCat3) && .preCat3 %in% pr) {
    fx3 <- forecastUSDJPY(mbase, currency = currency, 
                          ahead = ahead, price = .preCat3)
    
    fx1 <- merge(fx1, fx3, by = c('LatestDate.GMT', 'ForecastDate.GMT'))
	  rm(fx3)
  }
  
  if (!is.null(.preCat4) && .preCat4 %in% pr) {
    fx4 <- forecastUSDJPY(mbase, currency = currency, 
                          ahead = ahead, price = .preCat4)
    
    fx1 <- merge(fx1, fx4, by = c('LatestDate.GMT', 'ForecastDate.GMT'))
	  rm(fx4)
  }
  
  if (ncol(fx1) == 6) {
    fx1 <- fx1[c(1, 3, 5, 2, 4, 6)]
  } else if (ncol(fx1) == 8) {
    fx1 <- fx1[c(1, 3, 5, 7, 2, 4, 6, 8)]
  } else if (ncol(fx1) == 10) {
    fx1 <- fx1[c(1, 3, 5, 7, 9, 2, 4, 6, 8, 10)]
  } else {
    fx1 <- fx1[c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12)]
  }
  
  return(fx1)
}
