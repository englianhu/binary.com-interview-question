forecastUSDJPYHL <- function(mbase, .preCat = 'Op', .setPrice = 'Cl', 
                             currency = 'JPY=X', ahead = 1){
  fx1 <- forecastUSDJPY(mbase, currency = currency, ahead = ahead, price = .preCat)
  fx2 <- forecastUSDJPY(mbase, currency = currency, ahead = ahead, price = .setPrice)
  fxm <- merge(fx1, fx2, by = c('LatestDate.GMT', 'ForecastDate.GMT'))
  rm(fx1, fx2)
  
  fxm <- fxm[c(1, 3, 5, 2, 4, 6)]
  return(fxm)
}
