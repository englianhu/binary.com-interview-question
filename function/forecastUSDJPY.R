forecastUSDJPY <- function(mbase, currency = 'JPY=X', ahead = 1, price = 'Cl') {
  forC.USDJPY <- calC(mbase, currency = currency, ahead = ahead, price = price)
  
  fxC <- data.frame(
    LatestDate.GMT = index(forC.USDJPY$latestPrice), 
    latestPrice = forC.USDJPY$latestPrice, 
    #ForecastDate.GMT = index(forC.USDJPY$forecastPrice), 
    ForecastDate.GMT = rownames(forC.USDJPY$forecastPrice), 
    Currency = forC.USDJPY$forecastPrice)
  
  rownames(fxC) <- NULL
  nm <- names(mbase) %>% 
    str_replace_all('.Open|.High|.Low|.Close|.Volume|.Adjusted', '') %>% unique
  
  if(nm == 'USD.JPY') {
    if(price == 'Op') fxC %<>% dplyr::rename(Lst.Open = USD.JPY, Fct.Open = USD.JPY.1)
    if(price == 'Hi') fxC %<>% dplyr::rename(Lst.High = USD.JPY, Fct.High = USD.JPY.1)
    if(price == 'Lo') fxC %<>% dplyr::rename(Lst.Low = USD.JPY, Fct.Low = USD.JPY.1)
    if(price == 'Cl') fxC %<>% dplyr::rename(Lst.Close = USD.JPY, Fct.Close = USD.JPY.1)
  }
  if(nm == 'USDJPY') {
    if(price == 'Op') fxC %<>% dplyr::rename(Lst.Open = USDJPY, Fct.Open = USDJPY.1)
    if(price == 'Hi') fxC %<>% dplyr::rename(Lst.High = USDJPY, Fct.High = USDJPY.1)
    if(price == 'Lo') fxC %<>% dplyr::rename(Lst.Low = USDJPY, Fct.Low = USDJPY.1)
    if(price == 'Cl') fxC %<>% dplyr::rename(Lst.Close = USDJPY, Fct.Close = USDJPY.1)
  }
  
  return(fxC)
}
