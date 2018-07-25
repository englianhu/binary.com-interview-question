forecastUSDJPY <- function(mbase, currency = 'JPY=X', ahead = 1, price = 'Cl') {
  
  source('function/calC.R')
  source('function/filterFX.R')
  
  forC_price <- suppressWarnings(
    calC(mbase, currency = currency, ahead = ahead, price = price))
  
  fxC <- data.frame(
    LatestDate.GMT = index(forC_price$latestPrice), 
    latestPrice = forC_price$latestPrice, 
    #ForecastDate.GMT = index(forC_price$forecastPrice), 
    ForecastDate.GMT = rownames(forC_price$forecastPrice), 
    Currency = forC_price$forecastPrice)
  
  rownames(fxC) <- NULL
  nm <- names(mbase) %>% 
    str_replace_all('.Open|.High|.Low|.Close|.Volume|.Adjusted', '') %>% 
    unique %>% 
    str_replace('\\.', '') %>% 
    .[nchar(.) == 6]
  
  if(price == 'Op') {
    eval(parse(text = paste0(
    'fxC %<>% dplyr::rename(Lst.Open = ', nm, ', Fct.Open = ', nm, '.1)')))
    
  } else if(price == 'Hi') {
    eval(parse(text = paste0(
    'fxC %<>% dplyr::rename(Lst.High = ', nm, ', Fct.High = ', nm, '.1)')))
    
  } else if(price == 'Lo') {
    eval(parse(text = paste0(
    'fxC %<>% dplyr::rename(Lst.Low = ', nm, ', Fct.Low = ', nm, '.1)')))
    
  } else if(price == 'Cl') {
    eval(parse(text = paste0(
    'fxC %<>% dplyr::rename(Lst.Close = ', nm, ', Fct.Close = ', nm, '.1)')))
    
  } else {
    stop("price = 'Op', price = 'Hi', price = 'Lo' or price = 'Cl'.")
  }
  
  #'@ if(nm == 'USD.JPY') {
  #'@   if(price == 'Op') fxC %<>% dplyr::rename(Lst.Open = USD.JPY, Fct.Open = USD.JPY.1)
  #'@   if(price == 'Hi') fxC %<>% dplyr::rename(Lst.High = USD.JPY, Fct.High = USD.JPY.1)
  #'@   if(price == 'Lo') fxC %<>% dplyr::rename(Lst.Low = USD.JPY, Fct.Low = USD.JPY.1)
  #'@   if(price == 'Cl') fxC %<>% dplyr::rename(Lst.Close = USD.JPY, Fct.Close = USD.JPY.1)
  #'@ }
  #'@ if(nm == 'USDJPY') {
  #'@   if(price == 'Op') fxC %<>% dplyr::rename(Lst.Open = USDJPY, Fct.Open = USDJPY.1)
  #'@   if(price == 'Hi') fxC %<>% dplyr::rename(Lst.High = USDJPY, Fct.High = USDJPY.1)
  #'@   if(price == 'Lo') fxC %<>% dplyr::rename(Lst.Low = USDJPY, Fct.Low = USDJPY.1)
  #'@   if(price == 'Cl') fxC %<>% dplyr::rename(Lst.Close = USDJPY, Fct.Close = USDJPY.1)
  #'@ }
  
  return(fxC)
}
