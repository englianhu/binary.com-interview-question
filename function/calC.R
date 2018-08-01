calC <- memoise(function(mbase, currency = 'JPY=X', ahead = 1, price = 'Cl') {
  
  # Using "memoise" to automatically cache the results
  source('function/filterFX.R')
  source('function/armaSearch.R')
  mbase = suppressWarnings(filterFX(mbase, currency = currency, price = price))
  
  armaOrder = suppressWarnings(armaSearch(mbase))
  armaOrder %<>% dplyr::filter(AIC == min(AIC)) %>% .[c('p', 'q')] %>% unlist
  
  spec = ugarchspec(
    variance.model = list(
      model = 'gjrGARCH', garchOrder = c(1, 1), 
      submodel = NULL, external.regressors = NULL, 
      variance.targeting = FALSE), 
    mean.model = list(
      armaOrder = armaOrder, 
      include.mean = TRUE, archm = FALSE, 
      archpow = 1, arfima = FALSE, 
      ## https://stats.stackexchange.com/questions/73351/how-does-one-specify-arima-p-d-q-in-ugarchspec-for-ugarchfit-in-rugarch?answertab=votes#tab-top
      ## https://d.cosx.org/d/2689-2689/9
      external.regressors = NULL, 
      archex = FALSE), 
    distribution.model = 'snorm')
  fit = ugarchfit(spec, mbase, solver = 'hybrid')
  fc = ugarchforecast(fit, n.ahead = ahead)
  res = tail(attributes(fc)$forecast$seriesFor, 1)
  colnames(res) = names(mbase)
  latestPrice = tail(mbase, 1)
  
  #----
  ## count the number of days to forecast.
  #dy = ifelse(weekdays(index(latestPrice)) %in% wd[1:4], 1, 2)
  #if(weekdays(index(latestPrice)) %in% wd[c(1:3, 7)]) {
  #  dy <- 1
  #} else if(weekdays(index(latestPrice)) %in% wd[6]) {
  #  dy <- 2
  #} else if(weekdays(index(latestPrice)) %in% wd[4:5]) {
  #  dy <- 3
  #} else {
  #  stop('Weekdays must be within Monday to Sunday.')
  #}
  #----
  #forDate = latestPrice %>% index + days(dy)
  
  ## straighly use today('GMT') since last date will be the last 
  ##   trading day we get from getSymbols(), therefore the next 
  ##   trading day will be today('GMT').
  #'@ forDate = as.Date(today('GMT'))
  
  #rownames(res) <- as.character(forDate)
  latestPrice <- xts(latestPrice)
  #res <- as.xts(res)
  
  tmp = list(latestPrice = latestPrice, forecastPrice = res)
  return(tmp)
})
