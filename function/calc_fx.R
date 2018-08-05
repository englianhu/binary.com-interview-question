calc_fx <- memoise(function(mbase, currency = 'JPY=X', ahead = 1, price = 'Cl') {
  
  ## Using "memoise" to automatically cache the results
  ## http://rpubs.com/englianhu/arma-order-for-garch
  source('function/filterFX.R')
  #'@ source('function/armaSearch.R') #old optimal arma p,q value searching, but no d value. 
  source('function/opt_arma.R') #rename the function best.ARMA()
  
  mbase = suppressWarnings(filterFX(mbase, currency = currency, price = price))
  armaOrder = opt_arma(mbase)
  
  ## Set arma order for `p, d, q` for GARCH model.
  #'@ https://stats.stackexchange.com/questions/73351/how-does-one-specify-arima-p-d-q-in-ugarchspec-for-ugarchfit-in-rugarch
  spec = ugarchspec(
    variance.model = list(
      model = 'gjrGARCH', garchOrder = c(1, 1), 
      submodel = NULL, external.regressors = NULL, 
      variance.targeting = FALSE), 
    mean.model = list(
      armaOrder = armaOrder[c(1, 3)], #set arma order for `p` and `q`.
      include.mean = TRUE, archm = FALSE, 
      archpow = 1, arfima = TRUE, #set arima = TRUE
      external.regressors = NULL, 
      archex = FALSE), 
    fixed.pars = list(arfima = armaOrder[2]), #set fixed.pars for `d` value
    distribution.model = 'snorm')
  
  fit = ugarchfit(spec, mbase, solver = 'hybrid')
  
  fc = ugarchforecast(fit, n.ahead = ahead)
  #res = xts::last(attributes(fc)$forecast$seriesFor)
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
  
  tmp = list(latestPrice = latestPrice, forecastPrice = res, 
             AIC = infocriteria(fit))
  return(tmp)
  })

