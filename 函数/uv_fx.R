uv_fx <- memoise(function(mbase, .model = 'gjrGARCH', .submodel = NULL, 
                            .dst = 'snorm', .solver = 'hybrid', 
                            currency = 'JPY=X', ahead = 1, 
                            price = 'Cl', .roll = FALSE, .cluster = FALSE) {
  
  ## Using "memoise" to automatically cache the results
  ## http://rpubs.com/englianhu/arma-order-for-garch
  source('function/filterFX.R')
  source('function/opt_arma.R') #rename the function best.ARMA()
  require('forecast')
  
  if(!is.xts(mbase)) mbase <- xts(mbase[, -1], order.by = mbase$Date)
  
  mbase = suppressWarnings(filterFX(mbase, currency = currency, price = price))
  armaOrder = opt_arma(mbase)
  
  ## Set arma order and arfima for `p, d, q` for GARCH model.
  #'@ https://stats.stackexchange.com/questions/73351/how-does-one-specify-arima-p-d-q-in-ugarchspec-for-ugarchfit-in-rugarch
  spec = ugarchspec(
    variance.model = list(
      model = .model, garchOrder = c(1, 1),    # Univariate Garch 2012 powerpoint.pdf
      submodel = .submodel, external.regressors = NULL, #   compares the garchOrder and 
      variance.targeting = FALSE),                 #   concludes garch(1,1) is the best fit.
    mean.model = list(
      armaOrder = armaOrder[c(1, 3)], #set arma order for `p` and `q`.
      include.mean = TRUE, archm = FALSE, 
      archpow = 1, arfima = TRUE, #set arfima = TRUE
      external.regressors = NULL, 
      archex = FALSE), 
    fixed.pars = list(arfima = armaOrder[2]), #set fixed.pars for `d` value
    distribution.model = .dst)
  
  if (.cluster == TRUE) {
    cl <- makePSOCKcluster(ncol(mbase))
  } else {
    cl <- NULL
  }
  
  fit = ugarchfit(spec, mbase, solver = .solver, cluster = cl)
  #'@ cat('step 1/2 ugarchfit done!\n')
  
  if (.roll == TRUE) {
    mod = ugarchroll(spec, data = mbase, solver = .solver, 
                     forecast.length = nrow(mbase), cluster = cl)
    #'@ cat('step 2/2 ugarchroll done!\n')
    
    return(report(mod, type = 'fpm'))
    
  } else {
    fc = ugarchforecast(fit, n.ahead = ahead, cluster = cl)
    #'@ cat('step 2/2 ugarchforecast done!\n')
    
    res = tail(attributes(fc)$forecast$seriesFor, 1)
    colnames(res) = names(mbase)
    latestPrice = tail(mbase, 1)
    
    latestPrice <- xts(latestPrice)
    #res <- as.xts(res)
    
    tmp = list(latestPrice = latestPrice, forecastPrice = res, 
               fit = fit, AIC = infocriteria(fit))
    return(tmp)
  }
})

