simGarch <- function(mbase, 
                     .variance.model = list(model = 'sGARCH', garchOrder = c(1, 1), 
                                            .sub.fGarch = NULL, external.regressors = NULL, 
                                            variance.targeting = FALSE), 
                     .mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, archm = FALSE, 
                                       archpow = 1, arfima = FALSE, external.regressors = NULL, 
                                       archex = FALSE), 
                     .dist.model = 'norm', start.pars = list(), fixed.pars = list()){
  
  ## Data
  mbase <- USDJPY
  
  ## Models inside `rugarch` package.
  .variance.models <- c('sGARCH', 'fGARCH', 'eGARCH', 'gjrGARCH', 'apARCH', 'iGARCH', 'sGARCH', 'realGARCH')
  .sub.fGarchs <- c('GARCH', 'TGARCH', 'AVGARCH', 'NGARCH', 'NAGARCH', 'APARCH', 'GJRGARCH', 'ALLGARCH')
  .dist.models <- c('norm', 'snorm', 'std', 'sstd', 'ged', 'sged', 'nig', 'ghyp', 'jsu')
  
  llply(.variance.model, function(vm) {
    ft = ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(2, 1)), distribution = 'std')
    if(vm == 'fGARCH'){
      llply(.sub.fGarch, function(sub.vm){
        
      })
    }
    
  })
  
  
  return()
}