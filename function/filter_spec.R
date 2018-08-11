filter_spec <- function(mbase, .currency = 'JPY=X', .include.Op = TRUE, 
                        .Cl.only = FALSE) {
  
  ## load libraries and functions.
  if (!require('BBmisc')) install.packages('BBmisc')
  require('BBmisc')
  
  pkgs <- c('quantmod', 'forecast', 'plyr', 'dplyr', 'magrittr')
  suppressPackageStartupMessages(load_pkg(pkgs))
  
  source('function/filterFX.R')
  source('function/opt_arma.R')
  
  ## verify data type.
  if(!is.xts(mbase)) mbase <- xts(mbase[, -1], order.by = mbase$Date)
  mbase %<>% na.omit
  
  ## search optimal arma order.
  if (.include.Op == TRUE) {
    mb.Op <- suppressWarnings(filterFX(mbase, currency = .currency, price = 'Op'))
    armaOrder.Op <- opt_arma(mb.Op)
    
  } else {
    armaOrder.Op <- NULL
  }
  
  if (.Cl.only == FALSE) {
    mb.Hi <- suppressWarnings(filterFX(mbase, currency = .currency, price = 'Hi'))
    armaOrder.Hi <- opt_arma(mb.Hi)
    
    mb.Lo <- suppressWarnings(filterFX(mbase, currency = .currency, price = 'Lo'))
    armaOrder.Lo <- opt_arma(mb.Lo)
    
    mb.Cl <- suppressWarnings(filterFX(mbase, currency = .currency, price = 'Cl'))
    armaOrder.Cl <- opt_arma(mb.Cl)
    
    armaOrder <- list(armaOrder.Op = armaOrder.Op, armaOrder.Hi = armaOrder.Hi, 
                      armaOrder.Lo = armaOrder.Lo, armaOrder.Cl = armaOrder.Cl) %>% 
      filterNull

  } else {
    armaOrder.Hi <- NULL
    armaOrder.Lo <- NULL
    
    if (ncol(mbase) > 1) {
      mbase %<>% na.omit
      mb.Cl <- llply(mbase, function(x) 
        suppressWarnings(filterFX(x, currency = .currency, price = 'Cl')))
      armaOrder.Cl <- llply(mb.Cl, opt_arma)
      armaOrder <- armaOrder.Cl
      
    } else {
      mb.Cl <- suppressWarnings(filterFX(mbase, currency = .currency, price = 'Cl'))
      armaOrder.Cl <- opt_arma(mb.Cl)
      
      armaOrder <- list(armaOrder.Op = armaOrder.Op, armaOrder.Hi = armaOrder.Hi, 
                        armaOrder.Lo = armaOrder.Lo, armaOrder.Cl = armaOrder.Cl) %>% 
        filterNull
    }
  }
  
  speclist <- llply(armaOrder, function(x) {
    spec = ugarchspec(
    variance.model = list(
      model = 'gjrGARCH', garchOrder = c(1, 1), 
      submodel = NULL, external.regressors = NULL, 
      variance.targeting = FALSE), 
    mean.model = list(
      armaOrder = x[c(1, 3)], 
      include.mean = TRUE, archm = FALSE, 
      archpow = 1, arfima = TRUE, 
      external.regressors = NULL, 
      archex = FALSE), 
    fixed.pars = list(arfima = x[2]), 
    distribution.model = 'snorm')
  })
  
  return(speclist)
}
