filter_spec <- function(mbase, .currency = 'ALL', .price_type = 'OHLC') {
  
  ## load libraries and functions.
  if (!require('BBmisc')) install.packages('BBmisc')
  require('BBmisc')
  
  pkgs <- c('quantmod', 'forecast', 'plyr', 'dplyr', 'magrittr')
  suppressPackageStartupMessages(lib(pkgs))
  
  source('function/filterFX.R')
  source('function/opt_arma.R')
  
  ## verify data type.
  if (.currency == 'ALL' && is.list(mbase) && all(sapply(mbase, function(x) !is.xts(x)))) {
    mbase <- llply(mbase, function(x) {
      y <- xts(x[, -1], order.by = x$Date)
      y %<>% na.omit
      })
  }
  
  if (.currency != 'ALL' && !is.list(mbase) && !is.xts(mbase)) {
    mbase <- xts(mbase[, -1], order.by = mbase$Date)
    mbase %<>% na.omit
  }
  
  cr_code <- c('AUDUSD=X', 'EURUSD=X', 'GBPUSD=X', 'CHF=X', 'CAD=X', 
               'CNY=X', 'JPY=X')
  
  #'@ names(cr_code) <- c('AUDUSD', 'EURUSD', 'GBPUSD', 'USDCHF', 'USDCAD', 
  #'@                     'USDCNY', 'USDJPY')
  names(cr_code) <- c('USDAUD', 'USDEUR', 'USDGBP', 'USDCHF', 
                      'USDCAD', 'USDCNY', 'USDJPY')
  cr_codes <- c('ALL', cr_code)
  
  if(!.currency %in% cr_codes) {
    stop(paste0('.currency must be in \'', paste(cr_codes, collapse = ', '), '\'.'))
  }
  
  .price_types <- c('OHLC', 'HLC', 'HL', 'C')
  if(!.price_type %in% .price_types) {
    stop(paste0('.price_type must be in \'', paste(.price_types, collapse = ', '), '\'.'))
  }
  
  ## search optimal arma order.
  if (.price_type == 'OHLC') {
    
    if (.currency == 'ALL') {
      armaOrder <- llply(cr_code, function(x) {
        cr <- grep(x, cr_code, value = TRUE)
        
        mb.Op <- suppressWarnings(filterFX(mbase[[names(cr)]], currency = x, price = 'Op'))
        armaOrder.Op <- opt_arma(mb.Op)
        
        mb.Hi <- suppressWarnings(filterFX(mbase[[names(cr)]], currency = x, price = 'Hi'))
        armaOrder.Hi <- opt_arma(mb.Hi)
        
        mb.Lo <- suppressWarnings(filterFX(mbase[[names(cr)]], currency = x, price = 'Lo'))
        armaOrder.Lo <- opt_arma(mb.Lo)
        
        mb.Cl <- suppressWarnings(filterFX(mbase[[names(cr)]], currency = x, price = 'Cl'))
        armaOrder.Cl <- opt_arma(mb.Cl)
        
        armaOrder <- list(armaOrder.Op = armaOrder.Op, armaOrder.Hi = armaOrder.Hi, 
                          armaOrder.Lo = armaOrder.Lo, armaOrder.Cl = armaOrder.Cl) %>% 
          filterNull
      })
      
    } else {
      mb.Op <- suppressWarnings(filterFX(mbase, currency = .currency, price = 'Op'))
      armaOrder.Op <- opt_arma(mb.Op)
      
      mb.Hi <- suppressWarnings(filterFX(mbase, currency = .currency, price = 'Hi'))
      armaOrder.Hi <- opt_arma(mb.Hi)
      
      mb.Lo <- suppressWarnings(filterFX(mbase, currency = .currency, price = 'Lo'))
      armaOrder.Lo <- opt_arma(mb.Lo)
      
      mb.Cl <- suppressWarnings(filterFX(mbase, currency = .currency, price = 'Cl'))
      armaOrder.Cl <- opt_arma(mb.Cl)
      
      armaOrder <- list(armaOrder.Op = armaOrder.Op, armaOrder.Hi = armaOrder.Hi, 
                        armaOrder.Lo = armaOrder.Lo, armaOrder.Cl = armaOrder.Cl) %>% 
        filterNull
    }
    
  } else if (.price_type == 'HLC') {
    
    if (.currency == 'ALL') {
      armaOrder <- llply(cr_code, function(x) {
        cr <- grep(x, cr_code, value = TRUE)
        
        armaOrder.Op <- NULL
        
        mb.Hi <- suppressWarnings(filterFX(mbase[[names(cr)]], currency = x, price = 'Hi'))
        armaOrder.Hi <- opt_arma(mb.Hi)
        
        mb.Lo <- suppressWarnings(filterFX(mbase[[names(cr)]], currency = x, price = 'Lo'))
        armaOrder.Lo <- opt_arma(mb.Lo)
        
        mb.Cl <- suppressWarnings(filterFX(mbase[[names(cr)]], currency = x, price = 'Cl'))
        armaOrder.Cl <- opt_arma(mb.Cl)
        
        armaOrder <- list(armaOrder.Op = armaOrder.Op, armaOrder.Hi = armaOrder.Hi, 
                          armaOrder.Lo = armaOrder.Lo, armaOrder.Cl = armaOrder.Cl) %>% 
          filterNull
      })
      
    } else {
      armaOrder.Op <- NULL
      
      mb.Hi <- suppressWarnings(filterFX(mbase, currency = .currency, price = 'Hi'))
      armaOrder.Hi <- opt_arma(mb.Hi)
      
      mb.Lo <- suppressWarnings(filterFX(mbase, currency = .currency, price = 'Lo'))
      armaOrder.Lo <- opt_arma(mb.Lo)
      
      mb.Cl <- suppressWarnings(filterFX(mbase, currency = .currency, price = 'Cl'))
      armaOrder.Cl <- opt_arma(mb.Cl)
      
      armaOrder <- list(armaOrder.Op = armaOrder.Op, armaOrder.Hi = armaOrder.Hi, 
                        armaOrder.Lo = armaOrder.Lo, armaOrder.Cl = armaOrder.Cl) %>% 
        filterNull
    }
    
  } else if (.price_type == 'HL') {
    
    if (.currency == 'ALL') {
      armaOrder <- llply(cr_code, function(x) {
        cr <- grep(x, cr_code, value = TRUE)
        
        armaOrder.Op <- NULL
        armaOrder.Cl <- NULL
        
        mb.Hi <- suppressWarnings(filterFX(mbase[[names(cr)]], currency = x, price = 'Hi'))
        armaOrder.Hi <- opt_arma(mb.Hi)
        
        mb.Lo <- suppressWarnings(filterFX(mbase[[names(cr)]], currency = x, price = 'Lo'))
        armaOrder.Lo <- opt_arma(mb.Lo)
        
        armaOrder <- list(armaOrder.Op = armaOrder.Op, armaOrder.Hi = armaOrder.Hi, 
                          armaOrder.Lo = armaOrder.Lo, armaOrder.Cl = armaOrder.Cl) %>% 
          filterNull
      })
      
    } else {
      armaOrder.Op <- NULL
      armaOrder.Cl <- NULL
      
      mb.Hi <- suppressWarnings(filterFX(mbase, currency = .currency, price = 'Hi'))
      armaOrder.Hi <- opt_arma(mb.Hi)
      
      mb.Lo <- suppressWarnings(filterFX(mbase, currency = .currency, price = 'Lo'))
      armaOrder.Lo <- opt_arma(mb.Lo)
      
      armaOrder <- list(armaOrder.Op = armaOrder.Op, armaOrder.Hi = armaOrder.Hi, 
                        armaOrder.Lo = armaOrder.Lo, armaOrder.Cl = armaOrder.Cl) %>% 
        filterNull
    }
    
  } else if (.price_type == 'C') {
    
    if (.currency == 'ALL') {
      armaOrder <- llply(cr_code, function(x) {
        cr <- grep(x, cr_code, value = TRUE)
        
        armaOrder.Op <- NULL
        armaOrder.Hi <- NULL
        armaOrder.Lo <- NULL
        
        mb.Cl <- suppressWarnings(filterFX(mbase[[names(cr)]], currency = x, price = 'Cl'))
        armaOrder.Cl <- opt_arma(mb.Cl)
        
        armaOrder <- list(armaOrder.Op = armaOrder.Op, armaOrder.Hi = armaOrder.Hi, 
                          armaOrder.Lo = armaOrder.Lo, armaOrder.Cl = armaOrder.Cl) %>% 
          filterNull
      })
      
    } else {
      armaOrder.Op <- NULL
      armaOrder.Hi <- NULL
      armaOrder.Lo <- NULL
      
      mb.Cl <- suppressWarnings(filterFX(mbase, currency = .currency, price = 'Cl'))
      armaOrder.Cl <- opt_arma(mb.Cl)
      
      armaOrder <- list(armaOrder.Op = armaOrder.Op, armaOrder.Hi = armaOrder.Hi, 
                        armaOrder.Lo = armaOrder.Lo, armaOrder.Cl = armaOrder.Cl) %>% 
        filterNull
    }
    
  } else {
    stop(paste0('.price_type must be in \'', paste(.price_types, collapse = ', '), '\'.'))
  }
  
  if (.currency == 'ALL') {
    speclist <- llply(names(cr_code), function(x) {
      llply(armaOrder[[x]], function(y) {
        spec = ugarchspec(
          variance.model = list(
            model = 'gjrGARCH', garchOrder = c(1, 1), 
            submodel = NULL, external.regressors = NULL, 
            variance.targeting = FALSE), 
          mean.model = list(
            armaOrder = y[c('p', 'q')], 
            include.mean = TRUE, archm = FALSE, 
            archpow = 1, arfima = TRUE, 
            external.regressors = NULL, 
            archex = FALSE), 
          fixed.pars = list(arfima = y['d']), 
          distribution.model = 'snorm')
      })
    })
    names(speclist) <- names(cr_code)
    
  } else {
    speclist <- llply(armaOrder, function(x) {
      spec = ugarchspec(
        variance.model = list(
          model = 'gjrGARCH', garchOrder = c(1, 1), 
          submodel = NULL, external.regressors = NULL, 
          variance.targeting = FALSE), 
        mean.model = list(
          armaOrder = x[c('p', 'q')], 
          include.mean = TRUE, archm = FALSE, 
          archpow = 1, arfima = TRUE, 
          external.regressors = NULL, 
          archex = FALSE), 
        fixed.pars = list(arfima = x['d']), 
        distribution.model = 'snorm')
    })
  }
  
  return(c(speclist, recursive = TRUE))
}
