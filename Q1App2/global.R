options(warn = -1)
suppressWarnings(require('shiny'))
suppressWarnings(require('cronR'))
suppressWarnings(require('xts'))
suppressWarnings(require('quantmod'))
suppressWarnings(require('lubridate'))
suppressWarnings(require('stringr'))
suppressWarnings(require('memoise'))
suppressWarnings(require('rugarch'))
suppressWarnings(require('rmgarch'))

fx <- c('EURUSD=X', 'JPY=X', 'GBPUSD=X', 'CHF=X', 'CAD=X', 'AUDUSD=X')
cur <- c('EUR/USD', 'USD/JPY', 'GBP/USD', 'USD/CHF', 'USD/CAD', 'AUD/USD')
wd <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')#, 'Saturday', 'Sunday')

#'@ if(now('GMT') == today('GMT')) {
## https://finance.yahoo.com/quote/AUDUSD=X?p=AUDUSD=X
## Above link prove that https://finance.yahoo.com using GMT time zone.  
if(weekdays(today('GMT')) %in% wd) {
  prd <- ifelse(weekdays(today('GMT')) == wd[5], 3, 1)
  
  for(i in seq(fx)) {
    assign(fx[i], suppressWarnings(
      getSymbols(fx[i], from = (today('GMT') - prd) %m-% years(1), 
                 to = (today('GMT') - prd), auto.assign = FALSE))) }
  rm(i) }

# Using "memoise" to automatically cache the results
openBet <- memoise(function(currency, realFX, ahead = 1) {
  
  hi <- calc_fx(currency, ahead, price = 'Hi')
  lo <- calc_fx(currency, ahead, price = 'Lo')
  
  bid <- realFX %>% dplyr::select(Symbol, Bid.Price) %>% 
    dplyr::filter(Symbol %in% c('EUR/USD', 'USD/JPY', 'GBP/USD', 'USD/CHF', 
                                'USD/CAD', 'AUD/USD'))
  ask <- realFX %>% dplyr::select(Symbol, Ask.Price) %>% 
    dplyr::filter(Symbol %in% c('EUR/USD', 'USD/JPY', 'GBP/USD', 'USD/CHF', 
                                'USD/CAD', 'AUD/USD'))
  
  ## http://webrates.truefx.com/rates/connect.html
  tmp = list(latestPrice = tail(mbase, 1), forecastPrice = res)
  return(tmp)
})

kellyBet <- function(currency, ){
  
  return(res)
}

opt_arma <- function(mbase){
  #ARMA Modeling minimum AIC value of `p,d,q`
  fit <- auto.arima(mbase)
  arimaorder(fit)
}

filterFX <- function(mbase, currency = 'JPY=X', price = 'Cl') {
  
  cr_code <- c('AUDUSD=X', 'EURUSD=X', 'GBPUSD=X', 'CHF=X', 'CAD=X', 
               'CNY=X', 'JPY=X')
  
  cr_name <- c('AUDUSD', 'EURUSD', 'GBPUSD', 'USDCHF', 'USDCAD', 
               'USDCNY', 'USDJPY')
  
  price_type <- c('Op', 'Hi', 'Lo', 'Cl', 'Ad')
  
  if(currency == 'AUDUSD=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; suppressWarnings(rm(`AUDUSD=X`))
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; suppressWarnings(rm(`AUDUSD=X`))
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; suppressWarnings(rm(`AUDUSD=X`))
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; suppressWarnings(rm(`AUDUSD=X`))
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; suppressWarnings(rm(`AUDUSD=X`))
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('AUDUSD=X', 'AUDUSD')
    names(mbase) %<>% str_replace_all(
      '.Open|.High|.Low|.Close|.Volume|.Adjusted', '')
    
  } else if(currency == 'EURUSD=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; suppressWarnings(rm(`EURUSD=X`))
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; suppressWarnings(rm(`EURUSD=X`))
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; suppressWarnings(rm(`EURUSD=X`))
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; suppressWarnings(rm(`EURUSD=X`))
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; suppressWarnings(rm(`EURUSD=X`))
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('EURUSD=X', 'EURUSD')
    names(mbase) %<>% str_replace_all(
      '.Open|.High|.Low|.Close|.Volume|.Adjusted', '')
    
  } else if(currency == 'GBPUSD=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; suppressWarnings(rm(`GBPUSD=X`))
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; suppressWarnings(rm(`GBPUSD=X`))
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; suppressWarnings(rm(`GBPUSD=X`))
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; suppressWarnings(rm(`GBPUSD=X`))
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; suppressWarnings(rm(`GBPUSD=X`))
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('GBPUSD=X', 'GBPUSD')
    names(mbase) %<>% str_replace_all(
      '.Open|.High|.Low|.Close|.Volume|.Adjusted', '')
    
  } else if(currency == 'CHF=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; suppressWarnings(rm(`CHF=X`))
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; suppressWarnings(rm(`CHF=X`))
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; suppressWarnings(rm(`CHF=X`))
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; suppressWarnings(rm(`CHF=X`))
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; suppressWarnings(rm(`CHF=X`))
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('CHF=X', 'USDCHF')
    names(mbase) %<>% str_replace_all(
      '.Open|.High|.Low|.Close|.Volume|.Adjusted', '')
    
  } else if(currency == 'CAD=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; suppressWarnings(rm(`CAD=X`))
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; suppressWarnings(rm(`CAD=X`))
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; suppressWarnings(rm(`CAD=X`))
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; suppressWarnings(rm(`CAD=X`))
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; suppressWarnings(rm(`CAD=X`))
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('CAD=X', 'USDCAD')
    names(mbase) %<>% str_replace_all(
      '.Open|.High|.Low|.Close|.Volume|.Adjusted', '')
    
  } else if(currency == 'CNY=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; suppressWarnings(rm(`CNY=X`))
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; suppressWarnings(rm(`CNY=X`))
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; suppressWarnings(rm(`CNY=X`))
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; suppressWarnings(rm(`CNY=X`))
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; suppressWarnings(rm(`CNY=X`))
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('CNY=X', 'USDCNY')
    names(mbase) %<>% str_replace_all(
      '.Open|.High|.Low|.Close|.Volume|.Adjusted', '')
    
  } else if(currency == 'JPY=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; suppressWarnings(rm(`JPY=X`))
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; suppressWarnings(rm(`JPY=X`))
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; suppressWarnings(rm(`JPY=X`))
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; suppressWarnings(rm(`JPY=X`))
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; suppressWarnings(rm(`JPY=X`))
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('JPY=X', 'USDJPY')
    names(mbase) %<>% str_replace_all(
      '.Open|.High|.Low|.Close|.Volume|.Adjusted', '')
    
  } else {
    stop('Kindly choose common currencies exchange.')
  }
  
  mbase %<>% na.omit
  
  return(mbase)
}

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
  res = tail(attributes(fc)$forecast$seriesFor, 1)
  colnames(res) = names(mbase)
  latestPrice = xts(tail(mbase, 1))
  
  #'@ forDate = latestPrice %>% index + days(1)
  #'@ rownames(res) <- as.character(forDate)
  #'@ res <- as.xts(res)
  
  tmp = list(latestPrice = latestPrice, forecastPrice = res, 
             AIC = infocriteria(fit))
  return(tmp)
})

forecastData <- function(price = 'Cl', ahead = 1) {
  forC.EURUSD <- calc_fx(`EURUSD=X`, 'EURUSD=X', price = price, ahead = ahead)
  forC.USDJPY <- calc_fx(`JPY=X`, 'JPY=X', price = price, ahead = ahead)
  forC.GBPUSD <- calc_fx(`GBPUSD=X`, 'GBPUSD=X', price = price, ahead = ahead)
  forC.USDCHF <- calc_fx(`CHF=X`, 'CHF=X', price = price, ahead = ahead)
  forC.USDCAD <- calc_fx(`CAD=X`, 'CAD=X', price = price, ahead = ahead)
  forC.AUDUSD <- calc_fx(`AUDUSD=X`, 'AUDUSD=X', price = price, ahead = ahead)
  
  fxC <- ldply(list(EURUSD = forC.EURUSD, 
                    USDJPY = forC.USDJPY, 
                    GBPUSD = forC.GBPUSD, 
                    USDCHF = forC.USDCHF, 
                    USDCAD = forC.USDCAD, 
                    AUDUSD = forC.AUDUSD), function(x) 
                      data.frame(x$forecastPrice)) %>% 
    unite(., Price.T1, EURUSD:AUDUSD) %>% 
    mutate(Price.T1 = as.numeric(str_replace_all(Price.T1, 'NA|_', '')))
  
  if(price == 'Hi') names(fxC)[2] <- 'Price.T1.Hi'
  if(price == 'Lo') names(fxC)[2] <- 'Price.T1.Lo'
  
  return(fxC)
}
