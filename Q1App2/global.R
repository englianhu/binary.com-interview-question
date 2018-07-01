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
#'@ if(weekdays(today('GMT')) %in% wd) {
#'@   for(i in seq(fx)) {
#'@     suppressWarnings(getSymbols(fx[i], from = (today('GMT') - 1) %m-% years(1), 
#'@                to = (today('GMT') - 1), auto.assign = FALSE))
#'@   }
#'@   rm(i)
#'@ }

if(weekdays(today('GMT')) %in% wd) {
  prd <- ifelse(weekdays(today('GMT')) == wd[5], 3, 1)
  
  for(i in seq(fx)) {
    assign(fx[i], suppressWarnings(
      getSymbols(fx[i], from = (today('GMT') - prd) %m-% years(1), 
                 to = (today('GMT') - prd), auto.assign = FALSE))) }
  rm(i) }

# Using "memoise" to automatically cache the results
openBet <- memoise(function(currency, realFX, ahead = 1) {
  
  hi <- calC(currency, ahead, price = 'Hi')
  lo <- calC(currency, ahead, price = 'Lo')
  
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


armaSearch <- function(data, .method = 'CSS-ML'){ 
  ## ARMA Modeling寻找AIC值最小的p,q
  ##
  ## I set .method = 'CSS-ML' as default method since the AIC value we got is 
  ##  smaller than using method 'ML' while using method 'CSS' facing error.
  ## 
  ## https://stats.stackexchange.com/questions/209730/fitting-methods-in-arima
  ## According to the documentation, this is how each method fits the model:
  ##  - CSS minimises the sum of squared residuals.
  ##  - ML maximises the log-likelihood function of the ARIMA model.
  ##  - CSS-ML mixes both methods: first, CSS is run, the starting parameters 
  ##    for the optimization algorithm are set to zeros or to the values given 
  ##    in the optional argument init; then, ML is applied passing the CSS 
  ##    parameter estimates as starting parameter values for the optimization algorithm.
  
  .methods = c('CSS-ML', 'ML', 'CSS')
  
  if(!.method %in% .methods) stop(paste('Kindly choose .method among ', 
                                        paste0(.methods, collapse = ', '), '!'))
  
  armacoef <- data.frame()
  for (p in 0:5){
    for (q in 0:5) {
      #data.arma = arima(diff(data), order = c(p, 0, q))
      #'@ data.arma = arima(data, order = c(p, 1, q), method = .method)
      if(.method == 'CSS-ML') {
        data.arma = tryCatch({
          arma = arima(data, order = c(p, 1, q), method = 'CSS-ML')
          mth = 'CSS-ML'
          list(arma, mth)
        }, error = function(e) {
          arma = arima(data, order = c(p, 1, q), method = 'ML')
          mth = 'ML'
          list(arma = arma, mth = mth)
        })
      } else if(.method == 'ML') {
        data.arma = tryCatch({
          arma = arima(data, order = c(p, 1, q), method = 'ML')
          mth = 'ML'
          list(arma = arma, mth = mth)
        }, error = function(e) {
          arma = arima(data, order = c(p, 1, q), method = 'CSS-ML')
          mth = 'CSS-ML'
          list(arma = arma, mth = mth)
        })
      } else if(.method == 'CSS') {
        data.arma = tryCatch({
          arma = arima(data, order = c(p, 1, q), method = 'CSS')
          mth = 'CSS'
          list(arma = arma, mth = mth)
        }, error = function(e) {
          arma = arima(data, order = c(p, 1, q), method = 'CSS-ML')
          mth = 'CSS-ML'
          list(arma = arma, mth = mth)
        })
      } else {
        stop(paste('Kindly choose .method among ', 
                   paste0(.methods, collapse = ', '), '!'))
      }
      names(data.arma) <- c('arma', 'mth')
      
      #cat('p =', p, ', q =', q, 'AIC =', data.arma$arma$aic, '\n')
      armacoef <- rbind(armacoef, c(p, q, data.arma$arma$aic))
    }
  }
  
  colnames(armacoef) <- c('p', 'q', 'AIC')
  pos <- which(armacoef$AIC == min(armacoef$AIC))
  cat(paste0('method = \'', data.arma$mth, '\', the min AIC = ', 
             armacoef$AIC[pos], ', p = ', armacoef$p[pos], 
             ', q = ', armacoef$q[pos], '\n'))
  return(armacoef)
}

filterFX <- function(currency, price = 'Cl') {
  if(currency == 'AUDUSD=X') {
    if(price == 'Op') {
      mbase <- `AUDUSD=X` %>% Op %>% na.omit; rm(`AUDUSD=X`)
    } else if(price == 'Hi') {
      mbase <- `AUDUSD=X` %>% Hi %>% na.omit; rm(`AUDUSD=X`)
    } else if(price == 'Lo') {
      mbase <- `AUDUSD=X` %>% Lo %>% na.omit; rm(`AUDUSD=X`)
    } else if(price == 'Cl') {
      mbase <- `AUDUSD=X` %>% Cl %>% na.omit; rm(`AUDUSD=X`)
    } else if(price == 'Ad') {
      mbase <- `AUDUSD=X` %>% Ad %>% na.omit; rm(`AUDUSD=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('AUDUSD=X', 'AUD.USD')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else if(currency == 'EURUSD=X') {
    if(price == 'Op') {
      mbase <- `EURUSD=X` %>% Op %>% na.omit; rm(`EURUSD=X`)
    } else if(price == 'Hi') {
      mbase <- `EURUSD=X` %>% Hi %>% na.omit; rm(`EURUSD=X`)
    } else if(price == 'Lo') {
      mbase <- `EURUSD=X` %>% Lo %>% na.omit; rm(`EURUSD=X`)
    } else if(price == 'Cl') {
      mbase <- `EURUSD=X` %>% Cl %>% na.omit; rm(`EURUSD=X`)
    } else if(price == 'Ad') {
      mbase <- `EURUSD=X` %>% Ad %>% na.omit; rm(`EURUSD=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('EURUSD=X', 'EUR.USD')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else if(currency == 'GBPUSD=X') {
    if(price == 'Op') {
      mbase <- `GBPUSD=X` %>% Op %>% na.omit; rm(`GBPUSD=X`)
    } else if(price == 'Hi') {
      mbase <- `GBPUSD=X` %>% Hi %>% na.omit; rm(`GBPUSD=X`)
    } else if(price == 'Lo') {
      mbase <- `GBPUSD=X` %>% Lo %>% na.omit; rm(`GBPUSD=X`)
    } else if(price == 'Cl') {
      mbase <- `GBPUSD=X` %>% Cl %>% na.omit; rm(`GBPUSD=X`)
    } else if(price == 'Ad') {
      mbase <- `GBPUSD=X` %>% Ad %>% na.omit; rm(`GBPUSD=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('GBPUSD=X', 'GBP.USD')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else if(currency == 'CHF=X') {
    if(price == 'Op') {
      mbase <- `CHF=X` %>% Op %>% na.omit; rm(`CHF=X`)
    } else if(price == 'Hi') {
      mbase <- `CHF=X` %>% Hi %>% na.omit; rm(`CHF=X`)
    } else if(price == 'Lo') {
      mbase <- `CHF=X` %>% Lo %>% na.omit; rm(`CHF=X`)
    } else if(price == 'Cl') {
      mbase <- `CHF=X` %>% Cl %>% na.omit; rm(`CHF=X`)
    } else if(price == 'Ad') {
      mbase <- `CHF=X` %>% Ad %>% na.omit; rm(`CHF=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('CHF=X', 'USD.CHF')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else if(currency == 'CAD=X') {
    if(price == 'Op') {
      mbase <- `CAD=X` %>% Op %>% na.omit; rm(`CAD=X`)
    } else if(price == 'Hi') {
      mbase <- `CAD=X` %>% Hi %>% na.omit; rm(`CAD=X`)
    } else if(price == 'Lo') {
      mbase <- `CAD=X` %>% Lo %>% na.omit; rm(`CAD=X`)
    } else if(price == 'Cl') {
      mbase <- `CAD=X` %>% Cl %>% na.omit; rm(`CAD=X`)
    } else if(price == 'Ad') {
      mbase <- `CAD=X` %>% Ad %>% na.omit; rm(`CAD=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('CAD=X', 'USD.CAD')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else if(currency == 'CNY=X') {
    if(price == 'Op') {
      mbase <- `CNY=X` %>% Op %>% na.omit; rm(`CNY=X`)
    } else if(price == 'Hi') {
      mbase <- `CNY=X` %>% Hi %>% na.omit; rm(`CNY=X`)
    } else if(price == 'Lo') {
      mbase <- `CNY=X` %>% Lo %>% na.omit; rm(`CNY=X`)
    } else if(price == 'Cl') {
      mbase <- `CNY=X` %>% Cl %>% na.omit; rm(`CNY=X`)
    } else if(price == 'Ad') {
      mbase <- `CNY=X` %>% Ad %>% na.omit; rm(`CNY=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('CNY=X', 'USD.CNY')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else if(currency == 'JPY=X') {
    if(price == 'Op') {
      mbase <- `JPY=X` %>% Op %>% na.omit; rm(`JPY=X`)
    } else if(price == 'Hi') {
      mbase <- `JPY=X` %>% Hi %>% na.omit; rm(`JPY=X`)
    } else if(price == 'Lo') {
      mbase <- `JPY=X` %>% Lo %>% na.omit; rm(`JPY=X`)
    } else if(price == 'Cl') {
      mbase <- `JPY=X` %>% Cl %>% na.omit; rm(`JPY=X`)
    } else if(price == 'Ad') {
      mbase <- `JPY=X` %>% Ad %>% na.omit; rm(`JPY=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('JPY=X', 'USD.JPY')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else {
    stop('Kindly choose common currencies exchange.')
  }
  return(mbase)
}

# Using "memoise" to automatically cache the results
calC <- memoise(function(currency, ahead = 1, price = 'Cl') {
  
  mbase = filterFX(currency, price = price)
  
  armaOrder = armaSearch(mbase)
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
      external.regressors = NULL, 
      archex = FALSE), 
    distribution.model = 'snorm')
  fit = ugarchfit(spec, mbase, solver = 'hybrid')
  fc = ugarchforecast(fit, n.ahead = ahead)
  res = attributes(fc)$forecast$seriesFor
  colnames(res) = names(mbase)
  latestPrice = tail(mbase, 1)
  forDate = latestPrice %>% index + days(1)
  rownames(res) <- as.character(forDate)
  
  tmp = list(latestPrice = latestPrice, forecastPrice = res)
  return(tmp)
})

forecastData <- function(price = 'Cl') {
  forC.EURUSD <- calC('EURUSD=X', price = price, ahead = ahead)
  forC.USDJPY <- calC('JPY=X', price = price, ahead = ahead)
  forC.GBPUSD <- calC('GBPUSD=X', price = price, ahead = ahead)
  forC.USDCHF <- calC('CHF=X', price = price, ahead = ahead)
  forC.USDCAD <- calC('CAD=X', price = price, ahead = ahead)
  forC.AUDUSD <- calC('AUDUSD=X', price = price, ahead = ahead)
  
  fxC <- ldply(list(EURUSD = forC.EURUSD, 
                    USDJPY = forC.USDJPY, 
                    GBPUSD = forC.GBPUSD, 
                    USDCHF = forC.USDCHF, 
                    USDCAD = forC.USDCAD, 
                    AUDUSD = forC.AUDUSD), function(x) 
                      data.frame(ForecastDate.GMT = rownames(x$forecastPrice), 
                                 x$forecastPrice)) %>% 
    unite(., Currency, EUR.USD:AUD.USD) %>% 
    mutate(Currency = as.numeric(str_replace_all(Currency, 'NA|_', '')))
  if(price == 'Hi') names(fxC)[3] <- 'Currency.Hi'
  if(price == 'Lo') names(fxC)[3] <- 'Currency.Lo'
  
  return(fxC)
}
