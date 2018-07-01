## ================== Declaration ========================================
options(warn = -1)
suppressWarnings(require('shiny'))
suppressWarnings(require('cronR'))
suppressWarnings(require('xts'))
suppressWarnings(require('quantmod'))
suppressWarnings(require('TFX'))
suppressWarnings(require('lubridate'))
suppressWarnings(require('plyr'))
suppressWarnings(require('dplyr'))
suppressWarnings(require('tidyr'))
suppressWarnings(require('magrittr'))
suppressWarnings(require('memoise'))
suppressWarnings(require('stringr'))
suppressWarnings(require('RCurl'))
suppressWarnings(require('rugarch'))
suppressWarnings(require('rmgarch'))
suppressWarnings(require('forecast'))

Sys.setenv(TZ = 'GMT')
zones <- attr(as.POSIXlt(now('GMT')), 'tzone')
zone <- ifelse(zones[[1]] == '', paste(zones[-1], collapse = '/'), 
               zones[[1]])
timeR <- now('GMT')

#fx <- c('EURUSD=X', 'JPY=X', 'GBPUSD=X', 'CHF=X', 'CAD=X', 'AUDUSD=X')
fx <- c('JPY=X')
wd <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
wd %<>% factor(., levels = ., ordered = TRUE)
## https://beta.rstudioconnect.com/connect/#/apps/3803/logs
## ================== Functions ========================================
#if(weekdays(today('GMT')) %in% wd) {
  prd <- ifelse(weekdays(today('GMT')) %in% wd[1:4], 1, 3)
  
  for(i in seq(fx)) {
    assign(fx[i], suppressWarnings(
      getSymbols(fx[i], from = (today('GMT') - prd) %m-% years(1), 
                 to = (today('GMT') - prd), auto.assign = FALSE))) }
  rm(i)
#}

# Function to get new observations
#'@ get_new_data <- function() readLines('http://webrates.truefx.com/rates/connect.html')

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
  res = tail(attributes(fc)$forecast$seriesFor, ahead - (ahead - 1))
  colnames(res) = names(mbase)
  latestPrice = tail(mbase, 1)
  forDate = latestPrice %>% index + days(ahead)
  rownames(res) <- as.character(forDate)
  
  tmp = list(latestPrice = latestPrice, forecastPrice = res)
  return(tmp)
})

forecastUSDJPY <- function(ahead = 1, price = 'Cl') {
  forC.USDJPY <- calC('JPY=X', ahead = ahead, price = price)
  
  fxC <- data.frame(ForecastDate.GMT = rownames(forC.USDJPY$forecastPrice), 
                    Currency = forC.USDJPY$forecastPrice)
  
  if(price == 'Hi') fxC %<>% rename(fc.High = USD.JPY)
  if(price == 'Lo') fxC %<>% rename(fc.Low = USD.JPY)
  
  return(fxC)
  }

forecastUSDJPYHL <- function(ahead = ahead){
  fxLo <- forecastUSDJPY(ahead = ahead, price = 'Lo')
  fxHi <- forecastUSDJPY(ahead = ahead, price = 'Hi')
  fxHL <- merge(fxHi, fxLo, by = c('ForecastDate.GMT'))
  rm(fxHi, fxLo)
  return(fxHL)
  }


## ================== Reference ========================================
## https://shiny.rstudio.com/articles/persistent-data-storage.html
## https://github.com/bnosac/cronR
## http://www.bnosac.be/index.php/blog/64-scheduling-r-scripts-and-processes-on-windows-and-unix-linux

