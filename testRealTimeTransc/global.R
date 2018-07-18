## ================== Declaration ========================================
options(warn = -1, 'getSymbols.yahoo.warning' = FALSE)
suppressPackageStartupMessages(suppressWarnings(require('BBmisc')))
suppressAll(require('shiny'))
suppressAll(require('cronR'))
suppressAll(require('xts'))
suppressAll(require('quantmod'))
suppressAll(require('TFX'))
suppressAll(require('lubridate'))
suppressAll(require('plyr'))
suppressAll(require('dplyr'))
suppressAll(require('data.table'))
suppressAll(require('tidyr'))
suppressAll(require('magrittr'))
suppressAll(require('memoise'))
suppressAll(require('stringr'))
suppressAll(require('RCurl'))
suppressAll(require('rugarch'))
suppressAll(require('rmgarch'))
suppressAll(require('forecast'))

Sys.setenv(TZ = 'GMT')
zones <- attr(as.POSIXlt(now('GMT')), 'tzone')
zone <- ifelse(zones[[1]] == '', paste(zones[-1], collapse = '/'), 
               zones[[1]])
timeR <- now('GMT')

#fx <- c('EURUSD=X', 'JPY=X', 'GBPUSD=X', 'CHF=X', 'CAD=X', 'AUDUSD=X')
fx <- c('JPY=X')

#fxObj <- c('EURUSD', 'USDJPY', 'GBPUSD', 'USDCHF', 'USDCAD', 'AUDUSD')
fxObj <- c('USDJPY')

wd <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
wd %<>% factor(., levels = ., ordered = TRUE)
## https://beta.rstudioconnect.com/connect/#/apps/3803/logs
## ====================== Data ========================================
#if(weekdays(today('GMT')) %in% wd) {
  #prd <- ifelse(weekdays(today('GMT')) %in% wd[2:5], 1, 3)
  prd = 1 #since count trading day.
  
  for(i in seq(fx)) {
    assign(fxObj[i], na.omit(suppressWarnings(
      getSymbols(fx[i], from = (today('GMT') - days(prd)) %m-% years(1), 
                 to = (today('GMT') - days(prd)), auto.assign = FALSE)))) }
  rm(i)
#}
#mbase <- `JPY=X`
#rm(`JPY=X`)
names(USDJPY) <- str_replace_all(names(USDJPY), 'JPY.X', 'USD.JPY')
USDJPY %<>% na.omit

## ================== Functions ========================================
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

filterFX <- function(mbase, currency = 'JPY=X', price = 'Cl') {
  
  if(currency == 'AUDUSD=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; rm(`AUDUSD=X`)
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; rm(`AUDUSD=X`)
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; rm(`AUDUSD=X`)
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; rm(`AUDUSD=X`)
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; rm(`AUDUSD=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('AUDUSD=X', 'AUD.USD')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else if(currency == 'EURUSD=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; rm(`EURUSD=X`)
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; rm(`EURUSD=X`)
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; rm(`EURUSD=X`)
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; rm(`EURUSD=X`)
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; rm(`EURUSD=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('EURUSD=X', 'EUR.USD')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else if(currency == 'GBPUSD=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; rm(`GBPUSD=X`)
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; rm(`GBPUSD=X`)
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; rm(`GBPUSD=X`)
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; rm(`GBPUSD=X`)
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; rm(`GBPUSD=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('GBPUSD=X', 'GBP.USD')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else if(currency == 'CHF=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; rm(`CHF=X`)
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; rm(`CHF=X`)
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; rm(`CHF=X`)
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; rm(`CHF=X`)
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; rm(`CHF=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('CHF=X', 'USD.CHF')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else if(currency == 'CAD=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; rm(`CAD=X`)
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; rm(`CAD=X`)
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; rm(`CAD=X`)
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; rm(`CAD=X`)
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; rm(`CAD=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('CAD=X', 'USD.CAD')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else if(currency == 'CNY=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; rm(`CNY=X`)
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; rm(`CNY=X`)
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; rm(`CNY=X`)
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; rm(`CNY=X`)
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; rm(`CNY=X`)
    } else {
      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
    }
    names(mbase) %<>% str_replace_all('CNY=X', 'USD.CNY')
    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
    
  } else if(currency == 'JPY=X') {
    if(price == 'Op') {
      mbase %<>% Op %>% na.omit; rm(`JPY=X`)
    } else if(price == 'Hi') {
      mbase %<>% Hi %>% na.omit; rm(`JPY=X`)
    } else if(price == 'Lo') {
      mbase %<>% Lo %>% na.omit; rm(`JPY=X`)
    } else if(price == 'Cl') {
      mbase %<>% Cl %>% na.omit; rm(`JPY=X`)
    } else if(price == 'Ad') {
      mbase %<>% Ad %>% na.omit; rm(`JPY=X`)
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

#filterFX <- function(mbase, currency, price = 'Cl') {
#  if(currency == 'AUDUSD=X') {
#    if(price == 'Op') {
#      mbase <- `AUDUSD=X` %>% Op %>% na.omit; rm(`AUDUSD=X`)
#    } else if(price == 'Hi') {
#      mbase <- `AUDUSD=X` %>% Hi %>% na.omit; rm(`AUDUSD=X`)
#    } else if(price == 'Lo') {
#      mbase <- `AUDUSD=X` %>% Lo %>% na.omit; rm(`AUDUSD=X`)
#    } else if(price == 'Cl') {
#      mbase <- `AUDUSD=X` %>% Cl %>% na.omit; rm(`AUDUSD=X`)
#    } else if(price == 'Ad') {
#      mbase <- `AUDUSD=X` %>% Ad %>% na.omit; rm(`AUDUSD=X`)
#    } else {
#      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
#    }
#    names(mbase) %<>% str_replace_all('AUDUSD=X', 'AUD.USD')
#    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
#    
#  } else if(currency == 'EURUSD=X') {
#    if(price == 'Op') {
#      mbase <- `EURUSD=X` %>% Op %>% na.omit; rm(`EURUSD=X`)
#    } else if(price == 'Hi') {
#      mbase <- `EURUSD=X` %>% Hi %>% na.omit; rm(`EURUSD=X`)
#    } else if(price == 'Lo') {
#      mbase <- `EURUSD=X` %>% Lo %>% na.omit; rm(`EURUSD=X`)
#    } else if(price == 'Cl') {
#      mbase <- `EURUSD=X` %>% Cl %>% na.omit; rm(`EURUSD=X`)
#    } else if(price == 'Ad') {
#      mbase <- `EURUSD=X` %>% Ad %>% na.omit; rm(`EURUSD=X`)
#    } else {
#      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
#    }
#    names(mbase) %<>% str_replace_all('EURUSD=X', 'EUR.USD')
#    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
#    
#  } else if(currency == 'GBPUSD=X') {
#    if(price == 'Op') {
#      mbase <- `GBPUSD=X` %>% Op %>% na.omit; rm(`GBPUSD=X`)
#    } else if(price == 'Hi') {
#      mbase <- `GBPUSD=X` %>% Hi %>% na.omit; rm(`GBPUSD=X`)
#    } else if(price == 'Lo') {
#      mbase <- `GBPUSD=X` %>% Lo %>% na.omit; rm(`GBPUSD=X`)
#    } else if(price == 'Cl') {
#      mbase <- `GBPUSD=X` %>% Cl %>% na.omit; rm(`GBPUSD=X`)
#    } else if(price == 'Ad') {
#      mbase <- `GBPUSD=X` %>% Ad %>% na.omit; rm(`GBPUSD=X`)
#    } else {
#      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
#    }
#    names(mbase) %<>% str_replace_all('GBPUSD=X', 'GBP.USD')
#    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
#    
#  } else if(currency == 'CHF=X') {
#    if(price == 'Op') {
#      mbase <- `CHF=X` %>% Op %>% na.omit; rm(`CHF=X`)
#    } else if(price == 'Hi') {
#      mbase <- `CHF=X` %>% Hi %>% na.omit; rm(`CHF=X`)
#    } else if(price == 'Lo') {
#      mbase <- `CHF=X` %>% Lo %>% na.omit; rm(`CHF=X`)
#    } else if(price == 'Cl') {
#      mbase <- `CHF=X` %>% Cl %>% na.omit; rm(`CHF=X`)
#    } else if(price == 'Ad') {
#      mbase <- `CHF=X` %>% Ad %>% na.omit; rm(`CHF=X`)
#    } else {
#      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
#    }
#    names(mbase) %<>% str_replace_all('CHF=X', 'USD.CHF')
#    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
#    
#  } else if(currency == 'CAD=X') {
#    if(price == 'Op') {
#      mbase <- `CAD=X` %>% Op %>% na.omit; rm(`CAD=X`)
#    } else if(price == 'Hi') {
#      mbase <- `CAD=X` %>% Hi %>% na.omit; rm(`CAD=X`)
#    } else if(price == 'Lo') {
#      mbase <- `CAD=X` %>% Lo %>% na.omit; rm(`CAD=X`)
#    } else if(price == 'Cl') {
#      mbase <- `CAD=X` %>% Cl %>% na.omit; rm(`CAD=X`)
#    } else if(price == 'Ad') {
#      mbase <- `CAD=X` %>% Ad %>% na.omit; rm(`CAD=X`)
#    } else {
#      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
#    }
#    names(mbase) %<>% str_replace_all('CAD=X', 'USD.CAD')
#    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
#    
#  } else if(currency == 'CNY=X') {
#    if(price == 'Op') {
#      mbase <- `CNY=X` %>% Op %>% na.omit; rm(`CNY=X`)
#    } else if(price == 'Hi') {
#      mbase <- `CNY=X` %>% Hi %>% na.omit; rm(`CNY=X`)
#    } else if(price == 'Lo') {
#      mbase <- `CNY=X` %>% Lo %>% na.omit; rm(`CNY=X`)
#    } else if(price == 'Cl') {
#      mbase <- `CNY=X` %>% Cl %>% na.omit; rm(`CNY=X`)
#    } else if(price == 'Ad') {
#      mbase <- `CNY=X` %>% Ad %>% na.omit; rm(`CNY=X`)
#    } else {
#      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
#    }
#    names(mbase) %<>% str_replace_all('CNY=X', 'USD.CNY')
#    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
#    
#  } else if(currency == 'JPY=X') {
#    if(price == 'Op') {
#      mbase <- `JPY=X` %>% Op %>% na.omit; rm(`JPY=X`)
#    } else if(price == 'Hi') {
#      mbase <- `JPY=X` %>% Hi %>% na.omit; rm(`JPY=X`)
#    } else if(price == 'Lo') {
#      mbase <- `JPY=X` %>% Lo %>% na.omit; rm(`JPY=X`)
#    } else if(price == 'Cl') {
#      mbase <- `JPY=X` %>% Cl %>% na.omit; rm(`JPY=X`)
#    } else if(price == 'Ad') {
#      mbase <- `JPY=X` %>% Ad %>% na.omit; rm(`JPY=X`)
#    } else {
#      stop("'price' must be 'Op', 'Hi', 'Lo', 'Cl' or 'Ad'.")
#    }
#    names(mbase) %<>% str_replace_all('JPY=X', 'USD.JPY')
#    names(mbase) %<>% str_replace_all('Open|.High|.Low|.Close|Adjusted', '')
#    
#  } else {
#    stop('Kindly choose common currencies exchange.')
#  }
#  return(mbase)
#}

# Using "memoise" to automatically cache the results
calC <- memoise(function(mbase, currency = 'JPY=X', ahead = 1, price = 'Cl') {
  
  mbase = filterFX(mbase, currency = currency, price = price)
  
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

forecastUSDJPY <- function(mbase, currency = 'JPY=X', ahead = 1, price = 'Cl') {
  forC.USDJPY <- calC(mbase, currency = currency, ahead = ahead, price = price)
  
  fxC <- data.frame(
    LatestDate.GMT = index(forC.USDJPY$latestPrice), 
    latestPrice = forC.USDJPY$latestPrice, 
    #ForecastDate.GMT = index(forC.USDJPY$forecastPrice), 
    ForecastDate.GMT = rownames(forC.USDJPY$forecastPrice), 
    Currency = forC.USDJPY$forecastPrice)
  
  rownames(fxC) <- NULL
  nm <- names(mbase) %>% 
    str_replace_all('.Open|.High|.Low|.Close|.Volume|.Adjusted', '') %>% unique
  
  if(nm == 'USD.JPY') {
    if(price == 'Op') fxC %<>% dplyr::rename(Lst.Open = USD.JPY, Fct.Open = USD.JPY.1)
    if(price == 'Hi') fxC %<>% dplyr::rename(Lst.High = USD.JPY, Fct.High = USD.JPY.1)
    if(price == 'Lo') fxC %<>% dplyr::rename(Lst.Low = USD.JPY, Fct.Low = USD.JPY.1)
    if(price == 'Cl') fxC %<>% dplyr::rename(Lst.Close = USD.JPY, Fct.Close = USD.JPY.1)
  }
  if(nm == 'USDJPY') {
    if(price == 'Op') fxC %<>% dplyr::rename(Lst.Open = USDJPY, Fct.Open = USDJPY.1)
    if(price == 'Hi') fxC %<>% dplyr::rename(Lst.High = USDJPY, Fct.High = USDJPY.1)
    if(price == 'Lo') fxC %<>% dplyr::rename(Lst.Low = USDJPY, Fct.Low = USDJPY.1)
    if(price == 'Cl') fxC %<>% dplyr::rename(Lst.Close = USDJPY, Fct.Close = USDJPY.1)
  }
  
  return(fxC)
  }

forecastUSDJPYHL <- function(mbase, .preCat = 'Op', .setPrice = 'Cl', 
                             currency = 'JPY=X', ahead = 1){
  fx1 <- forecastUSDJPY(mbase, currency = currency, ahead = ahead, price = .preCat)
  fx2 <- forecastUSDJPY(mbase, currency = currency, ahead = ahead, price = .setPrice)
  fxm <- merge(fx1, fx2, by = c('LatestDate.GMT', 'ForecastDate.GMT'))
  rm(fx1, fx2)
  
  fxm <- fxm[c(1, 3, 5, 2, 4, 6)]
  return(fxm)
  }

simKelly <- function(mbase) {
  
  
  return(res)
  }

kellyBet <- function(mbase, initialFundSize = 10000){
  
  ## https://quantstrattrader.wordpress.com/2017/09/29/the-kelly-criterion-does-it-work/
  mbase %<>% data.frame
  names(mbase) <- str_replace_all(names(mbase), 'JPY.X', 'USDJPY')
  ## https://github.com/englianhu/binary.com-interview-question/blob/master/function/simStakesGarch.R
  ## 
  # pred.data %>% mutate(ProbB = pnorm(Fct.High, mean = mean(Fct.Low), 
  #                                    sd = sd(Fct.Low)), 
  #                      ProbS = 1 - ProbB, 
  #                      Fct.High = round(Fct.High, 3), 
  #                      Fct.Low = round(Fct.Low, 3)) %>% data.table
  #
  #   LatestDate.GMT Lst.High Lst.Low ForecastDate.GMT Fct.High Fct.Low     ProbB      ProbS
  #1:     2017-06-27  112.399 111.863              T+1  112.547 111.766 0.8280676 0.17193237
  #2:     2017-06-28  112.920 112.154              T+1  113.050 112.085 0.8845984 0.11540162
  #3:     2017-06-29  112.389 111.749              T+1  112.581 111.730 0.8323185 0.16768149
  #4:     2017-07-02  113.417 112.204              T+1  113.424 112.543 0.9170950 0.08290501
  #5:     2017-07-03  113.370 112.779              T+1  113.651 112.893 0.9331140 0.06688596
  #---                                                                                       
  #257:     2018-06-25  110.016 109.389              T+1  110.031 109.579 0.3766011 0.62339887
  #258:     2018-06-26  110.479 109.688              T+1  110.572 109.391 0.4827210 0.51727904
  #259:     2018-06-27  110.411 109.974              T+1  110.396 109.997 0.4478401 0.55215994
  #260:     2018-06-28  110.871 110.388              T+1  110.968 109.856 0.5616675 0.43833251
  #261:     2018-07-01  111.053 110.606              T+1  111.084 110.541 0.5845864 0.41541355
  # 
  # 
  # pred.data %>% mutate(ProbB = pnorm(Fct.Low, mean = mean(Fct.High), 
  #                                    sd = sd(Fct.High)), 
  #                      ProbS = 1 - ProbB, 
  #                      Fct.High = round(Fct.High, 3), 
  #                      Fct.Low = round(Fct.Low, 3)) %>% data.table
  #
  #   LatestDate.GMT Lst.High Lst.Low ForecastDate.GMT Fct.High Fct.Low     ProbB     ProbS
  #1:     2017-06-27  112.399 111.863              T+1  112.547 111.766 0.4753219 0.5246781
  #2:     2017-06-28  112.920 112.154              T+1  113.050 112.085 0.4753219 0.5246781
  #3:     2017-06-29  112.389 111.749              T+1  112.581 111.730 0.4753219 0.5246781
  #4:     2017-07-02  113.417 112.204              T+1  113.424 112.543 0.4753219 0.5246781
  #5:     2017-07-03  113.370 112.779              T+1  113.651 112.893 0.4753219 0.5246781
  #---                                                                                      
  #257:     2018-06-25  110.016 109.389              T+1  110.031 109.579 0.4753219 0.5246781
  #258:     2018-06-26  110.479 109.688              T+1  110.572 109.391 0.4753219 0.5246781
  #259:     2018-06-27  110.411 109.974              T+1  110.396 109.997 0.4753219 0.5246781
  #260:     2018-06-28  110.871 110.388              T+1  110.968 109.856 0.4753219 0.5246781
  #261:     2018-07-01  111.053 110.606              T+1  111.084 110.541 0.4753219 0.5246781
  # 
  # 
  # pred.data %>% mutate(
  #   ProbB = pnorm(Fct.Low, mean = mean(Lst.High), sd = sd(Lst.High)), 
  #   ProbS = 1 - ProbB, Fct.High = round(Fct.High, 3), 
  #   Fct.Low = round(Fct.Low, 3)) %>% data.table
  #
  #   LatestDate.GMT Lst.High Lst.Low ForecastDate.GMT Fct.High Fct.Low     ProbB     ProbS
  #1:     2017-06-27  112.399 111.863              T+1  112.547 111.766 0.6829508 0.3170492
  #2:     2017-06-28  112.920 112.154              T+1  113.050 112.085 0.7296487 0.2703513
  #3:     2017-06-29  112.389 111.749              T+1  112.581 111.730 0.6774416 0.3225584
  #4:     2017-07-02  113.417 112.204              T+1  113.424 112.543 0.7901496 0.2098504
  #5:     2017-07-03  113.370 112.779              T+1  113.651 112.893 0.8303839 0.1696161
  #---                                                                                      
  #257:     2018-06-25  110.016 109.389              T+1  110.031 109.579 0.3242934 0.6757066
  #258:     2018-06-26  110.479 109.688              T+1  110.572 109.391 0.2961039 0.7038961
  #259:     2018-06-27  110.411 109.974              T+1  110.396 109.997 0.3906327 0.6093673
  #260:     2018-06-28  110.871 110.388              T+1  110.968 109.856 0.3677742 0.6322258
  #261:     2018-07-01  111.053 110.606              T+1  111.084 110.541 0.4816531 0.5183469
  #
  # pred.data %>% mutate(
  #   ProbB = pnorm(Fct.Low, mean = mean(Fct.Low), sd = sd(Fct.Low)), 
  #   ProbS = 1 - ProbB, Fct.High = round(Fct.High, 3), 
  #   Fct.Low = round(Fct.Low, 3)) %>% data.table
  #
  #   LatestDate.GMT Lst.High Lst.Low ForecastDate.GMT Fct.High Fct.Low     ProbB     ProbS
  #1:     2017-06-27  112.399 111.863              T+1  112.547 111.766 0.7106333 0.2893667
  #2:     2017-06-28  112.920 112.154              T+1  113.050 112.085 0.7626865 0.2373135
  #3:     2017-06-29  112.389 111.749              T+1  112.581 111.730 0.7044035 0.2955965
  #4:     2017-07-02  113.417 112.204              T+1  113.424 112.543 0.8275657 0.1724343
  #5:     2017-07-03  113.370 112.779              T+1  113.651 112.893 0.8685511 0.1314489
  #---                                                                                      
  #257:     2018-06-25  110.016 109.389              T+1  110.031 109.579 0.2943248 0.7056752
  #258:     2018-06-26  110.479 109.688              T+1  110.572 109.391 0.2627786 0.7372214
  #259:     2018-06-27  110.411 109.974              T+1  110.396 109.997 0.3701970 0.6298030
  #260:     2018-06-28  110.871 110.388              T+1  110.968 109.856 0.3438371 0.6561629
  #261:     2018-07-01  111.053 110.606              T+1  111.084 110.541 0.4765542 0.5234458
  
  mbase %<>% mutate(
    ProbB = pnorm(Fct.Low, mean = mean(Fct.High), sd = sd(Fct.High)), 
    ProbS = 1 - ProbB, Fct.High = round(Fct.High, 3), 
    Fct.Low = round(Fct.Low, 3)) %>% data.table
  
  # Kelly criterion
  # Advantages = (prob of win * decimal odds) + (prob of lose * -1)
  # Optimal Kelly wager % = Advantages / decimal odds
  mbase$Adv <- (mbase$EMprob * mbase$COMOdds) + ((1-mbase$EMprob) * -1)
  mbase$Staking <- mbase$Adv / mbase$COMOdds
  mbase$Staking <- ifelse(mbase$Staking < 0, 0, mbase$Staking)
  
  return(res)
  }

# ---------------- KellyS ------------------------------------------
## http://srdas.github.io/MLBook/Gambling.html
KellyS <- function(fitm, .preCat = 'Lo', .forCat = 'Hi', .initialFundSize = 10000, 
                   .filterBets = FALSE, .fundLeverageLog = FALSE) {
  
  fitm %<>% na.omit
  
  if(.preCat == 'Op') fitm %<>% rename(Point.Forecast = Fct.Open)
  if(.preCat == 'Hi') fitm %<>% rename(Point.Forecast = Fct.High)
  if(.preCat == 'Lo') fitm %<>% rename(Point.Forecast = Fct.Low)
  if(.preCat == 'Cl') fitm %<>% rename(Point.Forecast = Fct.Close)
  
  if(.forCat == 'Op') fitm %<>% rename(forClose = Fct.Open)
  if(.forCat == 'Hi') fitm %<>% rename(forClose = Fct.High)
  if(.forCat == 'Lo') fitm %<>% rename(forClose = Fct.Low)
  if(.forCat == 'Cl') fitm %<>% rename(forClose = Fct.Close)
  
  fitm %<>% mutate(
    ProbB = pnorm(Point.Forecast, mean = mean(forClose), sd = sd(forClose)), 
    ProbS = 1 - ProbB)#, Fct.High = round(Fct.High, 3), 
  #Fct.Low = round(Fct.Low, 3))
  
  fitm %<>% mutate(Point.Forecast = round(lag(Point.Forecast), 3), 
                   forClose = round(lag(forClose), 3)) %>% na.omit %>% data.table
  
  fitm %<>% mutate(BR = .initialFundSize) %>% 
    #'@ mutate(Return.Back = ifelse(Prob > 0.5, Diff * Back * stakes, 0), 
    #'@        Return.Lay = ifelse(Prob < 0.5, -Diff * Lay * stakes, 0))
    mutate(fB = 2 * ProbB - 1, fS = 2 * ProbS - 1, 
           #EUB = ProbB * log(BR * (1 + fB)) + (1 - ProbB) * log(BR * (1 - fB)), 
           #EUS = ProbS * log(BR * (1 + fS)) + (1 - ProbS) * log(BR * (1 - fS)), 
           EUB = ProbB * log(ProbB) + (1 - ProbB) * log(1 - ProbB), 
           EUS = ProbS * log(ProbS) + (1 - ProbS) * log(1 - ProbS), 
           #EUB = ProbB * (BR * (1 + fB)) + (1 - ProbB) * (BR * (1 - fB)), 
           #EUS = ProbS * (BR * (1 + fS)) + (1 - ProbS) * (BR * (1 - fS)), 
           #'@ Edge = ifelse(f > 0, EUB, EUS), #For f > 0 need to buy and f <= 0 need to sell.
           #need to study on the risk management on "predicted profit" and "real profit".
           Edge = ifelse(fB > 0, EUB, ifelse(fS > 0, EUS, 0)), 
           PF = ifelse(Point.Forecast >= Lst.Low & 
                         Point.Forecast <= Lst.High, 
                       Point.Forecast, 0), #if forecasted place-bet price doesn't existing within Hi-Lo price, then the buying action is not stand. Assume there has no web bandwith delay.
           FC = ifelse(forClose >= Lst.Low & forClose <= Lst.High, 
                       forClose, Lst.Close), #if forecasted settle price doesn't existing within Hi-Lo price, then the closing action at closing price. Assume there has no web bandwith delay.
           #'@ Diff = round(forClose - USDJPY.Close, 2),
           ##forecasted closed price minus real close price.
           
           Buy = ifelse(PF > 0 & FC > PF, 1, 0), ##buy action
           Sell = ifelse(PF > 0 & FC < PF, 1, 0), ##sell action
           BuyS = Edge * Buy * (forClose - PF), 
           SellS = Edge * Sell * (PF - forClose), 
           Profit = BuyS + SellS, Bal = BR + Profit)
  
  
  #'@ fitm %>% dplyr::select(Point.Forecast, forClose, Prob, BR, f, EU, Edge, PF, FC, Buy, Sell, SP, Bal)
  #'@ fitm %>% dplyr::select(ProbB, ProbS, BR, fB, fS, EUB, EUS, Edge, PF, USDJPY.Open, FC, Buy, Sell, BuyS, SellS, Profit, Bal) %>% filter(PF > 0, FC > 0)
  
  ## The ets staking models (Kelly criterion) Adjusted Banl-roll and Balance column.
  for(i in seq(2, nrow(fitm))) {
    fitm$BR[i] = fitm$Bal[i - 1]
    fitm$fB[i] = 2 * fitm$ProbB[i] - 1
    fitm$fS[i] = 2 * fitm$ProbS[i] - 1
    fitm$EUB[i] = fitm$ProbB[i] * log(fitm$BR[i] * (1 + fitm$fB[i])) + 
      (1 - fitm$ProbB[i]) * log(fitm$BR[i] * (1 - fitm$fB[i]))
    fitm$EUS[i] = fitm$ProbS[i] * log(fitm$BR[i] * (1 + fitm$fS[i])) + 
      (1 - fitm$ProbS[i]) * log(fitm$BR[i] * (1 - fitm$fS[i]))
    fitm$Edge[i] = ifelse(fitm$fB[i] > 0, fitm$EUB[i], 
                          ifelse(fitm$fS[i] > 0, fitm$EUS[i], 0)) #For f > 0 need to buy and f <= 0 need to sell.
    #need to study on the risk management on "predicted profit" and "real profit".
    
    fitm$BuyS[i] = fitm$Edge[i] * fitm$Buy[i] * (fitm$forClose[i] - fitm$PF[i])
    fitm$SellS[i] = fitm$Edge[i] * fitm$Sell[i] * (fitm$PF[i] - fitm$forClose[i])
    fitm$Profit[i] = fitm$BuyS[i] + fitm$SellS[i]
    fitm$Bal[i] = fitm$BR[i] + fitm$Profit[i]
    if(fitm$Bal[i] <= 0) stop('All invested fund ruined!')
  }; rm(i)
  
  #names(mbase) <- str_replace_all(names(mbase), '^(.*?)+\\.', nm)
  
  if(.filterBets == TRUE) {
    fitm %<>% filter(PF > 0, FC > 0)
  }
  
  fitm %<>% mutate(RR = Bal/BR)
  
  ## convert the log leverage value of fund size and profit into normal digital figure with exp().
  if(.fundLeverageLog == TRUE) fitm %<>% 
    mutate(BR = exp(BR), BuyS = exp(BuyS), SellS = exp(SellS), 
           Profit = exp(Profit), Bal = exp(Profit))
  
  return(fitm)
  }





## ================== Reference ========================================
## https://shiny.rstudio.com/articles/persistent-data-storage.html
## https://github.com/bnosac/cronR
## http://www.bnosac.be/index.php/blog/64-scheduling-r-scripts-and-processes-on-windows-and-unix-linux

